
server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  
  values <- reactiveValues(
    ixns         = list(),
    rpts         = rpts_0,
    msg_prj      = NULL,
    ply          = NULL)
  # cat(capture.output(dput(values$ixns)))
  
  # login ----
  glogin <- shiny::callModule(googleSignIn, "login")

  output$user <- renderUser({

    dashboardUser(
      name     = glogin()$name,
      image    = glogin()$image,
      subtitle = glogin()$email,
      footer   = googleSignInUI_btn_signout("login"))
  })
  
  # map ----
  output$map_side <- renderLeaflet({
    message("output$map_side - beg")
    
    m <- leaflet(
      options = leafletOptions(
        zoomControl        = F,
        attributionControl = F)) |> 
      # add base: blue bathymetry and light brown/green topography
      addProviderTiles(
        "Esri.OceanBasemap",
        options = providerTileOptions(
          variant = "Ocean/World_Ocean_Base")) |>
      # add reference: placename labels and borders
      addProviderTiles(
        "Esri.OceanBasemap",
        options = providerTileOptions(
          variant = "Ocean/World_Ocean_Reference")) |>
      setView(-93.4, 37.4, 2)
    
    message("output$map_side - end")
    m
  })
  
  # *mapeditor ----
  output$mapeditor <- renderLeaflet({
    library(leaflet.extras)
    
    message("output$mapeditor - beg")
    
    # m <- map_edit
    m <- leaflet(
      options = leafletOptions(
        zoomControl = T,
        attributionControl = F)) |> 
      # add base: blue bathymetry and light brown/green topography
      addProviderTiles(
        "Esri.OceanBasemap",
        options = providerTileOptions(
          variant = "Ocean/World_Ocean_Base")) |>
      # add reference: placename labels and borders
      addProviderTiles(
        "Esri.OceanBasemap",
        options = providerTileOptions(
          variant = "Ocean/World_Ocean_Reference")) |>
      # addPolygons(data = ply_editable_0, group = "ply_editable") %>% 
      setView(-93.4, 37.4, 4)
    
    m <- m %>% 
      leaflet.extras::addDrawToolbar(
        targetGroup = "ply_editable",
        editOptions = leaflet.extras::editToolbarOptions(
          # edit = F,
          # remove = T,
          selectedPathOptions = selectedPathOptions()),
        circleOptions = F,
        circleMarkerOptions = F,
        markerOptions = F,
        polylineOptions = F,
        singleFeature = T) 
    
    ply <- values$ply
    if (!is.null(ply)){
      bb <- sf::st_bbox(ply)
      
      m <- m %>% 
        addPolygons(data = ply, group = "ply_editable") # %>%
      # flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
    }
    
    message("output$mapeditor - end")
    m
  })
 
  # *mapeditor_draw_all_features ----
  observe({
    #use the draw_stop event to detect when users finished drawing
    # https://github.com/bhaskarvk/leaflet.extras/blob/master/inst/examples/shiny/draw-events/app.R
    #req(input$mapeditor_draw_stop)
    req(input$mapeditor_draw_all_features)
    
    #message("observe output$mapeditor_draw_all_features - beg")
    
    feature <- isolate(input$mapeditor_draw_all_features$features[[1]])
    
    ply_json <- geojsonio::as.json(feature$geometry)
    # spdf <- geojsonio::geojson_sp(feature)
    ply <- st_read(ply_json, quiet = T)
    #ply_wkt <- st_as_text(st_geometry(ply))
    values$ply <- ply

    leafletProxy("map_side") %>%
      clearShapes()
    
    if (is.null(ply)){
      actionButton(
        "btn_mod_map", "Add", icon=icon("plus"))
    } else {
      bb <- sf::st_bbox(ply)
      
      leafletProxy("map_side") %>%
        addPolygons(data = ply, group = "ply_editable") %>% 
        flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
      
      updateActionButton(
        session,
        "btn_mod_map", "Modify", icon=icon("cog"))
    }
    
    #message("observe output$mapeditor_draw_all_features - end")
  })
  
  # *observe btn_mod_map
  observeEvent(input$btn_mod_map, {
    
    #message("observe btn_mod_map - beg")
    
    showModal(modalDialog(
      title     = "Modify Location",
      #editModUI("map_editor"),
      leafletOutput("mapeditor"),
      #leafletOutput("map_editable"),
      footer    = modalButton("Close"),
      easyClose = T))
    
    #message("observe btn_mod_map - end")
  })
  
  
  # ixns ----
  
  #* ixn_btns ----
  output$ixn_btns <- renderUI({
    
    #message("ixn_btns - beg")
    
    if (length(values$ixns) == 0){
      tags <- actionButton(
        "btn_add_ixn", "Add", icon=icon("plus"), width="263px")
    } else {
      tags <- tagList(
        actionButton(
          "btn_add_ixn" , "Add"         , icon=icon("plus"), width="120px", style="display:inline-block;"),
        actionButton(
          "btn_mod_ixns", "Modify (n=0)", icon=icon("cog"), width="120px", style="display:inline-block;margin-right:15px;float:right"))
    }
    
    #message("ixn_btns - end")
    tags
  })
  
  #* btn_add_ixn ----
  observeEvent(input$btn_add_ixn, {
  
    req(input$sel_ixn_tags)
    
    #message("btn_add_ixn - beg")
    
    values$ixns <- append(values$ixns, list(input$sel_ixn_tags))
    
    updateSelectizeInput(
      session, "sel_ixn_tags", selected = "")
    
    #message("btn_add_ixn - end")
  })
  
  #* btn_mod_ixns ----
  observeEvent(input$btn_mod_ixns, {
    
    #message("btn_mod_ixns - beg")
    
    showModal(modalDialog(
      title = "Modify Interactions",
      tagList(
        DTOutput("tbl_ixns"),
        actionButton("btn_del_ixns", "Delete selected interaction(s)", icon=icon("minus"))),
      footer    = modalButton("Close"),
      easyClose = T))
    
    #message("btn_mod_ixns - end")
  })
  
  #* tbl_ixns ----
  output$tbl_ixns <- renderDT({
    
    req(values$ixns)
    
    #message("tbl_ixns - beg")
    
    d <- ixns_to_colorhtml_df(values$ixns, df_tags)
    
    #message("tbl_ixns - end")
    
    d
  }, escape = F)
  
  
  #* btn_del_ixns ----
  observeEvent(input$btn_del_ixns, {
    req(values$ixns, input$tbl_ixns_rows_selected)
    
    #message("btn_del_ixns - beg")
    
    values$ixns <- values$ixns[-input$tbl_ixns_rows_selected]
  
    #message("btn_del_ixns - end")
  })
  
  #* btn_mod_inxns.n ----
  observe({
    #message("btn_mod_inxns.n - beg")
    
    n_ixns <- length(values$ixns)
    
    updateActionButton(
      session, 
      "btn_mod_ixns", 
      label = glue("Modify (n={ n_ixns })"))
    
    #message("btn_mod_inxns.n - end")
  })
  
  
  # projects ----

  #* get_projects ----
  get_projects <- reactive({
    
    if (debug)
      message("get_projects - beg")
    
    prj <- get_projects_tbl(ixns = values$ixns)
    
    if (debug)
      message("get_projects - end")
    
    prj
  })
  
  #* prj_map ----
  output$prj_map <- renderLeaflet({
    
    if (debug)
      message("prj_map - beg")
    
    m <- map_projects(get_projects())
    
    if (debug)
      message("prj_map - end")
    
    m
  })
  
  #* ba_map ----
  output$ba_map <- renderLeaflet({
    
    # #message("ba_map - beg")
    
    ba_sites <- tbl(con, "ba_sites") %>% 
      collect() %>% 
      mutate(
        label_html = glue(
          "<b>{ba_project}</b><br>
          at {site_name}") %>% lapply(htmltools::HTML))
    
    m <- leaflet::leaflet(
      data    = ba_sites, width = "100%",
      options = leaflet::leafletOptions(
        zoomControl = F)) |> 
      # add base: blue bathymetry and light brown/green topography
      leaflet::addProviderTiles(
        "Esri.OceanBasemap",
        options = providerTileOptions(
          variant = "Ocean/World_Ocean_Base")) |>
      # add reference: placename labels and borders
      leaflet::addProviderTiles(
        "Esri.OceanBasemap",
        options = providerTileOptions(
          variant = "Ocean/World_Ocean_Reference")) |>
      leaflet::addMarkers(
        lat   = ~lat,
        lng   = ~lon,
        label = ~label_html,
        popup = ~label_html,
        clusterOptions = 
          markerClusterOptions()) |> 
      htmlwidgets::onRender("function(el, x) {
          L.control.zoom({ position: 'topright' }).addTo(this) }")
    
    # #message("ba_map - end")
    
    m
  })
  
  # OLD: now handled by attr(get_projects(), "message")
  # msg: when no projects associated w/ selected tech
  # observe({
  #   req(values$ixns)
  # 
  #   tech_sel     <- unlist(values$ixns) %>% str_subset("^Technology.")
  #   tech_missing <- setdiff(tech_sel, projects_tech_avail)
  #   
  #   if (length(tech_missing) > 0) {
  #     values$msg_prj <- glue(
  #       "Sorry, Technology {ixn_to_colorhtml(tech_sel, df_tags, is_html=T)} does not match Technology 
  #       associated with available projects: {ixn_to_colorhtml(projects_tech_avail, df_tags, is_html=T)}.")
  #   } else {
  #     values$msg_prj <- NULL
  #   }
  #   
  # })
  
  #* msg_prj ----
  output$msg_prj <- renderUI({
    
    # #message("msg_prj - beg")
    #browser()
    
    msg <- attr(get_projects(), "message")
    if (is.null(msg)){
      m <- NULL
    } else {
      m <- div(
        class="alert alert-warning", role="alert",
        HTML(msg))
    }
    
    # #message("msg_prj - end")
    m
  })
  outputOptions(output, "msg_prj", suspendWhenHidden = FALSE)
  
  output$n_prj <- renderText({
    
    # #message("n_prj - beg")
    
    n <- nrow(get_projects())
    
    # #message("n_prj - end")
    n
  })
  outputOptions(output, "n_prj", suspendWhenHidden = FALSE)

  
  #* prj_map observe tags  ----
  observe({
    
    # #message("prj_map observe tags - beg")
    
    d_prjs <- get_projects()

    # map: redraw markers
    #browser()
    leaflet::leafletProxy("prj_map") %>%
      leaflet::clearMarkers()

    if (nrow(d_prjs) > 0)
      leaflet::leafletProxy("prj_map") %>%
        leaflet::addMarkers(
          data  = d_prjs,
          lat   = ~latitude,
          lng   = ~longitude,
          label = ~label_html %>% lapply(htmltools::HTML),
          popup = ~popup_html)

    #browser()
    # #message("prj_map observe tags - end")
  })
  
  #* prj_timeline ----
  #calculate_y_tech(tech)
  output$prj_timeline <- renderPlotly({
    
    ##message("prj_timeline - beg")
    
    suppressWarnings({
      plot_project_timelines(get_projects()) %>% 
        event_register("plotly_click")
    })
    
    ##message("prj_timeline - end")
    # p
  })
  
  #* prj_p plotly_click ----
  observe({
    
    # event_register("prj_p", "plotly_click")
    d <- event_data("plotly_click")
    req(d)
    
    # #message("prj_p plotly_click - beg")

    s <- get_projects() %>%
      filter(project == d$y)

    leafletProxy("prj_map") %>%
      flyTo(s$longitude, s$latitude, 8)

    # #message("prj_p plotly_click - end")
  })
  
  # management ----

  #* get_mgt() ----
  get_mgt <- reactive({
    
    #message("get_mgt() - beg")
    
    d <- get_mgt_tbl(ixns = values$ixns)
    
    #message("get_mgt() - end")
    
    d
  })
  
  #* msg_mgt ----
  output$msg_mgt <- renderUI({
    
    #message("msg_mgt - beg")
    
    msg <- attr(get_mgt(), "message")
    if (is.null(msg)){
      m <- NULL
    } else {
      m <- div(
        class="alert alert-warning", role="alert",
        HTML(msg))
    }
    
    #message("msg_mgt - end")
    m
  })
  outputOptions(output, "msg_mgt", suspendWhenHidden = FALSE)
  
  #* box_mgt ----
  output$box_mgt <- renderText({
    
    #message("box_mgt - beg")
    
    n_ixns <- length(values$ixns)
    m <- ifelse(
      n_ixns == 0,
      HTML(glue("Management Measures <small>({d_mgt_n} rows)</small>")),
      HTML(glue("Management Measures <small>({nrow(get_mgt())} of {d_mgt_n} rows; filtered by {n_ixns} interactions)</small>")))
    
    #message("box_mgt - end")
    m
  })
  
  #* tbl_mgt ----
  output$tbl_mgt <- renderDataTable({
    
    #message("tbl_mgt - beg")
    
    d <- get_mgt()
    
    #message("tbl_mgt - end")
    
    d
  }, escape = F, rownames = F)
  
  # documents ----

  #* get_docs() ----
  get_docs <- reactive({
    
    #message("get_docs() - beg")
    
    d <- get_docs_tbl(ixns = values$ixns, cks = input$cks_docs)
    
    #message("get_docs() - end")
    
    d
  })
  
  #* msg_docs ----
  #* get_ba() ----
  get_ba <- reactive({
    
    if (debug)
      message("get_ba() - beg")
    
    d <- get_ba_tbl(ixns = values$ixns, cks = input$cks_docs)
    #d <- get_docs_tbl(ixns = values$ixns, cks = input$cks_docs)
    
    if (debug)
      message("get_ba() - end")
    
    d
  })
  
  #* msg_docs ----
  output$msg_docs <- renderUI({
    
    #message("msg_docs - beg")
    
    msg <- attr(get_docs(), "message")
    if (is.null(msg)){
      m <- NULL
    } else {
      m <- div(
        class="alert alert-warning", role="alert",
        HTML(msg))
    }
    
    #message("msg_docs - end")
    m
  })
  outputOptions(output, "msg_docs", suspendWhenHidden = FALSE)
  
  
  #* box_docs ----
  output$box_docs <- renderText({
    
    #message("box_docs - beg")
    
    n_ixns <- length(values$ixns)
    n_cks  <- length(input$cks_docs)
    n_docs <- nrow(get_docs())
    
    m <- ifelse(
      n_ixns == 0 & n_cks == 0,
      HTML(glue("FERC Excerpts <small>({d_docs_n} rows)</small>")),
      HTML(glue("FERC Excerpts <small>({n_docs} of {d_docs_n} rows; filtered by {n_ixns} interactions & {n_cks} checkboxes)</small>")))
  
    #message("box_docs - end")
    
    m
  })
  
  #* box_ba ----
  output$box_ba <- renderText({
    
    #message("box_ba - beg")
    
    n_ixns <- length(values$ixns)
    n_cks  <- length(input$cks_docs)
    n_docs <- nrow(get_docs())
    
    m <- ifelse(
      n_ixns == 0 & n_cks == 0,
      HTML(glue("BioAssessment Excerpts <small>({d_docs_n} rows)</small>")),
      HTML(glue("BioAssessment Excerpts <small>({n_docs} of {d_docs_n} rows; filtered by {n_ixns} interactions & {n_cks} checkboxes)</small>")))
  
    #message("box_ba - end")
    
    m
  })
  
  #* tbl_docs ----
  output$tbl_docs <- renderDataTable({
    
    #message("tbl_docs - beg")
    
    d <- get_docs()
    
    #message("tbl_docs - end")
    
    d
  }, escape = F, rownames = F)
  
  #* tbl_ba ----
  output$tbl_ba <- renderDataTable({
    
    if (debug)
      message("tbl_ba - beg")
    
    d <- get_ba()
    
    if (debug)
      message("tbl_ba - end")
    
    d
  }, escape = F, rownames = F)
  
  # publications ----
  #* get_pubs() ----
  get_pubs <- reactive({
    
    #message("get_pubs() - beg")
    
    d <- get_pubs_tbl(ixns = values$ixns)
    
    #message("get_pubs() - beg")
    
    d 
  })

  #* msg_pubs ----
  output$msg_pubs <- renderUI({
    
    #message("msg_pubs - beg")
    
    msg <- attr(get_pubs(), "message")
    if (is.null(msg)){
      m <- NULL
    } else {
      m <- div(
        class="alert alert-warning", role="alert",
        HTML(msg))
    }
    
    #message("msg_pubs - end")
    
    m
  })
  outputOptions(output, "msg_pubs", suspendWhenHidden = FALSE)

  #* box_pubs ----
  output$box_pubs <- renderText({
    
    #message("box_pubs - beg")
    
    n_ixns <- length(values$ixns)
    n_pubs <- nrow(get_pubs())
    
    m <- ifelse(
      n_ixns == 0, 
      HTML(glue("Tethys Publications <small>({d_pubs_n} rows)</small>")),
      HTML(glue("Tethys Publications <small>({n_pubs} of {d_pubs_n} rows; filtered by {n_ixns} interactions)</small>")))
  
    #message("box_pubs - end")
    
    m
  })
  
  #* tbl_pubs ----
  output$tbl_pubs <- renderDataTable({
    
    #message("tbl_pubs - beg")
    
   d <- get_pubs() %>% 
      #select(-uri, -title, -tag)
      select(ID, Title, Tags)
    
    #message("tbl_pubs - end")
    
    d
  }, escape = F, rownames = F)
  
  
  # spatial ----
  
  #* get_spatial() ----
  get_spatial <- reactive({
    
    #message("get_spatial() - beg")
    
    # ST_Force_2D
    #DBI::dbGetQuery(con, "SELECT ST_Force2D();")
    d <- get_spatial_tbl(
      ixns    = values$ixns, 
      aoi_wkt = sf_to_wkt(values$ply))
    
    #message("get_spatial() - end")
    d
  })
    
  #* msg_spatial ----
  output$msg_spatial <- renderUI({
    
    #message("msg_spatial - beg")
    
    msg <- attr(get_spatial(), "message")
    if (is.null(msg)){
      m <- NULL
    } else {
      m <- div(
        class="alert alert-warning", role="alert",
        HTML(msg))
    }
    
    #message("msg_spatial - end")
    m
  })
  outputOptions(output, "msg_spatial", suspendWhenHidden = FALSE)
  
  #* box_spatial ----
  output$box_spatial <- renderText({
    
    #message("box_spatial - beg")
    
    n_ixns    <- length(values$ixns)
    n_spatial <- nrow(get_spatial())

    m <- ifelse(
      n_ixns == 0,
      HTML(glue("MarineCadastre Spatial datasets <small>({d_spatial_n} rows)</small>")),
      HTML(glue("MarineCadastre Spatial datasets <small>({n_spatial} of {d_spatial_n} rows; filtered by {n_ixns} interactions)</small>")))
    
    #message("box_spatial - end")
    
    m
  })
  

  #* tbl_spatial ----
  output$tbl_spatial <- renderDataTable({
    
    #message("tbl_spatial - beg")
    
    d <- get_spatial()
    
    if ("sp_data" %in% names(d)){c
      d <- d %>% 
        mutate(
          `Rows in Results` = map_int(sp_data, nrow)) %>% 
        select(ID, Title, Tags, `Rows in Results`) %>% 
        filter(`Rows in Results` > 0) # , sp_data)
    } else {
      d <- d %>% 
        select(ID, Title, Tags)
    }
    
    #message("tbl_spatial - end")
    d
    # TODO: 'expand data' buttons for each row which, when clicked result in the corresponding sp_data being displayed as a df
  }, escape = F, rownames = F)

  
  # reports ----
  
  #* get_email() ----
  get_email <- reactive({
    
    #message("get_email() - beg")
    
    email <- input$`login-g_email`
    if (is.null(email)){
      ##message("get_email() is.null")
      values$rpts <- rpts_0
      email <- NULL
    } else {
      ##message(glue("get_email(): {email}"))
      values$rpts <- get_user_reports(email)
    }
    
    #message("get_email() - end")
    
    email
  })
  observe(get_email())
  
  #* txt_rpt_login ----
  output$txt_rpt_login <- renderText({
    
    #message("txt_rpt_login - beg")
    
    m <- ifelse(
      is.null(get_email()),
      HTML("
        <span class = 'help-block'>
        In order to create a report, you'll need to <b>Sign in</b> (upper right) using any Google login.
        <br>
        If you need to create a Google Account or associate your existing email for logging into Google, 
        please see <a 
           href='https://support.google.com/accounts/answer/27441?hl=en' target='_blank'>
           Create a Google Account - Google Account Help
           </a>.
        </span>"),
      HTML("
        <span class = 'help-block'>
        Here you can generate a report that will use the Location, Interactions
        from Configure in the sidebar along with the checked content.
        </span>"))
    
    #message("txt_rpt_login - end")
    
    m
  })
  
  #* get_rpts_tbl() ----
  get_rpts_tbl <- reactivePoll(
    10000,
    session, # check every 10 seconds
    checkFunc = function() {
      email <- get_email()
      if (is.null(email))
        return("")
      lastmod <- get_user_reports_last_modified(email)
      # message(glue("poll_rpts_tbl({email}) {Sys.time()} -- lastmod: {lastmod}"))
      lastmod
      },
    valueFunc = function() {
      email <- get_email()
      if (is.null(email))
        return(rpts_0)
      # message(glue("poll_rpts_tbl({email}) set value {Sys.time()}"))
      values$rpts <- get_user_reports(email)
      values$rpts
    })

  #* tbl_rpts ----
  output$tbl_rpts = renderDT({
    
    message("tbl_rpts - beg")
    
    d <- get_rpts_tbl() %>% 
      # get_user_reports("ben@ecoquants.com") %>% 
      # arrange(desc(date)) %>% 
      mutate(
        filetype  = fs::path_ext(url),
        file_icon = file_icons[filetype],
        title = ifelse(
          status == "published",
          glue("<a href='{url}' target='_blank'><i class='fas fa-{file_icon}'></i> <b>{title}</b></a>"),
          glue("<i class='fas fa-{file_icon}'></i> <b>{title}</b>")),
        status = ifelse(
          status == "published",
          "<span class='badge btn-default'>Published</span>",
          "<span class='badge btn-default' disabled='disabled'>Rendering...</span>")) %>% 
      select(
        Title = title, Date = date, Status = status, 
        Contents = contents, `# Interactions` = n_ixns)
    
    
    message("tbl_rpts - end")
    
    d
  }, escape = F)
  
  #* btn_rpt_create() ----
  observeEvent(input$btn_rpt_create, {
    
    message("btn_rpt_create() - beg")
  
    # req(input$`login-g_email`)
    rpt_title <- isolate(input$txt_rpt_title)
    out_ext   <- isolate(input$sel_rpt_ext)
    email     <- isolate(glogin()$email) 
    
    if (rpt_title == "") 
      return()
    # TODO: message if missing title
    
    toJSONlist <- function(x){
      ifelse(
        is.null(x), 
        toJSON(list()),
        toJSON(x)) }
    
    q <- list(
      email        = email,
      date         = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      title        = rpt_title,
      filetype     = out_ext,
      contents     = toJSONlist(list(
          projects     = isolate(input$ck_rpt_prj),
          management   = isolate(input$ck_rpt_mgt),
          documents    = isolate(input$ck_rpt_docs),
          publications = isolate(input$ck_rpt_pubs),
          spatial      = isolate(input$ck_rpt_spatial))),
      interactions    = toJSONlist(values[["ixns"]]),
      document_checks = toJSONlist(isolate(input$cks_docs)),
      spatial_aoi_wkt = sf_to_wkt(values$ply))

    r <- GET(url_rpt_pfx, query = q)
    # message(glue("r$url: {r$url}"))

    values$rpts <- get_user_reports(glogin()$email)
    
    message("btn_rpt_create() - end")
  })

  #* btn_del_rpts ----
  observeEvent(input$btn_del_rpts, {
    message("btn_del_rpts - beg")
    
    req(input$tbl_rpts_rows_selected)
    
    irows <- input$tbl_rpts_rows_selected
    email <- isolate(get_email())
    
    rpts_del <- get_rpts_tbl() %>% 
      slice(irows) %>% 
      pull(url) %>% 
      basename()

    sapply(rpts_del, del_user_report, email = email)
    
    values$rpts <- get_user_reports(email)
    
    message("btn_del_rpts - end")
  })
}
