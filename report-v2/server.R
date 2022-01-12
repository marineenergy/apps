
server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  
  values <- reactiveValues(
    ixns    = list(),
    rpts    = rpts_0,
    msg_prj = NULL,
    ply     = NULL)
  # cat(capture.output(dput(values$ixns)))
  
  # login ----
  glogin <- shiny::callModule(googleSignIn, "login")

  output$user <- renderUser({
    #browser()
    # DEBUG
    dashboardUser(
      name     = glogin()$name,
      image    = glogin()$image,
      subtitle = glogin()$email,
      footer   = googleSignInUI_btn_signout("login"))
  })
  
  # map ----
  output$map_side <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        zoomControl        = F,
        attributionControl = F)) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      setView(-93.4, 37.4, 2)
  })
  
  output$mapeditor <- renderLeaflet({
    library(leaflet.extras)
    
    # m <- map_edit
    m <- leaflet(
      options = leafletOptions(
        zoomControl = T,
        attributionControl = F)) %>% 
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
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
    
    m
  })
 
  observe({
    #use the draw_stop event to detect when users finished drawing
    # https://github.com/bhaskarvk/leaflet.extras/blob/master/inst/examples/shiny/draw-events/app.R
    #req(input$mapeditor_draw_stop)
    req(input$mapeditor_draw_all_features)
    #browser()
    message("draw_all_features")
    
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
  })
  
  observeEvent(input$btn_mod_map, {
    showModal(modalDialog(
      title     = "Modify Location",
      #editModUI("map_editor"),
      leafletOutput("mapeditor"),
      #leafletOutput("map_editable"),
      footer    = modalButton("Close"),
      easyClose = T))
    
  })
  
  
  # ixns ----
  
  #* ixn_btns ----
  output$ixn_btns <- renderUI({
    
    if (length(values$ixns) == 0)
      return(
        actionButton(
          "btn_add_ixn", "Add", icon=icon("plus"), width="263px"))
    
    tagList(
      actionButton(
        "btn_add_ixn" , "Add"         , icon=icon("plus"), width="120px", style="display:inline-block;"),
      actionButton(
        "btn_mod_ixns", "Modify (n=0)", icon=icon("cog"), width="120px", style="display:inline-block;margin-right:15px;float:right"))
  })
  
  #* btn_add_ixn ----
  observeEvent(input$btn_add_ixn, {
    req(input$sel_ixn_tags)
    
    values$ixns <- append(values$ixns, list(input$sel_ixn_tags))
    
    updateSelectizeInput(
      session, "sel_ixn_tags", selected = "")
  })
  
  #* btn_mod_ixns ----
  observeEvent(input$btn_mod_ixns, {
    showModal(modalDialog(
      title = "Modify Interactions",
      tagList(
        DTOutput("tbl_ixns"),
        actionButton("btn_del_ixns", "Delete selected interaction(s)", icon=icon("minus"))),
      footer    = modalButton("Close"),
      easyClose = T))
  })
  
  #* tbl_ixns ----
  output$tbl_ixns <- renderDT({
    req(values$ixns)
    ixns_to_colorhtml_df(values$ixns, df_tags)
  }, escape = F)
  
  observeEvent(input$btn_del_ixns, {
    req(values$ixns, input$tbl_ixns_rows_selected)
    
    values$ixns <- values$ixns[-input$tbl_ixns_rows_selected]
  })
  
  
  observe({
    n_ixns <- length(values$ixns)
    
    updateActionButton(
      session, 
      "btn_mod_ixns", 
      label = glue("Modify (n={ n_ixns })"))
  })
  
  
  # projects ----

  #* prj_map ----
  output$prj_map <- renderLeaflet({
    
    map_projects(d_projects)})
  
  #* get_projects ----
  get_projects <- reactive({
    get_projects_tbl(d_projects_tags, ixns = values$ixns)
  })
  
  # msg: when no projects associated w/ selected tech
  observe({
    req(values$ixns)

    tech_sel     <- unlist(values$ixns) %>% str_subset("^Technology.")
    tech_missing <- setdiff(tech_sel, projects_tech_avail)
    
    if (length(tech_missing) > 0) {
      values$msg_prj <- glue(
        "Sorry, Technology {ixn_to_colorhtml(tech_sel, df_tags, is_html=T)} does not match Technology 
        associated with available projects: {ixn_to_colorhtml(projects_tech_avail, df_tags, is_html=T)}.")
    } else {
      values$msg_prj <- NULL
    }
    
  })
  
  #* msg_prj ----
  output$msg_prj <- renderUI({
    if (is.null(values$msg_prj))
      return(NULL)
    div(
      class="alert alert-warning", role="alert",
      HTML(values$msg_prj))
  })
  outputOptions(output, "msg_prj", suspendWhenHidden = FALSE)
  
  output$n_prj <- renderText({
    nrow(get_projects())
  })
  outputOptions(output, "n_prj", suspendWhenHidden = FALSE)

  
  #* prj_map observe tags  ----
  observe({
    
    d_prjs <- get_projects()

    # map: redraw markers
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
    
  })
  
  #* prj_p ----
  #calculate_y_tech(tech)
  output$prj_p <- renderPlotly({
    suppressWarnings({
      plot_project_timelines(get_projects()) %>% 
        event_register("plotly_click")
    })
  })
  
  #* prj_p plotly_click ----
  observe({

    # event_register("prj_p", "plotly_click")
    d <- event_data("plotly_click")
    req(d)

    s <- get_projects() %>%
      filter(project == d$y)

    leafletProxy("prj_map") %>%
      flyTo(s$longitude, s$latitude, 8)

  })
  
  # management ----
  
  #* get_mgt() ----
  get_mgt <- reactive({
    get_mgt_tbl(ixns = values$ixns, d_mgt_tags)
  })
  
  #* box_mgt ----
  output$box_mgt <- renderText({
    
    n_ixns <- length(values$ixns)
    ifelse(
      n_ixns == 0,
      HTML(glue("Management Measures <small>({d_mgt_n} rows)</small>")),
      HTML(glue("Management Measures <small>({nrow(get_mgt())} of {d_mgt_n} rows; filtered by {n_ixns} interactions)</small>")))
  })
  
  #* tbl_mgt ----
  output$tbl_mgt <- renderDataTable({
    get_mgt()
  }, escape = F, rownames = F)
  
  # documents ----
  #* get_docs() ----
  get_docs <- reactive({
    get_docs_tbl(d_docs_tags, ixns = values$ixns, cks = input$cks_docs)
  })
  
  #* box_docs ----
  output$box_docs <- renderText({
    n_ixns <- length(values$ixns)
    n_cks  <- length(input$cks_docs)
    n_docs <- nrow(get_docs())
    
    ifelse(
      n_ixns == 0 & n_cks == 0,
      HTML(glue("FERC Documents <small>({d_docs_n} rows)</small>")),
      HTML(glue("FERC Documents <small>({n_docs} of {d_docs_n} rows; filtered by {n_ixns} interactions & {n_cks} checkboxes </small>")))
  })
  
  #* tbl_docs ----
  output$tbl_docs <- renderDataTable({
    get_docs()
  }, escape = F, rownames = F)
  
  # publications ----
  #* get_pubs() ----
  get_pubs <- reactive({
    get_pubs_tbl(d_pubs_tags, ixns = values$ixns)
  })

  # observeEvent(input$btn_add_ixn, {
  #   req(values$ixns)
  #   browser()
  # })

  
  #* msg_pub_tag ---
  output$msg_pub_tag <- renderUI({
    if (is.null(attributes(get_pubs())$message))
      return(NULL)
    div(
      class="alert alert-warning", role="alert",
      HTML(attributes(get_pubs())$message))
  })
  outputOptions(output, "msg_pub_tag", suspendWhenHidden = FALSE)

  #* box_pubs ----
  output$box_pubs <- renderText({
    n_ixns <- length(values$ixns)
    n_pubs <- nrow(get_pubs())
    
    ifelse(
      n_ixns == 0,
      HTML(glue("Tethys Publications <small>({d_pubs_n} rows)</small>")),
      HTML(glue("Tethys Publications <small>({n_pubs} of {d_pubs_n} rows; filtered by {n_ixns} interactions</small>")))
  })
  
  #* tbl_pubs ----
  output$tbl_pubs <- renderDataTable({
    get_pubs() %>% 
      #select(-uri, -title, -tag)
      select(ID, Title, Tags)
  }, escape = F, rownames = F)
  
  
  # spatial ----
  
  #* get_spatial() ----
  get_spatial <- reactive({
    
    # ST_Force_2D
    #DBI::dbGetQuery(con, "SELECT ST_Force2D();")
    #browser()
    d <- get_spatial_tbl(
      d_spatial_tags, 
      ixns    = values$ixns, 
      aoi_wkt = sf_to_wkt(values$ply))
    
    d
  })
    
  #* box_spatial ----
  output$box_spatial <- renderText({
    n_ixns    <- length(values$ixns)
    n_spatial <- nrow(get_spatial())

    ifelse(
      n_ixns == 0,
      HTML(glue("MarineCadastre Spatial datasets <small>({d_spatial_n} rows)</small>")),
      HTML(glue("MarineCadastre Spatial datasets <small>({n_spatial} of {d_spatial_n} rows; filtered by {n_ixns} interactions)</small>")))
  })
  

  #* tbl_spatial ----
  output$tbl_spatial <- renderDataTable({
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
    
    d
    # TODO: 'expand data' buttons for each row which, when clicked result in the corresponding sp_data being displayed as a df
  }, escape = F, rownames = F)

  
  # reports ----
  
  #* get_email() ----
  get_email <- reactive({
    email <- input$`login-g_email`
    if (is.null(email)){
      #message("get_email() is.null")
      values$rpts <- rpts_0
      return(NULL)
    } else {
      #message(glue("get_email(): {email}"))
      values$rpts <- get_user_reports(email)
    }
    email
  })
  observe(get_email())
  
  #* txt_rpt_login ----
  output$txt_rpt_login <- renderText({
    ifelse(
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
    
    get_rpts_tbl() %>% 
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
  }, escape = F)
  
  #* btn_rpt_create() ----
  observeEvent(input$btn_rpt_create, {
  
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
  })

  #* btn_del_rpts ----
  observeEvent(input$btn_del_rpts, {
    req(input$tbl_rpts_rows_selected)
    
    irows <- input$tbl_rpts_rows_selected
    email <- isolate(get_email())
    
    rpts_del <- get_rpts_tbl() %>% 
      slice(irows) %>% 
      pull(url) %>% 
      basename()

    sapply(rpts_del, del_user_report, email = email)
    
    values$rpts <- get_user_reports(email)
  })
}
