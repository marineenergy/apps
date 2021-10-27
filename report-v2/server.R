
server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)

  values <- reactiveValues(
    ixns   = list(),
    rpts   = rpts_0,
    ply    = NULL)
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
  
  # tmp <- sf::st_sfc()
  # class(tmp)[1] <- "sfc_POLYGON"
  # ply_editable_0 <- sf::st_sf(X_leaflet_id = integer(0), feature_type = character(0), geometry=tmp, crs = 4326)
  # map_edit <- leaflet(
  #   options = leafletOptions(
  #     zoomControl = T,
  #     attributionControl = F)) %>% 
  #   addProviderTiles(providers$Esri.OceanBasemap) %>% 
  #   # addPolygons(data = ply_editable_0, group = "ply_editable") %>% 
  #   setView(-93.4, 37.4, 4)
  
  # Location input
  # crud <- callModule(
  #   editMod, 
  #   leafmap       = map_edit,
  #   id            = "map_editor", 
  #   # [edit existing feature within Shiny and save](https://github.com/r-spatial/mapedit/issues/105#issuecomment-552198352)
  #   targetLayerId = "ply_editable",  # "ply",
  #   editor        = "leaflet.extras", # "leafpm"
  #   editorOptions = list(
  #     polylineOptions = F, markerOptions = F, circleMarkerOptions = F,
  #     #editOptions = T, 
  #     singleFeature = T))
  
  # output$map_editable <- renderLeaflet({
  #   req(crud()$all)
  #   mapview(crud()$all)@map
  # })
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
    
    #browser()
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
 
  # observe({
  #   #use the draw_stop event to detect when users finished drawing
  #   
  #   # https://github.com/bhaskarvk/leaflet.extras/blob/master/inst/examples/shiny/draw-events/app.R
  #   #req(input$mapeditor_draw_stop)
  #   req(input$mapeditor_draw_all_features)
  #   message("draw_all_features SOLO")
  # })
  
  # observe({
  #   features <- input$mapeditor_draw_all_features
  #   message("observe: input$mapeditor_draw_all_features")
  #   
  #   
  # })
  
  observe({
    #use the draw_stop event to detect when users finished drawing
    
    # https://github.com/bhaskarvk/leaflet.extras/blob/master/inst/examples/shiny/draw-events/app.R
    #req(input$mapeditor_draw_stop)
    req(input$mapeditor_draw_all_features)
    #browser()
    message("draw_all_features")
    
    feature <- isolate(input$mapeditor_draw_all_features$features[[1]])
    
    # if(!is.null(isolate(input$mapeditor_draw_stop))){
    #   message("  New Feature")
    #   feature <- isolate(input$mapeditor_draw_new_feature)
    # }
    # if(!is.null(isolate(input$mapeditor_draw_edited_features))){
    #   message("  Edited Feature")
    #   feature <- isolate(input$mapeditor_draw_edited_features)
    # }
    # browser()
    #print(feature)
    #message("mapeditor_draw_stop")
    #browser()
    # polygon_coordinates <- input$mymap_draw_new_feature$geometry$coordinates[[1]]
    # # see  https://rstudio.github.io/leaflet/shiny.html
    # bb <- input$mymap_bounds
    #geom_polygon <- input$feature$geometry
    #geom_polygon <- feature$geometry
    # drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
    ply_json <- geojsonio::as.json(feature$geometry)
    # spdf <- geojsonio::geojson_sp(feature)
    ply <- st_read(ply_json, quiet = T)
    #ply_wkt <- st_as_text(st_geometry(ply))
    values$ply <- ply
    #mymap_proxy = leafletProxy("mymap") %>% clearPopups() %>% addPopups(south,west,coord)
    #textOutput("wkt")
    
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
    
    # ply <- crud()$finished
    # if (!is.null(ply)){
    #   bb <- sf::st_bbox(ply)
    #   #browser()
    #   
    #   leafletProxy("map_editor-map") %>%
    #     addPolygons(data = ply, group = "ply_editable") %>% 
    #     flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
    # }    
  })
  
  
  
  # observe({
  #   #ply <- crud()$finished
  #   ply <- crud()$all
  #   
  #   leafletProxy("map_side") %>%
  #     clearShapes()
  #   
  #   if (is.null(ply)){
  #     actionButton(
  #       "btn_mod_map", "Add", icon=icon("plus"))
  #   } else {
  #     bb <- sf::st_bbox(ply)
  #     
  #     leafletProxy("map_side") %>%
  #       addPolygons(data = ply, group = "ply_editable") %>% 
  #       flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
  #     
  #     updateActionButton(
  #       session,
  #       "btn_mod_map", "Modify", icon=icon("cog"))
  #   }
  # })
  
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
    # updateSelectInput(
    #   session, 
    #   "sel_ixn_tags",
    #   selected = "")
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
  load_projects()
  tech <<- c("Riverine Energy", "Tidal Energy", "Wave Energy")
  
  #* prj_map ----
  output$prj_map <- renderLeaflet({
    
    map_projects(prj_sites)})
  
  #* prj_map observe Technology tag  ----
  observe({
    req(length(values$ixns) > 0)
    # browser()
    # output$prj_p <- renderText(suppressWarnings(""))
    
    # extract technology from interaction tags
    sql2tech <- c(
      "Technology.Riverine" = "Riverine Energy", 
      "Technology.Tidal"    = "Tidal Energy",
      "Technology.Wave"     = "Wave Energy")
    
    if (!is.null(values$ixns)){
      tech <<- sql2tech[intersect(names(sql2tech), values$ixns %>% unlist())]
    } else if (is.null(values$ixns)) {
      tech <<- sql2tech
    }
    
    values_unlist  <- values$ixns %>% unlist() 
    tech_str_match <- grepl("Technology", values_unlist)
    
    # if tech type selected, redraw ONLY mkrs corresponding to selected tech types
    if (TRUE %in% tech_str_match){
      load_projects()
      prj_tech <<- prj_sites %>% 
        filter(technology_type %in% tech)
      leaflet::leafletProxy("prj_map") %>% 
        leaflet::clearMarkers() %>% 
        leaflet::addMarkers(
          data  = prj_tech,
          label = ~label_html, 
          popup = ~popup_html)
    # if NO tech type selected in ixns, redraw ALL markers
    } else if (!TRUE %in% tech_str_match){
      load_projects()
      prj_tech <<- prj_sites
      leaflet::leafletProxy("prj_map") %>% 
        leaflet::clearMarkers() %>% 
        leaflet::addMarkers(
          data  = prj_tech,
          label = ~label_html, 
          popup = ~popup_html)
    }
  })
  
  #* prj_p ----
  calculate_y_tech(tech)
  output$prj_p <- renderPlotly(
    suppressWarnings({
      plot_projects()
  }))

  #* prj_p observe Technology tag  ----
  observe({
    req(length(values$ixns) > 0)
    
    # vector: T = tech selected, F = no tech selected
    values_unlist  <- values$ixns %>% unlist() 
    tech_str_match <- grepl("Technology", values_unlist)
  
    # if no tech type selected in ixns 
    if (!TRUE %in% tech_str_match){
      # load_projects()
      tech <<- c("Riverine Energy", "Tidal Energy", "Wave Energy")
      message(glue("no explicitly selected tech so plot all tech: {paste(tech, collapse = ', ')}"))

      # calculate_y_tech(tech)
      output$prj_p <- renderPlotly(
        suppressWarnings({
          plot_projects()
        })
      )
    }
  
    else if (TRUE %in% tech_str_match){
      load_projects() 
      # output$prj_p <- renderText(suppressWarnings(""))
  
      # extract technology from interaction tags
      tags2tech <- c(
        "Technology.Riverine" = "Riverine Energy", 
        "Technology.Tidal"    = "Tidal Energy",
        "Technology.Wave"     = "Wave Energy")
      
      # if an ixn exists, find tech selected in the ixn
      if (!is.null(values$ixns)){
        tech <<- tags2tech[intersect(names(tags2tech), values$ixns %>% unlist())] %>% 
          unname()
      } else {
        tech <<- tags2tech 
      }
      
      output$prj_p <- renderPlotly(
        suppressWarnings({
          update_project_plot()
          })
        )
    }
  })
  

  #* observe plotly_click ----
  observe({

    # event_register("prj_p", "plotly_click")
    d <- event_data("plotly_click")
    req(d)

    proxy <- leafletProxy("prj_map")

    s <- prj_sites %>%
      filter(project == d$y)

    proxy %>%
      flyTo(s$longitude, s$latitude, 8)

  })
  
  # management ----
  
  #* get_mgt() ----
  get_mgt <- reactive({
    
    # TODO: functionalize for any tabular content
    if (length(values$ixns) == 0){
      d <- d_mgt_tags
    } else {
      rowids <- sapply(values$ixns, get_rowids_with_ixn, db_tbl = "tethys_mgt_tags") %>% 
        unlist() %>% unique()
      d <- d_mgt_tags %>%
        filter(rowid %in% !!rowids)
    }
    
    # TODO: functionalize for any tabular content
    # d_1 <- 
    d_to_tags_html(d)
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
    d <- d_docs
    if (length(values$ixns) > 0){
      rowids <- sapply(values$ixns, get_rowids_with_ixn, db_tbl = "ferc_doc_tags") %>% 
        unlist() %>% unique()
      d <- d %>%
        filter(rowid %in% !!rowids)
    }
    if (length(input$cks_docs) > 0){
      for (col_bln in input$cks_docs){
        d <- d %>% # collect() %>% nrow() # 1426
          filter(.data[[col_bln]] == TRUE) # %>% collect() %>% nrow()
        # message(glue("ck_docs == `{col_bln}` nrow: {d %>% collect() %>% nrow()}"))
      }
    }
    #browser() # TODO 236 #
    # d %>% filter(rowid == 236) %>% collect()
    # d_0 <<- d
    d <- d_to_tags_html(d)
    
    # rowid 236 showing up 2x in results
    # tbl(con, "ferc_docs") %>% arrange(desc(rowid))
    # tbl(con, "ferc_doc_tags") %>% filter(rowid == 236)
    
    d %>% 
      mutate(
        # TODO: include in scripts/update_tags.R:update_tags()
        across(where(is.character), na_if, "NA"),
        across(starts_with("ck_"), as.character),
        across(starts_with("ck_"), recode, "TRUE"="✓", "FALSE"="☐"),
        Doc = ifelse(
          is.na(prj_doc_attachment),
          prj_document,
          paste0(prj_document, ": ", prj_doc_attachment)),
        Doc = ifelse(
          is.na(prj_doc_attach_url),
          Doc,
          glue("<a href='{prj_doc_attach_url}'>{Doc}</a>"))) %>% 
      #names()
      select(
        ID, Project=project, Document=Doc, Detail=detail, Tags,
        Ixn = ck_ixn, 
        Obs = ck_obs, 
        MP  = ck_mp, 
        AMP = ck_amp, 
        PME = ck_pme, 
        BMP = ck_bmps)
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
    d <- d_pubs
    if (length(values$ixns) > 0){
      rowids <- sapply(values$ixns, get_rowids_with_ixn, db_tbl = "tethys_pub_tags") %>% 
        unlist() %>% unique()
      d <- d %>%
        filter(rowid %in% !!rowids)
    }

    #browser()
    d <- d_to_tags_html(d)
    
    d %>% 
      mutate(
        # TODO: include in scripts/update_tags.R:update_tags()
        across(where(is.character), na_if, "NA"),
        Title = as.character(glue("<a href='{uri}'>{title}</a>")))
  })
  
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
    
    # update mc_spatial table from  [spatial | marineenergy.app - Google Sheet](https://docs.google.com/spreadsheets/d/1MMVqPr39R5gAyZdY2iJIkkIdYqgEBJYQeGqDk1z-RKQ/edit#gid=936111013):
    #   source(file.path(dir_scripts, "db.R")); source(file.path(dir_scripts, "update.R")); update_spatial()

    # TODO: add Github issue for future reference
    # get_spatial_intersection(dataset_code='ocs-lease-blk', aoi_wkt='POLYGON ((-128.3687 31.86402, -128.3687 49.68904, -116.5255 49.68904, -116.5255 31.86402, -128.3687 31.86402))')
    # Warning: Error in : Problem with `mutate()` column `sp_data`.
    # ℹ `sp_data = map(code, get_spatial_intersection, aoi_wkt = aoi_wkt)`.
    # x Failed to fetch row: ERROR:  lwgeom_intersection: GEOS Error: TopologyException: Input geom 0 is invalid: Self-intersection at or near point -179.95155358242297 47.850116156353714 at -179.95155358242297 47.850116156353714
    # Fixed by updating to select_sql to using ST_MakeValid() for problematic shapefile: ...select prot_numbe, prot_aprv_, block_numb, blk_fed_ap, mms_region, mms_plan_a, ST_MakeValid(geometry) AS geometry from "shp_AK_BLKCLP")
    
    d <- d_spatial 
    
    # filter by Tags
    if (length(values$ixns) > 0){
      rowids <- sapply(values$ixns, get_rowids_with_ixn, db_tbl = "mc_spatial_tags") %>% 
        unlist() %>% unique()
      d <- d %>%
        filter(rowid %in% !!rowids)
    }
    d <- d_to_tags_html(d) %>% 
      mutate(
        Title = as.character(glue("{title} (Source: <a href='{src_url}'>{src_name}</a>)"))) %>% 
      arrange(Title)
    
    # get Location
    #browser()
    aoi_wkt <- ifelse(
      # !is.null(crud()$finished),
      # crud()$finished %>% pull(geometry) %>% sf::st_as_text(),
      !is.null(values$ply),
      values$ply %>% pull(geometry) %>% sf::st_as_text(),
      NA)
    
    # browser()
    
    # filter by Location
    d <-  d %>%
      # replace_na(list(buffer_km = 0)) %>%  # "
      mutate(
        sp_data = map(code, get_spatial_intersection, aoi_wkt = aoi_wkt)) # , output = "tibble"    

    # TODO: - [ ] get to work when d's nrow == 0
    #       - [ ] ixn != fish
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
    
    #browser()
    
    d <- d %>% 
      mutate(
        `Rows in Results` = map_int(sp_data, nrow)) %>% 
      select(ID, Title, Tags, `Rows in Results`) # , sp_data)
    
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
    
    # message(glue("
    #   input$txt_rpt_title: {rpt_title}
    #   input$sel_rpt_ext:   {out_ext}
    #   glogin()$email:      {email}"))
    
    if (rpt_title == "") 
      return()
    # TODO: message if missing title
    
    # metadata
    # email = "bdbest@gmail.com"; rpt_title = "Test Report"; out_ext = "html"
    # values <- list(
    #   ixns = list(
    #     c("Receptor.Fish", "Stressor.PhysicalInteraction.Collision"),
    #     c("Technology.Wave", "Receptor.Birds")))
    # input <- list(x
    #   ck_rpt_prj = T,
    #   ck_rpt_mgt = T)
    m <- list(
      email        = email,
      date         = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      title        = rpt_title,
      filetype     = out_ext,
      contents     = list(
          projects   = input$ck_rpt_prj,
          management = input$ck_rpt_mgt),
      interactions = values[["ixns"]])
    # list(params = m) %>% as.yaml() %>% cat()
    # TODO: Spatial wkt in meta
    
    # hash <- digest(m, algo="crc32")
    # yml <- glue("{dir_rpt_pfx}/{email}/MarineEnergy.app_report_{hash}_shiny.yml")
    # dir.create(dirname(yml), showWarnings = F)
    # write_yaml(m, yml)

    # params for Rmd
    # p <- m
    # p$Content      <- list(value = p$Content)
    # p$Interactions <- list(value = p$Interactions)
    # as.yaml(p) %>% cat()
        
    # submit report creation job request to API
    q <- m
    q$contents     <- toJSON(m$contents) # %>% as.character()
    q$interactions <- toJSON(m$interactions) # %>% as.character()
    # as.yaml(q) %>% cat()

    
    r <- GET(url_rpt_pfx, query = q)
    # message(glue("r$url: {r$url}"))
    
    #Sys.sleep(1)
    values$rpts <- get_user_reports(glogin()$email)
    
    # content(r)
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
    # message(glue("rpts_del: {paste(rpts_del, collapse=', ')}"))
    
    sapply(rpts_del, del_user_report, email = email)
    
    values$rpts <- get_user_reports(email)
    
    # dataTableProxy("tbl_rpts") %>% 
    #   selectRows(irows)
    # input$tbl_rpts
  })
}
