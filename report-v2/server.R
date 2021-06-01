
server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)

  values <- reactiveValues(
    ixns   = list(),
    rpts   = rpts_0 )
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
    leaflet(
      options = leafletOptions(
        zoomControl        = F,
        attributionControl = F)) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      setView(-93.4, 37.4, 2)
  })
  
  crud <- callModule(
    editMod, "mapEdit", map_edit, "ply",
    editorOptions = list(
      polylineOptions = F, markerOptions = F, circleMarkerOptions = F,
      singleFeature = T))
  
  observeEvent(input$btn_mod_map, {
    showModal(modalDialog(
      title = "Modify Location",
      editModUI("mapEdit"),
      easyClose = T))
  })
  
  observe({
    ply <- crud()$finished
    
    leafletProxy("map_side") %>%
      clearShapes()
    
    if (is.null(ply)){
      actionButton(
        "btn_mod_map", "Add", icon=icon("plus"))
    } else {
      bb <- sf::st_bbox(ply)
      
      leafletProxy("map_side") %>%
        addPolygons(data = ply) %>% 
        flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
      
      updateActionButton(
        session,
        "btn_mod_map", "Modify", icon=icon("gear"))
    }
  })
  
  # ixns ----
  
  output$ixn_btns <- renderUI({
    
    if (length(values$ixns) == 0)
      return(
        actionButton(
          "btn_add_ixn", "Add", icon=icon("plus"), width="263px"))
    
    tagList(
      actionButton(
        "btn_add_ixn" , "Add"         , icon=icon("plus"), width="120px", style="display:inline-block;"),
      actionButton(
        "btn_mod_ixns", "Modify (n=0)", icon=icon("gear"), width="120px", style="display:inline-block;margin-right:15px;float:right"))
  })
  
  observeEvent(input$btn_add_ixn, {
    req(input$sel_ixn_tags)
    
    values$ixns <- append(values$ixns, list(input$sel_ixn_tags))
    
    updateSelectInput(
      session, 
      "sel_ixn_tags",
      selected = "")
  })
  
  observeEvent(input$btn_mod_ixns, {
    showModal(modalDialog(
      title = "Modify Interactions",
      tagList(
        DTOutput("tbl_ixns"),
        actionButton("btn_del_ixns", "Delete selected interaction(s)", icon=icon("minus"))),
      easyClose = T))
  })
  
  output$tbl_ixns <- renderDT({
    req(values$ixns)
    
    # TODO: improve table in phases:
    #   1) replace df_tags.sql with prettier shorter df_tags.tag
    #   2) break into columns: technology | stressor | receptor
    tibble(
      Interaction = map_chr(values$ixns, paste, collapse = "; "))
  })
  
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
  output$prj_map <- renderLeaflet({
    leaflet(
      data = prj_sites, width = "100%",
      options = leafletOptions(
        zoomControl = F)) %>% 
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      addMarkers(
        label        = ~label_html, 
        popup        = ~popup_html) %>%
      htmlwidgets::onRender("function(el, x) {
          L.control.zoom({ position: 'topright' }).addTo(this)
        }") })
  
  
  output$prj_p <- renderPlotly(suppressWarnings({
    
    #browser()
    fig <- plot_ly(colors = cols, symbols = syms, height = 700)
    
    fig <- fig %>% 
      add_segments(
        data  = d_times,
        x     = ~date_beg,
        xend  = ~date_end,
        y     = ~project,
        yend  = ~project,
        color = ~project_status,
        line  = list(width = 10))
    #fig
    #plotly_json(p = fig)
    
    fig <- fig %>% add_markers(
      data = d_permits,
      x = ~license_date, 
      y = ~project,
      symbol = ~permit_type,
      symbols = syms_type,
      color = ~permit_type,
      colors = cols_type, 
      size = 10,
      hoverinfo = "text",
      hovertext = paste(
        'License Date: '    , d_permits$license_date, 
        '<br>Project Name: ', d_permits$project, 
        '<br>Permit Type: ' , d_permits$permit_type))
    
    #fig
    #plotly_json(p = fig)
    
    fig <- fig %>% 
      layout(
        xaxis = list(
          title = 'Date',
          showline = FALSE,
          showgrid = FALSE),
        yaxis = list(
          title = '',
          autorange = "reversed",
          domain = c(0,1),
          range = c(0, length(unique(d_times$project))),
          showline = FALSE,
          showgrid = FALSE,
          type = "category"),
        # margin = list(
        #   r = 10, 
        #   t = 25, 
        #   b = 40, 
        #   l = 100),
        legend = list(
          x = 1.01, 
          y = 0.5), 
        shapes = list(
          list(
            line = list(
              color = "black", 
              width = 0.8), 
            type = "line", 
            x0 = 0, 
            x1 = 1, 
            xref = "paper", 
            y0 = -0.5 + n_riv, #Defines horizontal line separating riverine projects from tidal projects
            y1 = -0.5 + n_riv, 
            yref = "y"), 
          list(
            line = list(
              color = "black", 
              width = 0.8), 
            type = "line", 
            x0 = 0, 
            x1 = 1, 
            xref = "paper", 
            y0 = -0.5 + n_riv + n_tid, #Defines horizontal line separating tidal projects from wave projects
            y1 = -0.5 + n_riv + n_tid, 
            yref = "y"), 
          list(
            line = list(
              color = "black", 
              width = 0.8), 
            type = "line", 
            x0 = 0, 
            x1 = 1, 
            xref = "paper", 
            y0 = 0.025, 
            y1 = 0.025, 
            yref = "paper"),
          list(
            line = list(
              color = "black", 
              width = 0.8), 
            type = "line", 
            x0 = 0, 
            x1 = 1, 
            xref = "paper", 
            y0 = 0.975, 
            y1 = 0.975, 
            yref = "paper"),
          list(
            line = list(
              color = "black", 
              width = 0.8
            ), 
            type = "line", 
            x0 = 0, 
            x1 = 0, 
            xref = "paper", 
            y0 = 0.975, 
            y1 = 0.025, 
            yref = "paper")),
        annotations = list(
          list(
            x = 1,
            y = (-1 + n_riv)/2,
            showarrow = FALSE,
            text = "<b>Riverine</b>",
            xref = "paper",
            yref = "y",
            align = "center",
            font = list(size = 14),
            textangle = "90",
            yshift = 4),
          list(
            x = 1,
            y = (-1 + n_riv + (n_tid)/2),
            showarrow = FALSE,
            text = "<b>Tidal</b>",
            xref = "paper",
            yref = "y",
            align = "center",
            font = list(size = 14),
            textangle = "90"),
          list(
            x = 1,
            y = (-1 + n_riv + n_tid + (n_wav)/2),
            showarrow = FALSE,
            text = "<b>Wave</b>",
            xref = "paper",
            yref = "y",
            align = "center",
            font = list(size = 14),
            textangle = "90")))
    
    fig
    
  }))
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click events appear here (double-click to clear)" else d
  })
  
  # Use a separate observer to zoom to point
  observe({
    d <- event_data("plotly_click")
    req(d)
    
    proxy <- leafletProxy("prj_map")
    
    s <- prj_sites %>% 
      filter(project == d$y)
    
    proxy %>% 
      flyTo(s$longitude, s$latitude, 8)
    
  })
  
  # management ----
  get_mgt <- reactive({
    if (length(values$ixns) == 0){
      d <- d_mgt
    } else {
      rowids <- sapply(values$ixns, get_rowids_with_ixn, db_tbl = "tethys_mgt_tags") %>% 
        unlist() %>% unique()
      d <- d_mgt %>%
        filter(rowid %in% !!rowids)
    }
    d %>% 
      select(-rowid) %>% 
      collect()
  })
  
  output$box_mgt <- renderText({
    n_ixns <- length(values$ixns)
    ifelse(
      n_ixns == 0,
      HTML(glue("Management Measures <small>({d_mgt_n} rows)</small>")),
      HTML(glue("Management Measures <small>({nrow(get_mgt())} of {d_mgt_n} rows; filtered by {n_ixns} interactions)</small>")))
  })
  
  output$tbl_mgt <- renderDataTable({
    get_mgt()
  })
  
  
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
  
  #* poll_rpts_tbl() ----
  poll_rpts_tbl <- reactivePoll(
    10000, session, # check every 10 seconds
    checkFunc = function() {
      email <- get_email()
      if (is.null(email)) 
        return("")
      lastmod <- get_user_reports_last_modified(email)
      #message(glue("poll_rpts_tbl({email}) {Sys.time()} -- lastmod: {lastmod}"))
      lastmod
      },
    valueFunc = function() {
      email <- get_email()
      if (is.null(email)) 
        return(rpts_0)
      #message(glue("poll_rpts_tbl({email}) set value {Sys.time()}"))
      #browser()
      values$rpts <- get_user_reports(email)
      values$rpts
    })
  observe(poll_rpts_tbl())
  
  #* get_rpts() ----
  get_rpts <- reactive({
    email       <- get_email()
    #message(glue("get_rpts() email: {email}"))
    values$rpts <- get_user_reports(email)
    values$rpts
  })
  observe(get_rpts())
  
  #* tbl_rpts ----
  output$tbl_rpts = renderDT({
    get_rpts() %>% 
      # arrange(desc(date)) %>% 
      mutate(
        title = ifelse(
          status == "published",
          glue("<a href='{url}' target='_blank'><b>{title}</b></a>"),
          glue("<b>{title}</b>")),
        status = ifelse(
          status == "published",
          "<span class='badge btn-success'>Published</span>",
          "<span class='badge btn-warning'>Rendering</span>")) %>% 
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
    # input <- list(
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
    #browser()
    q$interactions <- toJSON(m$interactions) # %>% as.character()
    # as.yaml(q) %>% cat()

    
    r <- GET(url_rpt_pfx, query = q)
    # message(glue("r$url: {r$url}"))
    
    #Sys.sleep(1)
    values$rpts <- get_user_reports(glogin()$email)
    
    # content(r)
  })

  #* btn_del_rpts
  observeEvent(input$btn_del_rpts, {
    req(input$tbl_rpts_rows_selected)
    
    irows <- input$tbl_rpts_rows_selected
    email <- isolate(get_email())
    
    rpts_del <- get_rpts() %>% 
      slice(irows) %>% 
      pull(url) %>% 
      basename()
    # message(glue("rpts_del: {paste(rpts_del, collapse=', ')}"))
    
    sapply(rpts_del, del_user_report, email = email)
    
    values$rpts <- get_user_reports(email)
    
    # browser()
    
    # dataTableProxy("tbl_rpts") %>% 
    #   selectRows(irows)
    # input$tbl_rpts
  })
}
