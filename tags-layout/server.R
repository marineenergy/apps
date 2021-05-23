
server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)

  values <- reactiveValues(
    ixns   = list())
  
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
      bb <- st_bbox(ply)
      
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
        actionButton("btn_del_ixns", "Delete selected interaction(s)")),
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
      data = prj_sites, width = "100%") %>% 
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      addMarkers(
        label        = ~label_html, 
        popup        = ~popup_html) })
  
  output$prj_p <- renderPlotly({
    
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
    
    #fig
    
  })
  
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
  
  # temp plot ----
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
}
