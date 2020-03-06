shinyServer(function(input, output, session) {
    
    # config <- reactiveValues(
    #     title  = input$txtTitle,
    #     tech   = input$selTech,
    #     env    = input$selEnv,
    #     geom   = NULL
    #     #seshid = 
    # )

    crud <- callModule(
        editMod,
        "mapEdit",
        m,
        "ply",
        #editor = "leaflet.extras") # aka breweries91
        editor = "leafpm") # aka breweries91
    
    # crud <- callModule(editMod, "test-edit", m, "breweries91")
    output$side_map <- renderLeaflet({
        req(crud()$finished)
        
        #browser()
        if (any(st_geometry_type(crud()$finished) != "POLYGON")){
            #browser()
            #alert("Sorry, only polygons are currently supported.")
            showModal(modalDialog ("Sorry, only polygons are currently supported."))
            req(NULL)

        }
        message(glue("st_geometry_type(crud()$finished): {st_geometry_type(crud()$finished)}"))
        
        #mapview(crud()$finished)@map
        #browser()
        
        #mapview(crud()$finished)@map
        
        # nc = st_read(system.file("shape/nc.shp", package="sf"))
        # summary(nc) # note that AREA was computed using Euclidian area on lon/lat degrees
        
        # sf_geojson()
        # sf::read_sf()
        # 
        # config$geom <- st_geometry(crud()$finished) %>% 
        #     
        # 
        # write_yaml()
        
        #browser()
        #st_geometry(crud()$finished) %>% write_sf(here("data/test_poly.geojson"))
        
        leaflet(
            options = leafletOptions(
                zoomControl = F,
                attributionControl = F)) %>%
            addProviderTiles(providers$Esri.OceanBasemap) %>%
            addPolygons(data = crud()$finished) #%>% 
            #setView(-93.4, 37.4, 4)
        
    })
    
    
    # observeEvent(input$save, {
    #     
    #     geom <- edits()$finished
    #     
    #     if (!is.null(geom)) {
    #         assign('new_geom', geom, envir = .GlobalEnv)
    #         write_sf(geom, "data/geom.geojson", delete_layer = TRUE, delete_dsn = TRUE)
    #         
    #         #geom <- read_sf("data/geom.geojson")
    #         b <- st_bbox(geom)
    #         
    #         leafletProxy("report_map", data = geom) %>%
    #             clearShapes() %>%
    #             addPolygons() %>% 
    #             fitBounds(b[["xmin"]], b[["ymin"]], b[["xmax"]], b[["ymax"]])
    #         
    #         leafletProxy("side_map", data = geom) %>%
    #             clearShapes() %>%
    #             addPolygons() %>% 
    #             fitBounds(b[["xmin"]], b[["ymin"]], b[["xmax"]], b[["ymax"]])
    #     }
    #     
    # })
    
    output$report_map <- renderLeaflet({
        
        # leaflet(
        #     options = leafletOptions(
        #         zoomControl = F,
        #         attributionControl = F)) %>%
        #     addProviderTiles(providers$Esri.OceanBasemap) %>%
        #     setView(-93.4, 37.4, 4)
        
        req(crud()$finished)
        
        #mapview(crud()$finished)@map
        #browser()
        
        #mapview(crud()$finished)@map
        
        leaflet(
            options = leafletOptions(
                zoomControl = F,
                attributionControl = F)) %>%
            addProviderTiles(providers$Esri.OceanBasemap) %>%
            addPolygons(data = crud()$finished)
        
        #req(crud()$finished)
        
        #mapview(crud()$finished)@map
        
        # leaflet() %>%
        #     addProviderTiles(providers$Esri.OceanBasemap) %>%
        #     addPolygons(ply) #%>% 
        
    })
    
    # output$side_map <- renderLeaflet({
    #     
    #     leaflet(
    #         options = leafletOptions(
    #             zoomControl = F,
    #             attributionControl = F)) %>%
    #         addProviderTiles(providers$Esri.OceanBasemap) %>% 
    #         setView(-93.4, 37.4, 4)
    # })
    
    output$tech_ui <- renderUI({
        tech1 <- tech %>% 
            filter(tech2 == input$selTech) %>% 
            pull(tech1)
        img_tech <- tech %>% 
            filter(tech2 == input$selTech) %>% 
            pull(gif)
        
        tagList(
            h4("Technology"),
            h5(glue("{tech1}: {input$selTech} in {input$selEnv} environment")),
            img(src=img_tech))
    })
    
    output$txtTitle <- renderText({ input$txtTitle })
    
})
