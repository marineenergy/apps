shinyServer(function(input, output, session) {

    edits <- callModule(
        editMod,
        leafmap = map,
        id = "map"
    )
    
    observeEvent(input$save, {
        
        geom <- edits()$finished
        
        if (!is.null(geom)) {
            assign('new_geom', geom, envir = .GlobalEnv)
            write_sf(geom, "data/geom.geojson", delete_layer = TRUE, delete_dsn = TRUE)
            
            #geom <- read_sf("data/geom.geojson")
            b <- st_bbox(geom)
            
            leafletProxy("report_map") %>%
                clearShapes() %>%
                addPolygons(data = geom) %>% 
                fitBounds(b[["xmin"]], b[["ymin"]], b[["xmax"]], b[["ymax"]])
            
            # leafletProxy("side_map") %>%
            #     clearShapes() %>%
            #     addPolygons(data = geom) %>% 
            #     fitBounds(b[["xmin"]], b[["ymin"]], b[["xmax"]], b[["ymax"]])
        }
        
    })
    
    output$report_map <- renderLeaflet({
        
        leaflet(
            options = leafletOptions(
                zoomControl = F,
                attributionControl = F)) %>%
            addProviderTiles(providers$Esri.OceanBasemap) %>% 
            setView(-93.4, 37.4, 4)
    })
    
    output$side_map <- renderLeaflet({
        
        leaflet(
            options = leafletOptions(
                zoomControl = F,
                attributionControl = F)) %>%
            addProviderTiles(providers$Esri.OceanBasemap) %>% 
            setView(-93.4, 37.4, 4)
    })
    
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
