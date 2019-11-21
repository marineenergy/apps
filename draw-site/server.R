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
            write_sf(geom, 'new_geom.geojson', delete_layer = TRUE, delete_dsn = TRUE)
        }
        
    })
    
})
