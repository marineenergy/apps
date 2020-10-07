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
        #st_geometry(crud()$finished) %>% write_sf(here("data/aoi_tmp__crud_finished_geom.geojson"))
        #crud()$finished %>% write_sf(here("data/aoi_tmp_crud_finished.geojson"))
        
        leaflet(
            options = leafletOptions(
                zoomControl = F,
                attributionControl = F)) %>%
            addProviderTiles(providers$Esri.OceanBasemap) %>%
            addPolygons(data = crud()$finished) #%>% 
            #setView(-93.4, 37.4, 4)
        
    })
    
    output$tbl_hab_spp <- function() {
        req(crud()$finished)
        
        aoi_wkt <- st_as_text(st_geometry(crud()$finished), EWKT=T)
        d_tbl <- "shp_CetMap_BIA_WGS84"
        
        dist_m  <- 10*1000
        sql <- glue("
            SELECT *
            FROM \"{d_tbl}\" d
            WHERE ST_DWithin(Geography(d.geometry), '{aoi_wkt}', {dist_m});")
        d_win <- st_read(con, query = sql)
        
        d_title <- "Biologically Important Areas for Cetaceans%"
        dbGetQuery(con, glue("SELECT * FROM dataset_shps WHERE title LIKE '{d_title}'"))
        
        message(glue("returning tagList with nrow(d_win): {nrow(d_win)}"))
        
        #glue("nrow(d_win): {nrow(d_win)}")
        d_win %>% 
            select(cmn_name, sci_name, stock_pop, bia_type, bia_time, bia_name, bia_id) %>% 
            st_drop_geometry() %>% 
            kable("html",
                  caption = HTML(markdownToHTML(
                      text = "[Cetacean Biologically Important Areas](https://cetsound.noaa.gov/important) within 10 kilometers of site."))) %>%
            kable_styling("striped", full_width = F)
        
        # leaflet() %>% 
        #     addProviderTiles(providers$Esri.OceanBasemap, group = "Esri Ocean") %>% 
        #     addProviderTiles( providers$Stamen.TonerLite, group = "Toner Lite") %>%
        #     addPolygons(data = aoi_ply, color =    "red", group = "AOI") %>% 
        #     addPolygons(data =   d_win, color =  "green", group = "BIAs w/in 10km") %>% 
        #     addPolygons(data =   d_ply, color =   "#03F", group = "BIAs") %>% 
        #     addScaleBar() %>% 
        #     fitBounds(bb[1], bb[2], bb[3], bb[4]) %>% 
        #     addLayersControl(
        #         baseGroups = c("Esri Ocean", "Toner Lite"),
        #         overlayGroups = c("AOI", "BIAs w/in 10km", "BIAs"),
        #         options = layersControlOptions(collapsed = FALSE)) %>% 
        #     hideGroup("BIAs"),
        # d_win %>% 
        #     st_drop_geometry() %>% 
        #     datatable())
    }
    
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
            #h5(glue("{tech1}: {input$selTech} in {input$selEnv} environment")),
            h5(glue("{tech1}: {input$selTech}")),
            img(src=img_tech))
    })
    
    output$txtTitle <- renderText({ input$txtTitle })
    
    
    output$dt_s_r_ckbox <- renderDT({
        s_r_ckbox <- read_csv(s_r_ckbox_csv)
        
        s_r_ckbox
    }, 
    editable  = T,
    rownames  = F,
    server    = F,
    options   = list(
        pageLength = Inf, 
        #dom        = 't',
        searching  = F, 
        bPaginate  = F, 
        info       = F))
    
    observeEvent(input$btnStressorReceptors, {
        # http://stla.github.io/stlapblog/posts/shiny_editTable.html
        
        s_r_ckbox <- read_csv(s_r_ckbox_csv) 
        #editTable(s_r_ckbox, outdir="~/Documents/", outfilename="newDF")
        
        # TODO: maked checkbox table editable and save per user session
        # * [Using DT in Shiny](https://rstudio.github.io/DT/shiny.html)
        # * [Double-click to edit table cells](https://yihui.shinyapps.io/DT-edit/)
        # * [Radio Buttons in Tables](https://rstudio.github.io/DT/011-radio.html)
        # * [Saturn Elephant - Useful callbacks for DT (in Shiny)](https://laustep.github.io/stlahblog/posts/DTcallbacks.html)
        # * [Edit a table with Shiny and rhandsontable](http://stla.github.io/stlapblog/posts/shiny_editTable.html)
        
        w_l_modal = 900
        w_tbl = w_l_modal - (15 * 2)
        w_col = w_tbl/ncol(s_r_ckbox)
        
        showModal(modalDialog(
            size = "l",
            title = "Select Stressor-Receptors",
            #"Table with clickable elements here...",
            #DTOutput('dt_s_r_ckbox'),
            #rhandsontable(s_r_ckbox, width = 600, height = 300),
            rhandsontable(
                s_r_ckbox, 
                rowHeaders = NULL, width = w_tbl) %>%
                hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
                hot_cols(colWidths = w_col),
            easyClose = TRUE,
            footer = tagList(
                modalButton("Cancel"),
                actionButton("ok", "OK"))))
    })
    
    observeEvent(input$btnSelectProjects, {

        showModal(modalDialog(
            size = "l",
            title = "Select Projects",
            "TODO: Select: [ ] Tethys; [X] FERC eLibrary",
            "Table of selectable Projects",
            "(links to project mitigations from either Tethys or FERC e-Library documents. Ideally, it will search Tethys Project Site (Content Type) documents and manually downloaded FERC e-library documents according to user-specified geography, technology, and/or stressor-receptors.)",
            easyClose = TRUE,
            footer = tagList(
                modalButton("Cancel"),
                actionButton("ok", "OK"))))
    })
})
