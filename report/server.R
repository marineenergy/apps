shinyServer(function(input, output, session) {
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query$nav)) {
      nav <- strsplit(query$nav,"/")[[1]]
      updateTabsetPanel(session, 'nav', nav)
    }
  })
  
  #addClass(selector = "body", class = "sidebar-collapse")
  
  #set_logging_session()
  w <- Waiter$new()
    
  crud <- callModule(
    editMod, "mapEdit", map_default, "ply",
    editorOptions = list(
      polylineOptions = F, markerOptions = F, circleMarkerOptions = F,
      singleFeature = T))

  vals <- reactiveValues(
    queries_lit = tibble(
      Receptors  = character(0),
      Stressors  = character(0),
      Technology = character(0))) 

  #* updateLitQueries() ----
  updateLitQueries <- function(technology, receptors, stressors){
    
    #browser()
    
    queries <- expand.grid(
      Receptors  = receptors, 
      Stressors  = stressors,
      Technology = technology,
      stringsAsFactors = F)
    
    vals$queries_lit <<- full_join(
      vals$queries_lit,
      queries,
      by = c("Receptors", "Stressors", "Technology")) %>% 
      distinct(Receptors, Stressors, Technology) %>% 
      arrange(Receptors, Stressors, Technology)
    
  }
  
  #* btnAddLitQuery ----
  observeEvent(input$btnAddLitQuery, {
    
    updateLitQueries(
      technology = input$selTech,
      receptor   = input$selReceptors %>% paste(collapse = " AND "),
      stressor   = input$selStressors %>% paste(collapse = " AND "))
  })
  
  
  observeEvent(input$btnAddAllLitQueries, {
    
    updateLitQueries(
      technology = input$selTech,
      receptor   = choices_receptors,
      stressor   = choices_stressors)
  })
  
  observeEvent(input$btnAddReceptorLitQueries, {
    
    updateLitQueries(
      technology = input$selTech,
      receptor   = input$selReceptors %>% paste(collapse = " AND "),
      stressor   = choices_stressors)
  })
  
  observeEvent(input$btnAddStressorLitQueries, {
    
    updateLitQueries(
      technology = input$selTech,
      receptor   = choices_receptors,
      stressor   = input$selStressors %>% paste(collapse = " AND "))
  })
  
  # tblLitQueries ----
  output$tblLitQueries <- renderDT(

    #browser("output$tblLitQueries")
    vals$queries_lit
  )
  #observe({ vals$queries_lit })
  
  # btnRmLitQ* ----
  observeEvent(input$btnRmLitQuery, {
    
    vals$queries_lit <- vals$queries_lit %>% 
      slice(-input$tblLitQueries_rows_selected)

  })

  observeEvent(input$btnRmAllLitQuery, {
    
    vals$queries_lit <- tibble(
      Receptors  = character(0),
      Stressors  = character(0),
      Technology = character(0))

  })

  get_rmd_params <- function(){
    
    if (is.null(crud()$finished)){
      aoi_wkt <- NULL
    } else {
      aoi_wkt <- crud()$finished %>% pull(geometry) %>% st_as_text()
    }
    
    #browser()
    
    lit_queries <- vals$queries_lit %>% 
      mutate(
        q = pmap(., function(Receptors, Stressors, Technology, ...){
          keys <- c(Receptors, Stressors, Technology) %>% 
            str_replace_all('"', '') %>%
            na_if("") %>% 
            na.omit()
          paste(keys, collapse = " AND ") })) %>% 
      pull(q) %>% 
      as.character()
    # TODO: check multiple Receptors etc OK
    
    spatial_receptors <- vals$queries_lit %>% 
      mutate(
        q = pmap(., function(Receptors, ...){
          keys <- c(Receptors) %>% 
            str_replace_all('"', '') %>%
            na_if("") %>% 
            na.omit()
          paste(keys, collapse = " AND ") })) %>% 
      pull(q) %>% 
      as.character()
    
    technologies <- vals$queries_lit %>% 
      distinct(Technology) %>% 
      pull(Technology)
    # TODO: handle multiple technologies in report_template Configuration
    
    if (length(technologies) == 0)
      technology = "Marine Energy"
      
    rmd_params <- list(
      title              = input$txtTitle,
      aoi_wkt            = aoi_wkt,
      
      technology         = technologies,
      #stressors          = input$selStressors,
      #receptors          = input$selReceptors,
      #stressor_receptors = stressor_receptors,
      lit_queries        = lit_queries,
      spatial_receptors  = spatial_receptors,
      
      lit_tethys         = input$ckboxLitTethys,
      lit_ferc           = input$ckboxLitFERC,
      spatial            = input$ckboxSpatialReceptors,
      mgt_tethys         = input$ckboxMgtTethys)
    
    message(cat(as.yaml(list(params = rmd_params))))
    
    rmd_params
  }
  
  # download report pdf/docx/htm ----
  download_report = function(out_fmt, out_ext){
    downloadHandler(
      filename = function() {
        paste0('mhk-env_report_', str_replace_all(format(Sys.time(), tz='GMT'), '[ ]', '.'), '-GMT.', out_ext)},
      content = function(file) {
        
        #url = bkmark(session)
        #plots = values$saved_plots
        
        input_rmd <- here("report_template_gen.Rmd")
        
        rmd_params <- get_rmd_params()
        # render(tmp_rmd, output_format=out_fmt, output_file=file, params = list(url=url))
        # saveRDS(rmd_params, here("data/tmp_rmd_params.rds"))
        # rmd_params <- readRDS(here("data/tmp_rmd_params.rds"))
        # file <- here("data/tmp_rmd_params_report.html")
        
        # log_event("download_report")
        # log_output(list(input_rmd=input_rmd, out_fmt=out_fmt, file=file) %>% as.yaml())
        # log_output(rmd_params %>% as.yaml())
        log_info("download_report")
        log_info(list(input_rmd=input_rmd, out_fmt=out_fmt, file=file) %>% as.yaml())
        log_info(rmd_params %>% as.yaml())

        waiter_show(html = waiting_screen, color = "black")
        
        render(input=input_rmd, output_format=out_fmt, output_file=file, params = rmd_params)
        # TODO: hash rmd_params for filename and save params.yml and filename.out_fmt
        
        waiter_hide()
      })
  }
  
  # out btn_download_* ----
  output$btn_download_pdf = download_report('pdf_document' ,'pdf')
  output$btn_download_doc = download_report('word_document','docx')
  output$btn_download_htm = download_report('html_document','html')
  
  # TODO: observe btn_download_url ----
  observeEvent(input$btn_download_url, {
    url = bkmark(session)
    showModal(urlModal(url, title='Bookmarked application link'))
    #browseURL(url)
  })
  
  # TODO: bkmark() ----
  bkmark = function(session){
    # return url
    
    # session$doBookmark() -- without showBookmarkUrlModal(url)
    state <- shiny:::ShinySaveState$new(
      input = session$input, exclude = session$getBookmarkExclude())
    #state$values$saved_plots <- values$saved_plots
    state$values$saved_time  <- Sys.time()
    
    url <- shiny:::saveShinySaveState(state)
    
    clientData <- session$clientData
    url <- paste0(
      clientData$url_protocol, "//", clientData$url_hostname,
      if (nzchar(clientData$url_port))
        paste0(":", clientData$url_port), clientData$url_pathname,
      "?", url)
    
    url
  }
  
  # TODO: onRestored ----
  onRestored(function(state) {
    cat("Restoring from state bookmarked at", state$values$saved_time, "\n", file=stderr())
    
    #values$saved_plots <- state$values$saved_plots
    
    #load_plot(plot_titles()[1])
  })
  
  # start introjs when button is pressed with custom options and events
  observeEvent(
    input$tour,
    introjs(session, events = list(onbeforechange = readCallback("switchTabs"))))
  
  # js$disableTab("Projects")
  # js$disableTab("Regulations")
  # js$disableTab("Management")
  # js$disableTab("Reports")
  
# tblLiterature() ----
output$tblLiterature <- renderDT({
    req(vals$queries_lit)
    
    message("output$tblLiterature")
    
    if (nrow(vals$queries_lit) == 0 ){
      dt_empty <- tibble(
        message = "Please Configure Tags to see results here") %>%
        datatable(rownames = F, options = list(dom = 't'))
      
      return(dt_empty)
    }
    
    q <- vals$queries_lit %>% 
      add_rownames() %>% 
      pivot_longer(-rowname, names_to = "type", values_to = "tag") %>% 
      # drop Marine Energy since includes all
      filter(tag != "Marine Energy") %>% 
      group_by(rowname) %>% 
      summarize(
        tags = paste(tag, collapse = "', '"),
        tags = glue("'{tags}'"))
    
    q_tags <- paste(
      glue("
        SELECT uri
        FROM tethys_pub_tags
        WHERE tag IN ({q$tags})"), collapse = "\n UNION \n")
    q_pubs <- glue(
      "
      SELECT DISTINCT q.uri, p.title FROM (\n{q_tags}\n) q 
      INNER JOIN (
        SELECT 
          uri,
          data -> 'title' ->> 0  AS title 
        FROM tethys_pubs) p ON q.uri = p.uri
      ORDER BY p.title")
    
    # TODO: spatial
    # doc_locs <- dbGetQuery(
    #   con, 
    #   "SELECT 
    #    uri, 
    #    CAST(data->'spatial'->'coordinates'->0->>0 AS DOUBLE PRECISION) AS lon,
    #    CAST(data->'spatial'->'coordinates'->1->>0 AS DOUBLE PRECISION) AS lat,
    #    data->'spatial'->'extent'->>0 AS extent
    #  FROM tethys_pubs
    #  WHERE data->'spatial'->'extent'->> 0 = 'point' -- 3,778 of 6,633 rows
    # ") %>% 
    #   tibble()
    # doc_locs
    
    res <- dbGetQuery(con, q_pubs)  %>% 
      tibble() %>% 
      mutate(
        title = str_trim(title))
      
    res %>%
      mutate(
        Title = map2_chr(
          title, uri,
          function(x, y)
            glue("<a href={y} target='_blank'>{x}</a>"))) %>%
      select(Title) %>%
      arrange(Title) %>%
      datatable(
        escape = F)
    
  })
  
# tblSpatial() ----
output$tblSpatial <- renderDT({

    req(vals$queries_lit)
    
    message("output$tblSpatial")
    
    if (is.null(crud()$finished)){
      aoi_wkt <- NULL
    } else {
      aoi_wkt <- crud()$finished %>% pull(geometry) %>% st_as_text()
    }
    
    if (nrow(vals$queries_lit) == 0 || is.null(aoi_wkt)){
      dt_empty <- tibble(
        message = "Please Configure Tags and Locations to see results here") %>%
        datatable(rownames = F, options = list(dom = 't'))
      
      return(dt_empty)
    }
    
    # receptors <- vals$queries_lit %>% 
    #   pull(Receptors) %>% 
    #   unique() %>% 
    #   sort()
    
    spatial_receptors <- vals$queries_lit %>% 
      mutate(
        q = pmap(., function(Receptors, ...){
          keys <- c(Receptors) %>% 
            str_replace_all('"', '') %>%
            na_if("") %>% 
            na.omit()
          paste(keys, collapse = " AND ") })) %>% 
      pull(q) %>% 
      as.character()
    
    # receptors = c("Marine Mammals", "Fish")
    # aoi_wkt = "POLYGON ((-122.6833 32.35398, -122.6833 35.31737, -116.1166 35.31737, -116.1166 32.35398, -122.6833 32.35398))"
    # 
    # datasets <- tbl(con, "datasets") %>% 
    #   collect() %>%
    #   filter(ready) %>% 
    #   replace_na(list(buffer_km = 0)) %>% 
    #   select(-notes, -issues) %>% 
    #   separate_rows(tags, sep = ";") %>% 
    #   rename(tag = tags) %>% 
    #   mutate(
    #     tag = str_trim(tag)) %>% 
    #   filter(
    #     tag %in% receptors) %>% 
    #   arrange(tag, title) %>% 
    #   mutate(
    #     data      = map(
    #       code, 
    #       tabulate_dataset_shp_within_aoi, 
    #       aoi_wkt = aoi_wkt, output = "tibble"),
    #     data_nrow = map_int(data, nrow),
    #     Title     = map2_chr(
    #       title, src_url,
    #       function(x, y)
    #         glue("<a href={y} target='_blank'>{x}</a>")),
    #     Title     = ifelse(
    #       buffer_nm > 0,
    #       glue("{Title} [within {buffer_nm} nm of Location]"),
    #       Title)) %>% 
    #   select(
    #     Title,
    #     `Rows of Results` = data_nrow) %>% 
    #   arrange(Title)
    
    # spatial_receptors = c("Marine Mammals", "Fish")
    # aoi_wkt = "POLYGON ((-122.6833 32.35398, -122.6833 35.31737, -116.1166 35.31737, -116.1166 32.35398, -122.6833 32.35398))"
    #
    #browser()
    datasets <- tbl(con, "datasets") %>% 
      collect() %>%
      filter(ready) %>% 
      replace_na(list(buffer_km = 0)) %>% 
      select(-notes, -issues) %>% 
      separate_rows(tags, sep = ";") %>% 
      rename(tag = tags) %>% 
      mutate(
        tag = str_trim(tag)) %>% 
      filter(
        tag %in% spatial_receptors) %>% 
      arrange(tag, title) %>% 
      mutate(
        data      = map(
          code, 
          tabulate_dataset_shp_within_aoi, 
          aoi_wkt = aoi_wkt, output = "tibble"),
    # datasets1 <- datasets
    # datasets2 <- datasets1 %>% 
    #   mutate(
        data_nrow = map_int(data, nrow),
        Title     = map2_chr(
          title, src_url,
          function(x, y)
            glue("<a href={y} target='_blank'>{x}</a>")),
        Title     = ifelse(
          buffer_nm > 0,
          glue("{Title} [within {buffer_nm} nm of Location]"),
          Title)) %>% 
      select(
        Title,
        `Rows of Results` = data_nrow) %>% 
      arrange(Title)
    
    # TODO: nest spatial dataset results as sub-tables
    #   https://stackoverflow.com/questions/55058126/multiple-child-tables-in-dt-datatable#answer-56486534
    datatable(datasets, escape = F)
    
  })
  
  
  output$messageMenu <- renderMenu({
    # # Code to generate each of the messageItems here, in a list. This assumes
    # # that messageData is a data frame with two columns, 'from' and 'message'.
    # msgs <- apply(messageData, 1, function(row) {
    #   messageItem(from = row[["from"]], message = row[["message"]])
    # })
    # 
    # # This is equivalent to calling:
    # #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    # dropdownMenu(type = "messages", .list = msgs)
    
    # dropdownMenu(
    #   type = "messages",
    #   messageItem(
    #     from = "Sales Dept",
    #     message = "Sales are steady this month."
    #   ),
    #   messageItem(
    #     from = "New User",
    #     message = "How do I register?",
    #     icon = icon("question"),
    #     time = "13:45"
    #   ),
    #   messageItem(
    #     from = "Support",
    #     message = "The new server is ready.",
    #     icon = icon("life-ring"),
    #     time = "2014-12-01"
    #   ))
    
  })
})
