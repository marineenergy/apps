shinyServer(function(input, output, session) {

  set_logging_session()
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

  updateLitQueries <- function(technology, receptors, stressors){
    
    queries <- expand.grid(
      Receptors  = receptors, 
      Stressors  = stressors,
      Technology = technology,
      stringsAsFactors = F)
    
    vals$queries_lit <- full_join(
      vals$queries_lit,
      queries,
      by = c("Receptors", "Stressors", "Technology")) %>% 
      distinct(Receptors, Stressors, Technology) %>% 
      arrange(Receptors, Stressors, Technology)
    
  }
  
  # btnAddLitQ* ----
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
  output$tblLitQueries <- DT::renderDataTable({

    vals$queries_lit
  })
  
  # btnRmLitQuery ----
  observeEvent(input$btnRmLitQuery, {
    
    vals$queries_lit <- vals$queries_lit %>% 
      slice(-input$tblLitQueries_rows_selected)

  })

  get_rmd_params <- function(){
    
    if (is.null(crud()$finished)){
      aoi_wkt <- NULL
    } else {
      aoi_wkt <- crud()$finished %>% pull(geometry) %>% st_as_text()
    }
    
    if (is.null(input$tblStressorReceptor)) {
      stressor_receptors = NULL
    } else {
      stressor_receptors <- hot_to_r(input$tblStressorReceptor) %>%
        tidyr::pivot_longer(-Receptor, "Stressor") %>%
        filter(value == T) %>%
        mutate(
          str_and = glue("{str_trim(Stressor)} AND {str_trim(Receptor)}")) %>%
        pull(str_and)
      if (length(stressor_receptors) == 0)
        stressor_receptors <- NULL
      stressor_receptors
    }
    
    rmd_params <- list(
      title              = input$txtTitle,
      technology         = input$selTech,
      aoi_wkt            = aoi_wkt,
      lit_tags           = 
      stressors          = input$selStressors,
      receptors          = input$selReceptors,
      stressor_receptors = stressor_receptors,
      lit_tethys         = input$ckboxLitTethys,
      lit_ferc           = input$ckboxLitFERC,
      spatial            = input$ckboxSpatialReceptors,
      mgt_tethys         = input$ckboxMgtTethys)
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
        
        input_rmd <- here("report_template.Rmd")
        
        rmd_params <- get_rmd_params()
        # render(tmp_rmd, output_format=out_fmt, output_file=file, params = list(url=url))
        # saveRDS(rmd_params, here("data/tmp_rmd_params.rds"))
        # rmd_params <- readRDS(here("data/tmp_rmd_params.rds"))
        # file <- here("data/tmp_rmd_params_report.html")
        
        log_event("download_report")
        log_output(list(input_rmd=input_rmd, out_fmt=out_fmt, file=file) %>% as.yaml())
        log_output(rmd_params %>% as.yaml())

        waiter_show(html = waiting_screen, color = "black")
        
        render(input=input_rmd, output_format=out_fmt, output_file=file, params = rmd_params)
        
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
  
})
