shinyServer(function(input, output, session) {

  set_logging_session()
  w <- Waiter$new()
    
  crud <- callModule(
    editMod, "mapEdit", map_default, "ply",
    editorOptions = list(
      polylineOptions = F, markerOptions = F, circleMarkerOptions = F,
      singleFeature = T)) # , editor = "leafpm")

  vals <- reactiveValues(
    queries_lit = tibble(
      Receptors  = character(0),
      Stressors  = character(0),
      Technology = character(0))) 
  
  # rxLitQuery <- reactive({
  #   
  #   tibble(
  #     Receptors  = input$selReceptors %>% paste(collapse = " AND "),
  #     Stressors  = input$selStressors %>% paste(collapse = " AND "),
  #     Technology = input$selTech      %>% paste(collapse = " AND "))
  # 
  # })
  
  # output$txtCurrentQuery <- renderText({ 
  #   rxLitQuery()
  # })
  
  updateLitQueries <- function(technology, receptors, stressors){
    
    queries <- expand.grid(
      Receptors  = receptors, 
      Stressors  = stressors,
      Technology = technology,
      stringsAsFactors = F)
    
    # vals$queries_lit <- union(vals$queries_lit, queries)
    vals$queries_lit <- full_join(
      vals$queries_lit,
      queries,
      by = c("Receptors", "Stressors", "Technology")) %>% 
      distinct(Receptors, Stressors, Technology) %>% 
      arrange(Receptors, Stressors, Technology)
    
    message(glue("updateLitQueries() vals$queries_lit (n={nrow(vals$queries_lit)})"))
    
    #message(glue("vals$queries_lit (n={length(vals$queries_lit)}) pre-btnAddAllLitQueries: {paste(vals$queries_lit, collapse='\n\t')}"))
    #message(glue("pre-btnAddReceptorLitQueries: vals$queries_lit (n={length(vals$queries_lit)})"))
    
    # updateSelectInput(
    #   session, 
    #   "selLitQueries", 
    #   choices = vals$queries_lit, selected = NULL)
    # 
  }
  
  observeEvent(input$btnAddLitQuery, {
    
    # vals$queries_lit <- bind_rows(
    #   vals$queries_lit,
    #   rxLitQuery())
    
    updateLitQueries(
      technology = input$selTech,
      receptor   = input$selReceptors %>% paste(collapse = " AND "),
      stressor   = input$selStressors %>% paste(collapse = " AND "))
    
    
    # updateSelectInput(
    #   session, 
    #   "selLitQueries", 
    #   choices = vals$queries_lit, selected = NULL)
    
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
  
  
  output$tblLitQueries <- DT::renderDataTable({
    
    # if (length(vals$queries_lit) > 0){
    #   tbl <- tibble(
    #     q = vals$queries_lit) %>% 
    #     separate(
    #       q, 
    #       into = c("Receptor", "Stressor", "Technology"),
    #       sep  = "' AND '")
    # } else {
    #   tbl <- tibble(
    #     Receptor   = NULL,
    #     Stressor   = NULL,
    #     Technology = NULL)
    # }
    vals$queries_lit
  })
  
  
  observeEvent(input$btnRmLitQuery, {
    
    #browser()
    
    vals$queries_lit <- vals$queries_lit %>% 
      slice(-input$tblLitQueries_rows_selected)
    
    # updateSelectInput(
    #   session, 
    #   "selLitQueries", 
    #   choices = vals$queries_lit, selected = NULL)
    
  })
  
  observe({
    
    n <- length(vals$queries_lit)
    
    updateSelectInput(
      session, 
      "selLitQueries", 
      label = glue("Queries (n={n})"))
    
  })
  
  output$tblStressorReceptor = renderRHandsontable({
    if (!is.null(input$tblStressorReceptor)) {
      DF = hot_to_r(input$tblStressorReceptor)
    } else {
      DF = s_r
    }

    rhandsontable(DF) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE) })

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

        #withProgress(message = 'Generating report', value = 0.1, {
          # input_rmd <- here("report_template.Rmd"); out_fmt = "pdf_document"; file = here("report_template.pdf")
          # rmd_params = list(
          #   title = "demo",
          #   technology = "Wave",
          #   aoi_wkt = "POLYGON ((-121.2012 32.91649, -121.2012 34.8138, -116.4551 34.8138, -116.4551 32.91649, -121.2012 32.91649))",
          #   stressors = NULL,
          #   receptors = NULL,
          #   stressor_receptors = "Fish AND Displacement",
          #   lit_tethys = TRUE,
          #   lit_ferc = FALSE,
          #   spatial = TRUE,
          #   mgt_tethys = FALSE)
          
        waiter_show(html = waiting_screen, color = "black")
        
        render(input=input_rmd, output_format=out_fmt, output_file=file, params = rmd_params)
        
        waiter_hide()
        #})
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
  
  # TODO: reactives ----
  values = reactiveValues(saved_plots = list(), env_datewindow=NULL)
  
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
