shinyServer(function(input, output, session) {

  set_logging_session()
  w <- Waiter$new()
    
  crud <- callModule(
    editMod, "mapEdit", map_default, "ply",
    editorOptions = list(
      polylineOptions = F, markerOptions = F, circleMarkerOptions = F,
      singleFeature = T)) # , editor = "leafpm")

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
        tidyr::pivot_longer(-Stressor, "Receptor") %>%
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
