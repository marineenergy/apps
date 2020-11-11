shinyServer(function(input, output, session) {
    
    crud <- callModule(editMod, "mapEdit", map_default, "ply") # , editor = "leafpm")
    
    output$tblStressorReceptor = renderDataTable(
      s_r)
    
    fname = tempfile(fileext = ".csv")
    
    observe({
      # remove button and isolate to update file automatically
      # after each table change
      input$saveBtn
      s_r_in = isolate(input$tblStressorReceptor)
      if (!is.null(s_r_in)) {
        write.csv(hot_to_r(input$tblStressorReceptor), fname)
        print(fname)
      }
    })
    
    output$tblStressorReceptor = renderRHandsontable({
      if (!is.null(input$tblStressorReceptor)) {
        DF = hot_to_r(input$tblStressorReceptor)
      } else {
        DF = s_r
      }
      
      rhandsontable(DF) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
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
          tidyr::pivot_longer(-Stressor, "Receptor") %>% 
          filter(value == T) %>% 
          mutate(
            str_and = glue("{str_trim(Stressor)} AND {str_trim(Receptor)}")) %>% 
          pull(str_and)
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
          
          url = bkmark(session)
          #plots = values$saved_plots
          
          rmd <- here("report_template.Rmd")
          
          rmd_params <- get_rmd_params()
          #render(tmp_rmd, output_format=out_fmt, output_file=file, params = list(url=url))
          # saveRDS(rmd_params, here("data/tmp_rmd_params.rds"))
          # rmd_params <- readRDS(here("data/tmp_rmd_params.rds"))
          #browser()
          # file <- here("data/tmp_rmd_params_report.html")
          render(rmd, output_format=out_fmt, output_file=file, params = rmd_params)})
    }
    
    # out btn_download_* ----
    output$btn_download_pdf = download_report('pdf_document' ,'pdf')
    output$btn_download_doc = download_report('word_document','docx')
    output$btn_download_htm = download_report('html_document','html')
    
    # observe btn_download_url ----
    observeEvent(input$btn_download_url, {
      url = bkmark(session)
      showModal(urlModal(url, title='Bookmarked application link'))
      #browseURL(url)
    })
    
    # save: bkmark() ----
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
    
    # save: reactives ----
    values = reactiveValues(saved_plots = list(), env_datewindow=NULL)
    
    # save: onRestored ----
    onRestored(function(state) {
      cat("Restoring from state bookmarked at", state$values$saved_time, "\n", file=stderr())
      
      #values$saved_plots <- state$values$saved_plots
      
      #load_plot(plot_titles()[1])
    })
    
    
    
})
