shinyServer(function(input, output, session) {

  # reactiveVal(): ba_doc_files_rx, ba_projects_rx ----
  ba_doc_files_rx <- reactiveVal(
    tbl(con, "ba_docs") |> 
      pull(ba_doc_file) |> 
      sort())
  
  # EDIT BA DOCS PAGE ----
  #* dtedit() object ----
  badt <-  dtedit(
    input, output,
    name            = 'ba_dt_edit',
    thedata         = get_ba_doc_excerpts(),
    view.cols       = labels %>% filter(!is.na(view_label)) %>% pull(fld),
    edit.cols       = labels %>% filter(!is.na(edit_label)) %>% pull(fld),
    edit.label.cols = labels %>% filter(!is.na(edit_label)) %>% pull(edit_label),
    delete.info.label.cols = labels %>% 
      filter(!is.na(view_label)) %>% pull(view_label),
    input.types = c(
      ba_doc_file         = "selectizeInputReactive",
      excerpt             = "textAreaInput",
      tag_named           = "selectInputMultiple"),
    input.choices = list(
      ba_doc_file         = 'ba_doc_file_list',
      tag_named           = tag_choices), 
    input.choices.reactive = list(
      ba_doc_file_list = ba_doc_files_rx),
    selectize = T, 
    datatable.rownames = F,
    datatable.call = function(...) {
      arguments <- list(...)
      arguments$escape   <- 0
      arguments$class    <- 'display'
      arguments$colnames <- labels %>%
        filter(!is.na(view_label)) %>% pull(view_label)
      # arguments$extensions <- "Buttons"
      
      do.call(DT::datatable, arguments) %>%
        DT::formatStyle(c("excerpt", "tag_html"), fontSize = "13px")
    },
    # * --> datatable.options ----
    datatable.options = list(
      columnDefs = list(
        # list(targets = 0, className = 'dt-right'), # original: ID
        list(targets = 0, className = 'dt-left') # Document
        # list(targets = 3, className = "dt-left cell-border-right"), # Section
        ),
      # dom = "Bfrtip", buttons = "Sort by newest record",
      # header: black background
      initComplete = JS("
        function(settings, json) {
          $(this.api().table().header()).css({
            'background-color': '#2b2d2f',
            'color': '#fff'
          })
        }"),
      searchHighlight = T,
      order           = list(list(1, 'asc')),
      autowidth       = T,
      pageLength      = 5,
      lengthMenu      = list(c(5, 10, 25, 50, 100, -1), c(5, 10, 25, 50, 100, "All"))
    ), # end options
    
    modal.size      = 'l', 
    text.width      = '100%',
    icon.delete     = icon("trash"),
    icon.edit       = icon("edit"),
    icon.add        = icon("plus"),
    icon.copy       = icon("copy"),
    title.delete    = 'Delete',
    title.edit      = 'Edit',
    title.add       = 'Add new excerpt',
    
    text.delete.modal = HTML(
      "<span><b>Are you sure you want to delete this excerpt?</b></span>"),
    
    # callbacks
    callback.update = ba.update.callback,
    callback.insert = ba.insert.callback,
    callback.delete = ba.delete.callback
  )
  
  
  # observe prj docs page updates in ba docs page ----
  observeEvent(input$save_sel, {
    # browser()
    
    # [ should insert line to make sure no null inputs ]
    
    # new row of prj_doc_sec
    isolate(
      new_input <- tibble(
        prj = input$sel_prj,
        doc = input$sel_prj_doc,
        sec = input$sel_prj_doc_sec,
        url = input$sel_prj_doc_sec_url,
        prj_doc_sec_display = as.character(glue("<h5><b>{input$sel_prj}</b></h5> {input$sel_prj_doc} {ifelse(!is.na(input$sel_prj_doc_sec), glue('<br><i>{input$sel_prj_doc_sec}</i>'), '')}")),
        prj_doc_sec_values = as.character(glue("{input$sel_prj};;{input$sel_prj_doc};;{input$sel_prj_doc_sec}")),
        prj_doc_sec = map2(prj_doc_sec_values, prj_doc_sec_display, setNames)
      ) 
    )
    
    # update reactive data with new row
    isolate(
      prj_values$data <- prj_values$data %>% 
        bind_rows(new_input) %>% 
        unique()
    )
    
    # update prj_doc_sec_lookup 
    prj_doc_sec_lookup <<- prj_values$data
    # update table in DB
    dbWriteTable(
      con, "ba_project_doc_sec", 
      prj_doc_sec_lookup %>% 
        select(-prj_doc_sec), 
      overwrite = T)
    
    prj_doc_sec_lookup <<- dbReadTable(con, "ba_project_doc_sec") %>% 
      tibble() %>% collect()
    
    # modal with buttons to (1) add another prj doc or (2) return to ba docs
    prj_input_modal <- {modalDialog(
      title = "Success!",
      HTML(paste(
        "<strong>You have added the following input choices:</strong><br>", 
        new_input$prj_doc_sec_display)), 
      easyClose = TRUE,
      size = "m",
      footer = fluidRow(
        tagList(
          column(
            width = 5,
            # ADD MORE
            modalButton("Add more input choices")),
          column(
            width = 6,
            # RETURN TO BA DOCS
            actionButton(
              "return_to_ba_docs", 
              "Return to BA docs table", 
              class   = "btn btn-primary",
              onclick = "customHref('docs')")))))}
    showModal(prj_input_modal)
    
    observeEvent(input$return_to_ba_docs, {
      removeModal()
      # update_dtedit_page() # in global.R
    })
  })
  
  # refresh_btn ----
  observeEvent(input$refresh_btn, {
    # update_dtedit_page()  # in global.R
    # browser()
    
    update_ba()
    
    ba_doc_files_rx(
      tbl(con, "ba_docs") |> 
        pull(ba_doc_file))

    showModal(
      modalDialog(
        "Input choices have been refreshed from those added on project docs page.",
        footer    = modalButton("Continue"),
        easyClose = TRUE,
        size      = "s",
        fade      = TRUE,
        style     = "font-weight: bold;"))
  })
  
  # update data according to dtedit data ----
  # data_list <- list() 
  # https://rpubs.com/DavidFong/DTedit#custom-icons-for-neweditdeletecopy-buttons
  # data_list <- list()
  # observeEvent(badt$thedata, {
  #   # browser()
  #   data_list[[length(data_list) + 1]] <<- badt$thedata
  # })
  # 
  # shiny::exportTestValues(data_list = {data_list})
  
  # cancel.onsessionEnded <- session$onSessionEnded(function() {
  #   pool::poolClose(con)
  #   connections::connection_close(conn)
  # })
})
    
   