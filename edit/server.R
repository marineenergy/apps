shinyServer(function(input, output, session) {
  
  # PROJECT DOCS PAGE ----
  
  # * observe prj selection ----
  # update doc choices
  observe({
    updateSelectizeInput(
      session, 
      "sel_prj_doc", 
      choices = d_prj_doc %>% 
        filter(prj == input$sel_prj) %>% 
        pull(prj_doc),
      server = T)
  })
  # * observe doc selection ----
  # update section choices
  observe({ 
    updateSelectizeInput(
      session, 
      "sel_prj_doc_sec",
      choices = d_prj_doc_sec %>% 
        filter(
          prj     == input$sel_prj,
          prj_doc == input$sel_prj_doc) %>% 
        pull(prj_doc_sec),
      server = T)
  })
  # * observe section selection ----
  # update url choices
  observe({ 
    updateSelectizeInput(
      session, 
      "sel_prj_doc_sec_url",
      choices = d_prj_doc_sec %>% 
        filter(
          prj     == input$sel_prj,
          prj_doc == input$sel_prj_doc) %>% 
        pull(prj_doc_sec_url),
      server = T)
  })
  
  
  # STORING & UPDATING INPUT CHOICES ----
  
  # prjs        <- d_prj$project 
  prj_choices <- reactiveVal(
    d_prj_doc_sec$prj %>% 
      sort() %>% unique())
  # prj_doc_sec_choices <- reactiveVal(
  #  
  # )
  
  prj_doc_choices <- reactiveVal(
    d_prj_doc_sec$prj_doc %>% 
      sort() %>% unique())
  
  prj_doc_attach_choices <- reactiveVal(
    d_prj_doc_sec$prj_doc_sec %>% 
      sort() %>% unique())
  
  #old: works for storage/updating
  prj_values <- reactiveValues()
  prj_values$data <- d_prj_doc_sec 
  
  # display all existing prj names
  output$prj_table <- DT::renderDT(
    prj_values$data,
    class = 'compact row-border hover order-column',
    colnames = c(
      "Project", "Project Doc", 
      "Project Doc Section", "Project Doc Section URL"),
    extensions = "RowGroup",
    callback   = JS("
      table.on('click', 'tr.dtrg-group', function () {
        var rowsCollapse = $(this).nextUntil('.dtrg-group');
        $(rowsCollapse).toggleClass('hidden');
      });"),
    selection  = "none",
    options = list(
      pageLength = -1,
      rowGroup  = list(
        dataSrc = 1,
        startRender = JS("
          function(rows, group) {
            var style = 
              'background-color: #337AB7; color: white;'
            var td = `<td style='${style}' colspan=12>${group}</td>`;
            return $(`<tr>${td}</tr>`);
          }")),
      scrollX    = T, scrollCollapse = T,
      autowidth  = T,
      columnDefs = list(
        # rowid hidden
        list(targets = 0, visible = F),
        # prj 
        list(targets = 1, className = "dt-right cell-border-right "),
        # prj doc, prj_doc_section
        list(targets = c(2,3), className = "dt-left"),
        # prj doc section url
        list(targets = 4, visible = F)
      ),
      
      # header: black background
      initComplete = JS("
        function(settings, json) {
          $(this.api().table().header()).css({
            'background-color': '#2b2d2f',
            'color': '#fff'
          })
        }"),
      searchHighlight = T,
      lengthMenu = list(c(5, 10, 25, 50, -1), 
                        c("5", "10", "25", "50", "All")))) 
  
  
  # ADD PRJ TABLE ----
  # * get data ----
  # ferc     <- get_ferc()
  # 
  # library(stringr)
  # 
  # match_prj <- function(d_projects, d_doc) {
  #   prj_match <- d_doc$prj_document %>% 
  #     str_extract(
  #       regex(
  #         projects$project %>% 
  #           str_replace_all("-", " ") %>% 
  #           str_sub(), 
  #         ignore_case = T, skip_word_none = T)) %>% 
  #     na.omit() %>% 
  #     unlist(recursive = T) %>% 
  #     first()
  #   if (is.na(prj_match)) {
  #     prj_match <- d_doc$prj_document %>% 
  #       str_extract(
  #         regex(
  #           projects$project_alt_name %>% 
  #             str_replace_all("-", " ") %>% 
  #             str_sub(), 
  #           ignore_case = T, skip_word_none = T)) %>% 
  
  
  
  
  #  match_prj <- function(
  #   prj_doc, 
  #   prj_names     = projects$project, 
  #   alt_prj_names = projects$project_alt_name) {
  #   
  #     prj_match <- prj_doc %>% 
  #       str_extract(
  #         regex(
  #           prj_names %>% 
  #             str_replace_all("-", " ") %>% 
  #             str_sub(), 
  #           ignore_case = T)) %>% 
  #       na.omit() %>% 
  #       unlist(recursive = T) %>% 
  #       first()
  #     if (is.na(prj_match)) {
  #       prj_match <- prj_doc %>% 
  #         str_extract(
  #           regex(
  #             alt_prj_names %>% 
  #               str_replace_all("-", " ") %>% 
  #               str_sub(), 
  #             ignore_case = T)) %>% 
  #           na.omit() %>% 
  #           unlist(recursive = T) %>% 
  #           first()
  #     }
  #     prj_match
  #   }
  # 
  # projects <- update_projects() %>% 
  #   mutate(
  #     project_alt_name = case_when(
  #       project == "OPT Reedsport" ~ "REEDSPORT OPT",
  #       project == "Pacwave-N"     ~ "Pacwave North",
  #       project == "Pacwave-S"     ~ "Pacwave South",
  #       project == "RITE"          ~ "Roosevelt Island Tidal Energy")) %>% 
  #   select(project, project_alt_name)
  # 

    # TODO: 
    # - [x] replace `view_col_names` w/ view from above
    # - [x] view_label
    # - [x] edit
    # - [x] edit_label
      
    #   )
    # ) %>%
    # mutate( 
    #   # is column displayed on dtedit TABLE?
    #   view_col = ifelse(is.na(view_col_names), FALSE, TRUE),
    #   # names to display in dtedit EDIT interface
    #   edit_col_names = c(
    #     "ID", NA, NA,  "Project, document, section", 
    #     "Key interaction detail",  NA, "Tags", # tag_named
    #     NA,  NA, NA, NA,
    #     "Presented as potential interaction?",
    #     "Described from observations at the project site?",
    #     "Monitoring plan (MP)?",
    #     "Adaptive management plan (AMP)?",
    #     "Protection mitigation and enhancement (PME)?",
    #     "Best management practices (BMPs) applied?", NA, NA),
    #   # is column displayed in dtedit EDIT interface?
    #   edit_col = ifelse(
    #     ferc_col_names %in% c(
    #       "rowid", "document", "project", "prj_document", "prj_doc_attachment", 
    #       "prj_doc_attach_url", "tag_sql", "tag_html", "prj_doc_sec_display", "prj_doc_sec_values"), 
    #     FALSE, TRUE))
  
  
  # dt_data <- reactiveVal()
  # dt_data(data.frame(get_ferc()))
  
  
  #* dtedit() object ----
  fercdt <- dtedit(
    input, output,
    name            = 'ferc_dt_edit',
    thedata         = get_ferc(),
    view.cols       = labels %>% filter(!is.na(view_label)) %>% pull(fld),
    edit.cols       = labels %>% filter(!is.na(edit_label)) %>% pull(fld),
    edit.label.cols = labels %>% filter(!is.na(edit_label)) %>% pull(edit_label),
    input.types = c(
      # project     = "selectizeInputReactive",  
      prj_doc_sec_display = "selectizeInput", 
      # prj_doc_sec = "selectizeInputReactive",
      
      # prj_document       = "selectizeInputReactive",
      # prj_doc_attachment = "selectizeInputReactive",
      detail      = "textAreaInput",
      tag_named   = "selectInputMultiple",
      ck_ixn      = "checkboxInput",
      ck_obs      = "checkboxInput",
      ck_mp       = "checkboxInput",
      ck_amp      = "checkboxInput",
      ck_pme      = "checkboxInput",
      ck_bmps     = "checkboxInput"),
    input.choices = list(
      # TODO: add tab/link to app for editing projects and associated documents, attachments and urls
      project     = 'project.choices.list',
      prj_doc_sec_display =  prj_doc_sec_choices,
      # prj_doc_sec =  prj_doc_sec_choices,
      # prj_doc_sec = 'prj.doc.sec.choices.list',
      ck_ixn     = c(TRUE, FALSE),
      ck_obs     = c(TRUE, FALSE),
      ck_mp      = c(TRUE, FALSE), 
      ck_amp     = c(TRUE, FALSE),
      ck_pme     = c(TRUE, FALSE),
      ck_bmps    = c(TRUE, FALSE),
      tag_named  = tag_choices), 
    input.choices.reactive = list(
      project.choices.list = prj_choices
      # , prj.doc.sec.choices.list = prj_doc_sec_choices
    ),
    selectize = T, 
    selectize.options  = list(
      project = list(
        create = T, persist = T, maxItems = 1, selectize = T),
      prj_doc_sec_display = list(
        render = I("{
          option: function(data, escape) {
						return '<div><strong>' + item.name + '</strong></div>';
					},
					item: function(data, escape) {
						return '<div>' + data.name + '</div>';
					}
        }"))
    ),
    
    datatable.rownames = F,
    
    datatable.call = function(...) {
      arguments <- list(...)
      arguments$escape   <- 0
      arguments$class    <- 'display'
      arguments$colnames <- labels %>%
        filter(!is.na(view_label)) %>% pull(view_label)
      # arguments$colnames <- labels %>%
      #   filter(view_col == T) %>% pull(view_col_names)
      do.call(DT::datatable, arguments) %>%
        DT::formatStyle('document',  fontWeight = 'bold')
    },
    
    # datatable.options ----
    datatable.options = list(
      columnDefs = list(
        # centered rowid
        list(targets = c(0, 2, 6, 7, 8, 9, 10, 11),
             className = 'dt-center'),
        # in-row checkboxes
        list(targets = c(6, 7, 8, 9, 10, 11),
             render = JS(
               "function(data, type, row) {
                  if (data == true) {
                    data = '<div class=\"text-success\"><span class=\"glyphicon glyphicon-ok-circle\"></span></div>';
                  } else if (data == false) {
                    data = '<div class=\"text-danger\"><span class=\"glyphicon glyphicon-remove-circle\"></span></div>';
                  } return data}")),
        list(targets = 1,
             className = "dt-right cell-border-right")),
      # header: black background
      initComplete = JS("
        function(settings, json) {
          $(this.api().table().header()).css({
            'background-color': '#2b2d2f',
            'color': '#fff'
          })
        }"),
      searchHighlight = T,
      autowidth  = T,
      pageLength = 5,
      lengthMenu = list(c(5, 10, 25, 50, 100, -1), c(5, 10, 25, 50, 100, "All"))
    ),
    modal.size      = 'l', 
    text.width      = '100%',
    # textarea.width  = '200%',
    # textarea.height = '300px',
    # select.width    = '100%',
    
    icon.delete     = icon("trash"),
    icon.edit       = icon("edit"),
    icon.add        = icon("plus"),
    icon.copy       = icon("copy"),
    
    title.delete    = 'Delete',
    title.edit      = 'Edit',
    title.add       = 'Add new key interaction detail',
    
    text.delete.modal = HTML(
      "<span><b>Are you sure you want to delete this record?</b></span>"),
    
    # callbacks
    callback.update = ferc.update.callback,
    callback.insert = ferc.insert.callback,
    callback.delete = ferc.delete.callback
  )
  
  # observe({
  #   # browser()
  #   dt_data(isolate(as.data.frame(fercdt$thedata, stringsasfactors = FALSE)))
  #   print(isolate(dt_data()))
  #   print(paste("Edit count:", fercdt$edit.count)) 
  #   # only reacts to change in $edit.count
  # })
  # 
  # 
  
  
  
  
  
  
  
  
  # observe prj updates ----
  observeEvent(input$save_sel, {
    # browser()
    
    # create new row to be added from inputs
    new_input <- isolate(c(
      input$sel_prj,
      input$sel_prj_doc,
      input$sel_prj_doc_sec,
      input$sel_prj_doc_sec_url))
    # update data with new row
    isolate(
      prj_values$data <- rbind(
        as.matrix(prj_values$data), unlist(new_input)) %>% 
        unique())
    
    # tbl of all prj data, including new inputs
    prj_values_df <<- as.data.frame(prj_values$data) %>% tibble()
    
    # modal to add another prj doc or return to ferc docs page
    prj_input_modal <- modalDialog(
      title = "Success!",
      HTML(paste(
        "<strong>You have added the following input choices:</strong><br>", 
        paste(new_input, collapse = "<br>"))), 
      easyClose = TRUE,
      size = "m",
      footer = fluidRow(
        tagList(
          column(
            width = 5,
            modalButton(
              "Add more input choices")),
          column(
            width = 6,
            actionButton(
              "return_to_ferc_docs", 
              "Return to FERC docs table", 
              class   = "btn btn-primary",
              onclick = "customHref('docs')")))))
    showModal(prj_input_modal)
    observeEvent(input$return_to_ferc_docs, {
      removeModal()
      
      # not sure if the following line works, but
      # goal is to trigger refresh_btn by clicking return_to_ferc_docs
      # input$refresh_btn
    })
    
    # clear selections
    updateSelectizeInput(
      session, "sel_prj",
      choices = d_prj$project,
      server = T, selected = character(0)
    )
    updateSelectizeInput(
      session, 
      "sel_prj_doc", 
      choices = d_prj_doc %>% 
        filter(prj == input$sel_prj) %>% 
        pull(prj_doc),
      server = T, selected = character(0))
    
    # prj_choices$prj <- prj_values_df$prj %>% unique()
    # prj_choices$prj_doc <- prj_values_df$prj_doc %>% unique()
    # prj_choices$prj_doc_sec <- prj_values_df$prj_doc_sec %>% unique()
    # prj_choices$prj_doc_url <- prj_values_df$prj_doc_sec_url %>% unique()
    # will update projects with this data via SQL 
    # so it can be displayed in ferc docs table on other tab
  })
  
  observeEvent(input$refresh_btn, {
    prj_choices(
      prj_values_df$prj %>% 
        sort() %>% unique())
    prj_doc_choices(
      prj_values_df %>% 
        filter(prj %in% prj_choices()) %>% 
        pull(prj_doc) %>% 
        sort() %>% unique())
    prj_doc_attach_choices(
      prj_values_df$prj_doc_sec %>% 
        sort() %>% unique())
    
    
    # prj_doc_sec_choices(
    #   prj_values_df$prj_doc_
    # )
    
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
  data_list <- list() 
  observeEvent(fercdt$thedata, {
    data_list[[length(data_list) + 1]] <<- fercdt$thedata
  })
  
  shiny::exportTestValues(data_list = {data_list})
  
  cancel.onsessionEnded <- session$onSessionEnded(function() {
    pool::poolClose(con)
    DBI::dbDisconnect(conn)
  })
})