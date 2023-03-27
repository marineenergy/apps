shinyServer(function(input, output, session) {

  # reactiveVal(): ba_doc_files_rx, ba_projects_rx ----
  ba_doc_files_rx <- reactiveVal(
    tbl(con, "ba_docs") |> 
      pull(ba_doc_file) |> 
      sort())
  
  # dtedit() object ----
  badt <-  dtedit(
    input, output,
    name       = 'ba_dt_edit',
    thedata    = get_ba_doc_excerpts(gpt_version_init),
    view.cols       = labels %>% filter(!is.na(view_label)) %>% pull(fld),        # "ba_doc_file" "rowid"   "excerpt_html" "tag_html" 
    edit.cols       = labels %>% filter(!is.na(edit_label)) %>% pull(fld),        # "ba_doc_file" "excerpt" "tag_named"    "gpt_version" "ck_gpt"
    edit.label.cols = labels %>% filter(!is.na(edit_label)) %>% pull(edit_label), # "BA Document" "Excerpt" "Tags"         "GPT version" "Auto Tag (with OpenAI GPT)"
    delete.info.label.cols = labels %>% 
      filter(!is.na(view_label)) %>% pull(view_label),                            # "BA Document" "ID"      "Excerpt"      "Tags"
    input.types = c(
      ba_doc_file         = "selectizeInputReactive",
      excerpt             = "textAreaInput",
      tag_named           = "selectInputMultiple",
      gpt_version         = "selectInput",
      ck_gpt              = "checkboxInput"),
    input.choices = list(
      ba_doc_file         = 'ba_doc_file_list',
      tag_named           = tag_choices,
      gpt_version         = gpt_versions), 
    input.choices.reactive = list(
      ba_doc_file_list = ba_doc_files_rx),
    selectize = T, 
    datatable.rownames = F,
    datatable.call = function(...) {
      arguments <- list(...)
      arguments$escape   <- 1
      arguments$class    <- 'display'
      arguments$colnames <- labels %>%
        filter(!is.na(view_label)) %>% pull(view_label)
      # arguments$extensions <- "Buttons"
      
      do.call(DT::datatable, arguments) %>%
        DT::formatStyle(c("excerpt_html", "tag_html"), fontSize = "13px")
      # Warning: 
      #   Error in name2int: You specified the columns: excerpt, tag_html, 
      #   but the column names of the data are ba_doc_file, rowid, excerpt_html, tag_html
      # datatable.call [/share/github/apps_dev/edit-ba/server.R#41]
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
  
  # refresh_btn ----
  observeEvent(input$refresh_btn, {
    # update_dtedit_page()  # in global.R
    # browser()
    
    ck_con()
    
    # update tags from Google Sheet to db + csv + lookup
    update_tags()
    tags        <-  get_tags() 
    tag_choices <<- get_tag_choices(tags)
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
  
})
    
   