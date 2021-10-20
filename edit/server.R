shinyServer(function(input, output, session) {

  # PRJ DOCS PAGE ----
  
  # * dependent/conditional dropdowns: get new prj doc sec ----
  
  # observe prj selection, update doc choices
  observe({
    updateSelectizeInput(
      session, 
      "sel_prj_doc", 
      choices = prj_doc_sec_lookup %>% 
        filter(prj == input$sel_prj) %>% 
        pull(doc),
      server = T)
  })
  # observe doc selection, update section choices
  observe({ 
    updateSelectizeInput(
      session, 
      "sel_prj_doc_sec",
      choices = prj_doc_sec_lookup %>% 
        filter(
          prj == input$sel_prj,
          doc == input$sel_prj_doc) %>% 
        pull(sec),
      server = T)
  })
  # observe section selection, update url choices
  observe({ 
    updateSelectizeInput(
      session, 
      "sel_prj_doc_sec_url",
      choices = prj_doc_sec_lookup %>% 
        filter(
          prj == input$sel_prj,
          doc == input$sel_prj_doc) %>% 
        pull(url),
      server = T)
  })
  
  
  # * storing & updating dtedit input choices ----
  # baseline input choices for editing prj_doc_sec
  prj_doc_sec_choices <- reactiveVal(
    prj_doc_sec_lookup %>% pull(prj_doc_sec_display) %>% sort() %>% unique())
  # for storing & updating prj_doc_sec_lookup; displayed in DT in right column
  prj_values <- reactiveValues()
  prj_values$data <- prj_doc_sec_lookup 

  # * prj table ----
  # display all existing prj names
  output$prj_table <- { DT::renderDT(
    prj_values$data %>%     # based on reactive data
      select(prj, doc, sec, url) %>%
      arrange(prj),
    class = 'compact row-border hover order-column',
    colnames = c(
      "Project", "Project Doc", "Project Doc Section", 
      "Project Doc Section URL"),
    extensions = "RowGroup",  # grouped by prj
    callback   = { JS("
      table.on('click', 'tr.dtrg-group', function () {
        var rowsCollapse = $(this).nextUntil('.dtrg-group');
        $(rowsCollapse).toggleClass('hidden');
      });") },       # can collapse by rowgroup
    selection  = "none",
    options = list(
      pageLength = -1,
      rowGroup  = list(
        dataSrc = 1,
        startRender = {JS("
          function(rows, group) {
            var style = 
              'background-color: #337AB7; color: white;'
            var td = `<td style='${style}' colspan=12>${group}</td>`;
            return $(`<tr>${td}</tr>`);
          }")}
      ),
      scrollX    = T, scrollCollapse = T,
      autowidth  = T,
      columnDefs = list(
        # prj
        list(targets = 1, className = "dt-right cell-border-right"),
        # prj doc, prj_doc_section
        list(targets = c(2, 3), className = "dt-left"),
        # hide url
        list(targets = 4, visible = F)
      ),
      # header: black background
      initComplete = JS({"
        function(settings, json) {
          $(this.api().table().header()).css({
            'background-color': '#2b2d2f',
            'color': '#fff'
          })
        }"}),
      searchHighlight = T,
      lengthMenu = list(c(5, 10, 25, 50, -1), c("5", "10", "25", "50", "All"))
    )
  )}
  

  
  # EDIT FERC DOCS PAGE ----
  #* dtedit() object ----
  fercdt <-  dtedit(
    input, output,
    name            = 'ferc_dt_edit',
    thedata         = get_ferc(),
    view.cols       = labels %>% filter(!is.na(view_label)) %>% pull(fld),
    edit.cols       = labels %>% filter(!is.na(edit_label)) %>% pull(fld),
    edit.label.cols = labels %>% filter(!is.na(edit_label)) %>% pull(edit_label),
    delete.info.label.cols = labels %>% 
      filter(!is.na(view_label)) %>% pull(view_label),
    input.types = c(
      prj_doc_sec_display = "selectizeInputReactive",
      detail              = "textAreaInput",
      tag_named           = "selectInputMultiple",
      ck_ixn              = "checkboxInput",
      ck_obs              = "checkboxInput",
      ck_mp               = "checkboxInput",
      ck_amp              = "checkboxInput",
      ck_pme              = "checkboxInput",
      ck_bmps             = "checkboxInput"),
    input.choices = list(
      prj_doc_sec_display = 'prj.doc.sec.choices.list',
      ck_ixn              = c(TRUE, FALSE),
      ck_obs              = c(TRUE, FALSE),
      ck_mp               = c(TRUE, FALSE), 
      ck_amp              = c(TRUE, FALSE),
      ck_pme              = c(TRUE, FALSE),
      ck_bmps             = c(TRUE, FALSE),
      tag_named           = tag_choices), 
    input.choices.reactive = list(
      prj.doc.sec.choices.list = prj_doc_sec_choices), # reactiveVal
    selectize = T, 
    selectize.options = list(
      prj_doc_sec_display = list(
        create = F,
        render = I("{
          option: function(data, escape) {
            return '<div>' + data.label + '</div>';
          },
          item: function(data, escape) {
            return '<div>' + data.label + '</div>';
          }
        }")
      )
    ),
    datatable.rownames = F,
    datatable.call = function(...) {
      arguments <- list(...)
      arguments$escape   <- 0
      arguments$class    <- 'display'
      arguments$colnames <- labels %>%
        filter(!is.na(view_label)) %>% pull(view_label)
      # arguments$extensions <- "Buttons"
      
      do.call(DT::datatable, arguments) %>%
        DT::formatStyle("project", fontWeight = 'bold', fontSize = "16px") %>% 
        DT::formatStyle("prj_doc_attachment", fontStyle = 'italic') %>% 
        DT::formatStyle(c("detail", "tag_html"), fontSize = "13px")
    },
    # * --> datatable.options ----
    datatable.options = list(
      columnDefs = list(
        list(targets = 0, className = 'dt-right'),
        list(targets = 1, className = 'dt-center cell-border-right'),
        list(targets = 2, className = 'dt-left'),
        list(targets = 3, className = "dt-left cell-border-right"),
        list(targets = c(6, 7, 8, 9, 10, 11), className = 'dt-center'),
        # in-row checkboxes
        list(targets = c(6, 7, 8, 9, 10, 11),
             render = JS(
               "function(data, type, row) {
                  if (data == true) {
                    data = '<div class=\"text-success\"><span class=\"glyphicon glyphicon-ok-circle\"></span></div>';
                  } else if (data == false) {
                    data = '<div class=\"text-danger\"><span class=\"glyphicon glyphicon-remove-circle\"></span></div>';
                  } return data 
               }"))
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
      order      = list(list(1, 'asc')),
      autowidth  = T,
      pageLength = 5,
      lengthMenu = list(c(5, 10, 25, 50, 100, -1), c(5, 10, 25, 50, 100, "All"))
    ), # end options
    
    modal.size      = 'l', 
    text.width      = '100%',
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
  
  
  # observe prj docs page updates in ferc docs page ----
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
      con, "ferc_project_doc_sec", 
      prj_doc_sec_lookup %>% 
        select(-prj_doc_sec), 
      overwrite = T)
    
    prj_doc_sec_lookup <<- dbReadTable(con, "ferc_project_doc_sec") %>% 
      tibble() %>% collect()
    
    # modal with buttons to (1) add another prj doc or (2) return to ferc docs
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
            # RETURN TO FERC DOCS
            actionButton(
              "return_to_ferc_docs", 
              "Return to FERC docs table", 
              class   = "btn btn-primary",
              onclick = "customHref('docs')")))))}
    showModal(prj_input_modal)
    
    observeEvent(input$return_to_ferc_docs, {
      removeModal()
      # update_dtedit_page() # in global.R
    })
  })
  
  observeEvent(input$refresh_btn, {
    # update_dtedit_page()  # in global.R
    # browser()
    
    prj_doc_sec_lookup <<- dbReadTable(con, "ferc_project_doc_sec") %>%
      tibble() %>% collect()

    d_prj_doc <- prj_doc_sec_lookup %>%
      group_by(prj, doc) %>% summarize() %>% ungroup()

    prj_doc_sec_choices(
      prj_doc_sec_lookup %>% pull(prj_doc_sec_display) %>% sort() %>% unique())

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
  # observeEvent(fercdt$thedata, {
  #   # browser()
  #   data_list[[length(data_list) + 1]] <<- fercdt$thedata
  # })
  # 
  # shiny::exportTestValues(data_list = {data_list})
  
  # cancel.onsessionEnded <- session$onSessionEnded(function() {
  #   pool::poolClose(con)
  #   connections::connection_close(conn)
  # })
})
    
   