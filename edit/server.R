shinyServer(function(input, output, session) {

  # PRJ DOCS PAGE ----
  
  # * dependent/conditional dropdowns: get new prj doc sec ----
  
  # observe prj selection, update doc choices
  observe({
    updateSelectizeInput(
      session, 
      "sel_prj_doc", 
      choices = d_prj_doc %>% 
        filter(prj == input$sel_prj) %>% 
        pull(doc),
      server = T)
  })
  # observe doc selection, update section choices
  observe({ 
    updateSelectizeInput(
      session, 
      "sel_prj_doc_sec",
      choices = d_prj_doc_sec %>% 
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
      choices = d_prj_doc_sec %>% 
        filter(
          prj == input$sel_prj,
          doc == input$sel_prj_doc) %>% 
        pull(url),
      server = T)
  })
  
  
  # * storing & updating dtedit input choices ----
  # TODO: make prj_doc_sec_choices reactive based on refresh_btn like prj_choices below:
  # below works!
  prj_doc_sec_choices <- reactiveVal(
    d_prj_doc_sec %>% pull(prj_doc_sec_display) %>% sort() %>% unique()
  )

  
  

           
    
    
    

      # choices <- list()
      # for (project in (d_prj_doc_sec$prj %>% na.omit() %>% unique())) { 
      #   choices <- append(
      #     prj_doc_sec_choices,
      #     setNames(
      #       list(
      #         ferc %>% 
      #           filter(project == !!project) %>% 
      #           # pull(prj_doc_sec) %>% 
      #           pull(prj_doc_sec_display) %>% 
      #           unlist()),
      #       project)) 
      # }
      # d_prj_doc_sec$prj_doc_sec_display %>% 
      #  sort() %>% unique()
  
  
  # for storage/updating project info data
  prj_values <- reactiveValues()
  # based on all projects & their assoc'd
  prj_values$data <- d_prj_doc_sec 

  
  # * prj table ----
  # display all existing prj names
  output$prj_table <- { DT::renderDT(
    prj_values$data %>% select(prj, doc, sec, url),

    class = 'compact row-border hover order-column',
    colnames = c(
      "Project", "Project Doc", 
      "Project Doc Section", "Project Doc Section URL"),
    extensions = "RowGroup",
    callback   = { JS("
      table.on('click', 'tr.dtrg-group', function () {
        var rowsCollapse = $(this).nextUntil('.dtrg-group');
        $(rowsCollapse).toggleClass('hidden');
      });") },
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
        list(targets = 1, className = "dt-right cell-border-right"),
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
  }
  
  

  
  # EDIT FERC DOCS PAGE ----
  #* dtedit() object ----
  fercdt <- { dtedit(
    input, output,
    name            = 'ferc_dt_edit',
    thedata         = get_ferc(),
    view.cols       = labels %>% filter(!is.na(view_label)) %>% pull(fld),
    edit.cols       = labels %>% filter(!is.na(edit_label)) %>% pull(fld),
    edit.label.cols = labels %>% filter(!is.na(edit_label)) %>% pull(edit_label),
    delete.info.label.cols = labels %>% filter(!is.na(view_label)) %>% pull(view_label),
    
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
      prj.doc.sec.choices.list = prj_doc_sec_choices
      # , prj.doc.sec.choices.list = prj_doc_sec_choices
    ),
    selectize = T, 
    selectize.options = list(
      prj_doc_sec_display = list(
        create = F,
        # options run correctly, selected item
        # TODO: fix so that selected item (`item`) is also formatted properly
        # looks like an issue in the way the options are stored...
        render = I("{
          option: function(data, escape) {
            return '<div>' + data.label + '</div>';
          },
          item: function(data, escape) {
            return '<div>' + data.label + '</div>';
          }
        }")
        # render = I("{
        #   option: function(data, escape) {
        #     return '<div>' + data.label + '</div>';
        #   },
        #   item: function(data, escape) {
        #     return '<div>' + data.label + </div>; 
        #   }
        # }")
        
      )),
      
      # project = list(
      #   create = T, persist = T, maxItems = 1, selectize = T)),
      # prj_doc_sec_display = list(
      #   render = 
        
        
#         I("{
#           option: function(data, escape) {
# 						return '<div><strong>' + item.name + '</strong></div>';
# 					},
# 					item: function(data, escape) {
# 						return '<div>' + data.name + '</div>';
# 					}
#         }"))
#     ),
    
    datatable.rownames = F,
    
    datatable.call = function(...) {
      arguments <- list(...)
      arguments$escape   <- 0
      arguments$class    <- 'display'
      arguments$colnames <- labels %>%
        filter(!is.na(view_label)) %>% pull(view_label)
      do.call(DT::datatable, arguments) %>%
        DT::formatStyle('document',  fontWeight = 'bold') # bold document name
    },
    
    # * --> datatable.options ----
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
    ), # end options

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
  ) }
  
  # observe({
  #   # browser()
  #   dt_data(isolate(as.data.frame(fercdt$thedata, stringsasfactors = FALSE)))
  #   print(isolate(dt_data()))
  #   print(paste("Edit count:", fercdt$edit.count)) 
  #   # only reacts to change in $edit.count
  # })

  
  # observe prj docs page updates in ferc docs page ----
  observeEvent(input$save_sel, {
    # browser()
    
    # [ insert line to make sure no null inputs ]
    
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
    
    # update data with new row
    isolate(
      prj_values$data <- prj_values$data %>% 
        bind_rows(new_input) %>% 
        unique()
    )
    
    # tbl of all prj data, including new inputs
    d_prj_doc_sec <<- prj_values$data
    dbWriteTable(
      con, "ferc_project_doc_sec", 
      d_prj_doc_sec %>% 
        select(-prj_doc_sec), 
      overwrite = T)
    
    # modal with buttons to (1) add another prj doc or (2) return to ferc docs
    prj_input_modal <- modalDialog(
      title = "Success!",
      HTML(paste(
        "<strong>You have added the following input choices:</strong><br>", 
        new_input$prj_doc_sec_display)), 
        # paste(new_input, collapse = "<br>"))), 
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
    # updateSelectizeInput(
    #   session, "sel_prj",
    #   selected = NULL
    #   # choices = d_prj$project,
    #   # server = T, selected = character(0)
    # )
    # updateSelectizeInput(
    #   session, 
    #   "sel_prj_doc", 
    #   choices = d_prj_doc %>% 
    #     filter(prj == input$sel_prj) %>% 
    #     pull(prj_doc),
    #   server = T, selected = character(0))
    
    # prj_choices$prj <- prj_values_df$prj %>% unique()
    # prj_choices$prj_doc <- prj_values_df$prj_doc %>% unique()
    # prj_choices$prj_doc_sec <- prj_values_df$prj_doc_sec %>% unique()
    # prj_choices$prj_doc_url <- prj_values_df$prj_doc_sec_url %>% unique()
    # will update projects with this data via SQL 
    # so it can be displayed in ferc docs table on other tab
  })
  
  observeEvent(input$refresh_btn, {
    # browser()
    
    d_prj_doc_sec <- dbReadTable(con, "ferc_project_doc_sec") %>% 
      tibble() %>% collect()
    d_prj_doc <- d_prj_doc_sec %>% 
      group_by(prj, doc) %>% summarize() %>% ungroup()
    
    prj_doc_sec_choices(
      d_prj_doc_sec %>% pull(prj_doc_sec_display) %>% sort() %>% unique())
    
    # prj_doc_sec_choices(
    #   prj_values_df %>% 
    #     pull(prj_doc_sec_display) %>% unlist() %>% sort() %>% unique()
    # or read table d_prj_doc_sec again & do:
    # d_prj_doc_sec %>% pull(prj_doc_sec_display) %>% sort() %>% unique()
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
    

    
    
    
    

    # prj_doc_sec_choices(
    #   prj_values_df$prj_doc_sec %>% 
    #     sort() %>% unique())
    # prj_doc_choices(
    #   prj_values_df %>% 
    #     filter(prj %in% prj_choices()) %>% 
    #     pull(prj_doc) %>% 
    #     sort() %>% unique())
    # prj_doc_attach_choices(
    #   prj_values_df$prj_doc_sec %>% 
    #     sort() %>% unique())
    
  # observeEvent(input$refresh_btn, {
  #   prj_choices(
  #     prj_values_df$prj %>% 
  #       sort() %>% unique())
  #   prj_doc_choices(
  #     prj_values_df %>% 
  #       filter(prj %in% prj_choices()) %>% 
  #       pull(prj_doc) %>% 
  #       sort() %>% unique())
  #   prj_doc_attach_choices(
  #     prj_values_df$prj_doc_sec %>% 
  #       sort() %>% unique())
    
    
    # prj_doc_sec_choices(
    #   prj_values_df$prj_doc_
    # )
    
   