# DB CONNECTION & SCRIPTS ----
dir_scripts <<- here::here("scripts")
source(file.path(dir_scripts, "common_2.R"))
source(file.path(dir_scripts, "db.R"))
source(file.path(dir_scripts, "shiny_report.R"))
source(file.path(dir_scripts, "update.R"))

# LIBRARIES ----
# devtools::install_github("DavidPatShuiFong/DTedit@f1617e253d564bce9b2aa67e0662d4cf04d7931f")
shelf(DavidPatShuiFong/DTedit, DBI, DT, glue, purrr, readr)

# FXNS REFERENCED IN CALLBACKS ----

# convert data from dtedit to ferc_docs format  

get_new_docs <- function(d, flds = ferc_doc_names) {
  d %>% 
    select(-document, -tag_sql, -tag_named, -tag_html) %>% 
    relocate(flds)
}

# TODO: find home for creation of these in db, borrowing from prj_subpages_test.Rmd
d_prj_doc_sec   <- read_csv(here("data/project_doc_sec.csv"))
d_prj_doc       <- read_csv(here("data/project_doc.csv"))
d_prj           <- read_csv(here("data/project_sites.csv")) %>% arrange(project)


# convert data from dtedit to ferc_doc_tags format
get_new_tags <- function(d, flds = ferc_tag_names) {
  d %>% 
    select(-tag_html, -tag_sql) %>% 
    unnest(tag_named = tag_named) %>% 
    select(rowid, tag_named) %>% 
    left_join(
      tags %>% 
        rename(
          tag_category = category,
          content_tag  = tag_nocat) %>% 
        select(
          tag_category, 
          content_tag,
          tag_named,
          tag_sql) %>% 
        mutate(tag_named = as.character(tag_named)),
      by = "tag_named") %>% 
    mutate(content = "ferc_docs") %>% 
    select(-tag_named) %>% 
    relocate(flds)
}


# CALLBACK FXNS ----

# INSERT
ferc.insert.callback <- function(data, row) {
  d <- data %>% slice(row) %>% 
    na_if("NA") %>% na_if("") %>% 
    mutate(rowid = max(get_ferc()$rowid) + 1)
  d_docs <- get_new_docs(d)  # data to INSERT into ferc_docs
  d_tags <- get_new_tags(d)  # data to INSERT into ferc_doc_tags

  sql_insert_docs <- glue_data_sql(
    d_docs,
    "INSERT INTO ferc_docs VALUES
      ({rowid}, {detail}, {project},
      {prj_document}, {prj_doc_attachment}, {prj_doc_attach_url},
      {ck_ixn}, {ck_obs}, {ck_mp}, {ck_amp}, {ck_pme}, {ck_bmps})",
    .con = conn)
  res <- try(dbExecute(con, sql_insert_docs))
  if ("try-error" %in% class(res)) stop(res)
  
  DBI::dbAppendTable(conn, "ferc_doc_tags", d_tags)
  
  get_ferc()
}

# UPDATE
ferc.update.callback <- function(data, olddata, row) {
  d <- data %>% slice(row) %>% 
    tibble() %>% 
    mutate(
      across(starts_with("ck_"), as.logical)) %>% 
    na_if("NA") %>% 
    na_if("") 
  d_docs <- get_new_docs(d)  # data to UPDATE ferc_docs
  d_tags <- get_new_tags(d)  # data to be APPENDED to ferc_doc_tags
  
  sql_update_docs <- glue_data_sql(
    d_docs,
    "UPDATE ferc_docs 
      SET
        rowid              = {rowid}, 
        detail             = {detail}, 
        project            = {project},
        prj_document       = {prj_document}, 
        prj_doc_attachment = {prj_doc_attachment}, 
        prj_doc_attach_url = {prj_doc_attach_url},
        ck_ixn             = {ck_ixn}, 
        ck_obs             = {ck_obs}, 
        ck_mp              = {ck_mp}, 
        ck_amp             = {ck_amp}, 
        ck_pme             = {ck_pme}, 
        ck_bmps            = {ck_bmps}
      WHERE rowid = {rowid}",
    .con = conn)
  
  sql_delete_tags <- glue("
    DELETE FROM ferc_doc_tags WHERE rowid = {d$rowid};")
  
  res <- try(dbExecute(con, sql_update_docs))
  if ("try-error" %in% class(res)) stop(res)

  res <- try(dbExecute(con, sql_delete_tags))
  if ("try-error" %in% class(res)) stop(res)
  DBI::dbAppendTable(conn, "ferc_doc_tags", d_tags)
  
  get_ferc()
}
  
# DELETE
ferc.delete.callback <- function(data, row) {
  d <- data %>% slice(row) %>% na_if("NA") %>% na_if("")
  sql_delete_docs <- glue("DELETE FROM ferc_docs WHERE rowid = {d$rowid};")
  sql_delete_tags <- glue("DELETE FROM ferc_doc_tags WHERE rowid = {d$rowid}")
  
  res <- try(dbExecute(con, sql_delete_docs))
  if ("try-error" %in% class(res)) stop(res)
  
  res <- try(dbExecute(con, sql_delete_tags))
  if ("try-error" %in% class(res)) stop(res)
  
  get_ferc()
}

#* get additional data ----
ferc <- get_ferc() 
tags <- get_tags() 
ferc_doc_names <- dbReadTable(con, "ferc_docs") %>% names()
ferc_tag_names <- dbReadTable(con, "ferc_doc_tags") %>% names()

tag_choices <- list()
for (category in unique(tags$category)){ # category = tags$category[1]
  tag_choices <- append(
    tag_choices,
    setNames(
      list(
        tags %>% 
          filter(category == !!category) %>% 
          pull(tag_named) %>% 
          unlist()),
      category)) 
}

# SHINY SERVER ----
server <- function(input, output, session) {

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
  prj_choices <- reactiveVal(d_prj_doc_sec$prj %>% sort() %>% unique())

    
  #old: works for storage/updating
  prj_values <- reactiveValues()
  prj_values$data <- d_prj_doc_sec 
  
  # observeEvent(input$save_sel, {
  #   browser()
  # 
  #   # create new row to be added from inputs
  #   new_input <- isolate(c(
  #     input$sel_prj,
  #     input$sel_prj_doc,
  #     input$sel_prj_doc_sec,
  #     input$sel_prj_doc_sec_url))
  #   # update data with new row
  #   isolate(
  #     prj_values$data <- rbind(
  #       as.matrix(prj_values$data), unlist(new_input)) %>% 
  #       unique())
  #   
  #   prj_values_df <<- as.data.frame(prj_values$data) %>% tibble()
  #   # will update projects with this data via SQL 
  #   # so it can be displayed in ferc docs table on other tab
  # })
  # 
  
  # for debugging:
  output$prj_table <- renderTable(
    {prj_values$data},
    include.rownames = F)
  
  
  
  
  
  # observe({
  #   req(input$sel_prj)
  #   d_prj_input$prj <- data.frame(
  #     prj             = input$sel_prj,
  #     prj_doc         = input$sel_prj_doc,
  #     prj_doc_sec     = input$sel_prj_doc_sec,
  #     prj_doc_sec_url = input$sel_prj_doc_sec_url
  #   )
  # })
  # 
  # 
  
  
  # observeEvent(input$sel_prj, {
  #   d_prj_input$prj <- input$sel_prj})
  # observeEvent(input$sel_prj_doc, {
  #   d_prj_input$prj_doc <- input$sel_prj_doc})
  # observeEvent(input$sel_prj_doc_sec, {
  #   d_prj_input$prj_doc_sec <- input$sel_prj_doc_sec})
  # observeEvent(input$sel_prj_doc_sec_url, {
  #   d_prj_input$prj_doc_sec_url <- input$sel_prj_doc_sec_url})

    
    
    
    
  #   prj             <- input$sel_prj
  #   prj_doc         <- input$sel_prj_doc,
  #   prj_doc_sec     <- input$sel_prj_doc_sec,
  #   prj_doc_sec_url <- input$sel_prj_doc_sec_url
  # })
  # 

  # observeEvent(input$save_sel, {
  #   browser()
  #   # need(validate(input$sel_prj, input$sel_prj_doc, input$sel_prj_doc_sec, input$sel_prj_doc_sec_url))
  #   if (!is.null(input$sel_prj)) {
  #     d$prj <- input$sel_prj
  #   }
  #   # data <<- data.frame(
  #   #   prj             = input$sel_prj,
  #   #   prj_doc         = input$sel_prj_doc,
  #   #   prj_doc_sec     = input$sel_prj_doc_sec,
  #   #   prj_doc_sec_url = input$sel_prj_doc_sec_url)
  # 
  #   
  # })
  
  
# observeEvent(input$sel_prj_doc_url, {
#   showModal(modalDialog(
#     title = "Selection",
#     glue("Project: {input$sel_prj}, Project Doc: {input$sel_prj_doc}, Project Doc Section: {input$sel_prj_doc_sec}")
#   ))
# })
  
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
  #       na.omit() %>% 
  #       unlist(recursive = T) %>% 
  #       first()
  #   }
  #   prj_match
  # }
  
  # d_prjs = tbl of prjs & alt prj names
  # d_doc  = 1 row of tbl of prj_docs$prj_document
  
 
  
  
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
  # prj_docs <- ferc %>%
  #   select(prj_document, prj_doc_attachment, prj_doc_attach_url) %>% 
  #   distinct() %>% 
  #   mutate(project = map(prj_docs$prj_document, match_prj)) %>% 
  #   relocate(project) %>% 
  #   arrange(project)
  # 
  
  
  
  
  
  # DTEDIT ----
  #* get labels for dtedit ----
  labels <- 
    tibble(
      # actual names as stored in ferc, ferc_docs, and ferc_doc_tags
      ferc_col_names = names(ferc),
      # names to display on dtedit TABLE
      view_col_names = c(
        "ID", "Document", "Project", "Detail", NA, NA, "Tags", NA, 
        "Attachment", NA, "Ixn", "Obs", "MP?", "AMP?", "PME?", "BMPs?")) %>%
    mutate( 
      # is column displayed on dtedit TABLE?
      view_col = ifelse(is.na(view_col_names), FALSE, TRUE),
      # names to display in dtedit EDIT interface
      edit_col_names = c(
        "ID", NA, "Project",  "Key interaction detail",  NA, "Tags",
        NA,  "Document name", "Upload document attachment", 
        "Upload document URL link",
        "Presented as potential interaction?",
        "Described from observations at the project site?",
        "Monitoring plan (MP)?",
        "Adaptive management plan (AMP)?",
        "Protection mitigation and enhancement (PME)?",
        "Best management practices (BMPs) applied?"),
      # is column displayed in dtedit EDIT interface?
      edit_col = ifelse(
        ferc_col_names %in% c("rowid", "document", "tag_sql", "tag_html"), 
        FALSE, TRUE))
  

  #* dtedit() object ----
  fercdt <- dtedit(
    input, output,
    name      = 'ferc_dt_edit',
    thedata   = get_ferc(),
    view.cols = labels %>% filter(view_col == T) %>% pull(ferc_col_names),
    # previous version:
    # view.cols = names(
    #   ferc %>%
    #     select(-prj_document, -prj_doc_attach_url, -tag_sql, -tag_named)),
    edit.cols = c(
      "project", "prj_document", "prj_doc_attachment", "prj_doc_attach_url", 
      "detail", "tag_named", 
      "ck_ixn", "ck_obs", "ck_mp", "ck_amp", "ck_pme", "ck_bmps"),
    edit.label.cols = c(
      "Project: Select project below or type to add new project",
      'Document name',
      'Upload document attachment',
      'Upload document URL link',
      'Key interaction detail', 
      'Tags',
      'Presented as potential interaction?',
      'Described from observations at the project site?',
      'Monitoring plan (MP)?',
      'Adaptive management plan (AMP)?',
      'Protection mitigation and enhancement (PME)?',
      'Best management practices (BMPs) applied?'),
    input.types = c(
      # project            = "selectizeInput",    
      project            = "selectizeInputReactive",  
      prj_document       = "textAreaInput",
      prj_doc_attachment = "fileInput",
      prj_doc_attach_url = "textInput",
      detail             = "textAreaInput",
      tag_named          = "selectInputMultiple",
      ck_ixn             = "checkboxInput",
      ck_obs             = "checkboxInput",
      ck_mp              = "checkboxInput",
      ck_amp             = "checkboxInput",
      ck_pme             = "checkboxInput",
      ck_bmps            = "checkboxInput"),
    input.choices = list(
      # TODO: add tab/link to app for editing projects and associated documents, attachments and urls
      # project   = ferc$project %>% unique() %>% sort() %>% na.omit(),
      # project   = get_ferc()$project %>% unique() %>% sort() %>% na.omit(),
      project   = 'project.choices.list',
      ck_ixn    = c(TRUE, FALSE),
      ck_obs    = c(TRUE, FALSE),
      ck_mp     = c(TRUE, FALSE), 
      ck_amp    = c(TRUE, FALSE),
      ck_pme    = c(TRUE, FALSE),
      ck_bmps   = c(TRUE, FALSE),
      tag_named = tag_choices), 
    input.choices.reactive = list(project.choices.list = prj_choices),
    
    selectize = T, 
    selectize.options  = list(
      project = list(
        create   = T,
        maxItems = 1, 
        selectize = T
        # ideally would display project name if it exists & if null, then ""
        # onInitialize = I('
        #   function() {
        #     this.setValue("");
        #   }'),
        # placeholder  = "Select project below or type to add new project")
      )),
      
    datatable.rownames = F,
    
    datatable.call = function(...) {
      arguments <- list(...)
      arguments$escape   <- 0
      arguments$class    <- 'display'
      arguments$colnames <- labels %>%
        filter(view_col == T) %>% pull(view_col_names)
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
      autowidth       = T,
      pageLength      = 5,
      lengthMenu      = c(5, 10, 25, 50, 100, nrow(ferc))
    ),
      
    modal.size      = 'm', 
    text.width      = '100%',
    textarea.width  = '535px',
    textarea.height = '300px',
    select.width    = '100%',
    
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
  
  
  # observeEvent(input$refresh, {
  #   if (is.null(input$save_sel)) {
  #     project_choices <- get_ferc()$project %>% unique() %>% sort() %>% na.omit()
  #   } else {
  #     project_choices <- prj_values_df$prj %>% unique() %>% sort() %>% na.omit()
  #   }
  # })
  
  
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

    # modal_controls <- glideControls(
    #   list(
    #     prevButton(),
    #     firstButton(
    #       class = "btn btn-primary",
    #       `data-dismiss` = "modal",
    #       "Add more project input choices"
    #     )
    #   ),
    #   list(
    #     nextButton(),
    #     lastButton(
    #       class = "btn btn-primary",
    #       `data-dismiss`= "modal",
    #       "Return to FERC docs table"
    #     )
    #   )
    # )
    
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
    })
        
      
    
  # shinyglide::glide(
  #   custom_controls = modal_controls,
  #   screen(
  #     next_label = "Continue etc.",
  #     p("ok")),
  #   screen(
  #     p("no")),
  #   screen(
  #     p("next"))
    
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
    # browser()
    prj_choices(prj_values_df$prj %>% sort() %>% unique())
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
} 


# UI ----
ui <- tagList(
  # * css styling ----
  includeCSS("www/styles.css"),
  shiny::tags$head(
    shiny::tags$script(
      src = "js/index.js"),
    shiny::tags$style(HTML("
      ::placeholder { 
        word-wrap: break-word;
        white-space: pre-line;
        position: relative;
        padding: 30px 30px 10px 5px !important;
      }
      .selectize-input { 
        word-wrap: break-word; 
        word-break: break-word; 
        padding: 10px;
        min-height: 60px;
      }
      .selectize-dropdown {
        word-wrap: break-word; 
        padding: 10px;
        min-height: 60px;
      }
      
      /* prj dropdown menus */
      #sel_prj+ div>.selectize-dropdown, 
      #sel_prj_doc+ div>.selectize-dropdown,
      #sel_prj_doc_sec+ div>.selectize-dropdown,
      #sel_prj_doc_sec_url+ div>.selectize-dropdown,
      
      /* prj inputs */
      #sel_prj+ div>.selectize-input,
      #sel_prj_doc+ div>.selectize-input,
      #sel_prj_doc_sec+ div>.selectize-input,
      #sel_prj_doc_sec_url+ div>.selectize-input {
        width: 350px !important;
        
      }
     
      .btn-primary:hover {
        color: #fff;
        background-color: #0069d9 !important;
        border-color: #0069d9 !important;
      }
    
      .btn-primary:active, .btn-primary:focus {
        background-color: #0069d9 !important;
        border-color: #00397 !important;
        background-blend-mode: darken !important;
        box-shadow: inset 0px 0px 400px 110px rgba(0, 0, 0, .7) !important;
      }
      #refresh_btn {
        display: inline;
        float: right;
        right: 10vw;
        text-align: center;
        height: 60px;
        width: 300px;
        margin: 5px;
        top: 10px;
        font-size: 16px;
      }
      
      .cell-border-right {
        border-right: 0.75px solid #ddd;
      }
     "))
    ),

  navbarPage(
    "Edit",
    
    #* Documents ----
    tabPanel(
      "Documents", value = "docs",
      span(
        span(
          actionButton(
            "refresh_btn",
            "Refresh FERC docs table",
            icon  = icon("refresh"),
            class = "btn-primary")),
        span(
          h3("Editable FERC Documents"))),
      helpText(
        span(
          span("Please add a new project, project document, or document section on "),
          shiny::tags$a("Project Docs page", onclick="customHref('prj_docs')"))),
          # span(actionLink("link_to_prj_docs", "Project Docs page")))),
      hr(),
      div(
        "Tags filter by:",
        icon("tags"),
        span(class="me-tag me-technology", "Technology"),
        span(class="me-tag me-stressor",   "Stressor"),
        span(class="me-tag me-receptor",   "Receptor"),
        span(class="me-tag me-phase",      "Phase")),
      helpText(
        HTML("The FERC eLibrary contains environmental compliance project documents, 
          of which excerpts have been manually tagged for reference.")),
      hr(), 
      # DTEDIT TABLE
      div(
        id = "ferc_docs_table",
        shinycssloaders::withSpinner(
          uiOutput("ferc_dt_edit"),
          # loading spinner:
          type = 8, color = "#007BFE"),
        style = "padding: 5px;")
    ),
    
    #* Project Docs ----
    tabPanel(
      "Project Docs", value = "prj_docs",
      
      fluidRow(
        
        column(
          width = 4,
          div(
            style = "
              display: inline;
              float: right;
              padding: 10px;
              display: inline;
              margin: 10px 10px 20px 10px;
            ",
            helpText(
              "Add new projects, project docs, and project doc sections below."),
          
            # PROJECT
            selectizeInput(
              "sel_prj",
              "Project",
              d_prj$project,
              options = list(
                create = T,
                onInitialize = I(
                  'function() { this.setValue("") }'),
                placeholder = 
                  "Select from menu or type to add new project")),
            # TODO: (BB did for ecoidx-up) if new project, update google sheet [data | marineenergy.app - Google Sheets](https://docs.google.com/spreadsheets/d/1MTlWQgBeV4eNbM2JXNXU3Y-_Y6QcOOfjWFyKWfdMIQM/edit#gid=5178015)
            
            # DOC
            selectizeInput(
              "sel_prj_doc",
              "Project Document", 
              d_prj_doc$prj_doc, 
              options = list(
                create = T,
                onInitialize = I(
                  'function() { this.setValue("") }'),
                placeholder = 
                  "Select from menu or type to add new doc")),
            
            # SECTION
            selectizeInput(
              "sel_prj_doc_sec",
              "Project Document Section",
              d_prj_doc_sec$prj_doc_sec,
              options = list(
                create = T,
                onInitialize = I(
                  'function() { this.setValue("") }'),
                placeholder = 
                  "Select from menu or type to add new section")),
            
            # URL
            selectizeInput(
              "sel_prj_doc_sec_url",
              "Project Document Section URL",
              d_prj_doc_sec$prj_doc_sec_url,
              options = list(
                create = T,
                onInitialize = I(
                  'function() { this.setValue("") }'),
                placeholder = 
                  "Select from menu or type to add URL")),
            
            # SAVE/UPDATE
            actionButton(
              "save_sel",
              "Save",
              icon = icon("save"),
              class = "btn-primary"),
            br()
          )), 
      
        # for debugging: visualize updates
        column(
          width = 8,
          div(
            h3("Existing projects"),
            withSpinner(tableOutput("prj_table"), type = 8, color = "#007BFE")
        ))
      )
    )
    
    
    
      
 
        
        
         
        # 
        # prj_1           <- d_prj$project[1]
        # # all prj docs for first prj
        # d_prj_doc_1     <- d_prj_doc %>% filter(prj == prj_1)
        # # first prj doc for first prj
        # prj_doc_1       <- d_prj_doc_1$prj_doc[1]
        # # all prj doc sections for first prj doc for first prj
        # d_prj_doc_sec_1 <- d_prj_doc %>% filter(prj == prj_1, prj_doc == prj_doc_1)
        
        
        
        
        
        
        
        # read_csv("data/project_docs.csv") %>% 
        #   # default to first project
        #   filter(
        #     project == readr::read_csv("data/project_sites.csv") %>% 
        #       arrange(project) %>% pull(project) %>% .[1])
        #   pull(project),
        # options = list(
        #   create = T)) 
    
))

# SHINY APP ----
shinyApp(ui, server)
