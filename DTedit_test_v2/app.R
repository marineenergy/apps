# DB CONNECTION & SCRIPTS ----


# LIBRARIES ----
# devtools::install_github("DavidPatShuiFong/DTedit@f1617e253d564bce9b2aa67e0662d4cf04d7931f")
shelf(DavidPatShuiFong/DTedit, DBI, DT, glue, purrr)

# FXNS REFERENCED IN CALLBACKS ----

# convert data from dtedit to ferc_docs format  
get_new_docs <- function(d, flds = ferc_doc_names) {
  d %>% 
    select(-document, -tag_sql, -tag_named, -tag_html) %>% 
    relocate(flds)
}

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
  
  prj_levels <- get_ferc()$project %>% unique() %>% sort() %>% na.omit()
  
  # for server:
  observeEvent(input$submit, {
    #browser()
    input <- input$add_prj
    updateSelectInput(
      session, "prj_select",
      choices = as.character(c(
        ferc$project %>% unique() %>% sort() %>% na.omit(),
        input)))
  })
  
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
      project            = "selectizeInput",    
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
      project   = get_ferc()$project %>% unique() %>% sort() %>% na.omit(),
      ck_ixn    = c(TRUE, FALSE),
      ck_obs    = c(TRUE, FALSE),
      ck_mp     = c(TRUE, FALSE), 
      ck_amp    = c(TRUE, FALSE),
      ck_pme    = c(TRUE, FALSE),
      ck_bmps   = c(TRUE, FALSE),
      tag_named = tag_choices
    ), 
    
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
        list(targets = c(0, 6, 7, 8, 9, 10, 11),
             className = 'dt-center'),
        # in-row checkboxes
        list(targets = c(6, 7, 8, 9, 10, 11),
             render = DT::JS(
               "function(data, type, row) {
                  if (data == true) {
                    data = '<div class=\"text-success\"><span class=\"glyphicon glyphicon-ok-circle\"></span></div>';
                  } else if (data == false) {
                    data = '<div class=\"text-danger\"><span class=\"glyphicon glyphicon-remove-circle\"></span></div>';
                  } return data}"))),
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
  
  # update data according to dtedit data
  data_list <- list() 
  observeEvent(fercdt$thedata, {
    data_list[[length(data_list) + 1]] <<- fercdt$thedata
  })

  shiny::exportTestValues(data_list = {data_list})
  
  cancel.onsessionEnded <- session$onSessionEnded(function() {
    DBI::dbDisconnect(con)
  })
} 


# UI ----
ui <- tagList(
  # * css styling ----
  includeCSS("www/styles.css"),
  
  navbarPage(
    "Edit",
    
    tabPanel(
      "Documents", 
      #* Documents ----
      helpText(
        "Editable FERC Documents", br(),
        "Please add a new project in ", 
        a("data | marineenergy.app - Google Sheet", 
          href="https://docs.google.com/spreadsheets/d/1MTlWQgBeV4eNbM2JXNXU3Y-_Y6QcOOfjWFyKWfdMIQM/edit#gid=5178015", 
          target="_blank")),
      div(
        "Filters by:",
        icon("tags"),
        span(class="me-tag me-technology", "Technology"),
        span(class="me-tag me-stressor",   "Stressor"),
        span(class="me-tag me-receptor",   "Receptor"),
        span(class="me-tag me-phase",      "Phase")),
      helpText(
        HTML("The FERC eLibrary contains environmental compliance project documents, 
          of which excerpts have been manually tagged for reference.")),
      uiOutput("ferc_dt_edit")
    ),
    
    tabPanel(
      "Add new project"
      
    )
      
      
))

# SHINY APP ----
shinyApp(ui, server)
