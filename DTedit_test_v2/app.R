# DB CONNECTION ----
dir_scripts <<- here::here("scripts")
source(file.path(dir_scripts, "common_2.R"))
source(file.path(dir_scripts, "db.R"))
source(file.path(dir_scripts, "shiny_report.R"))

# LIBRARIES ----
shelf(DavidPatShuiFong/DTedit, DT, glue, purrr)

# FXNS FOR get_ferc() ----
merge_tags <- function(tag_list_col) {
  tag_list_col %>% 
    unlist() %>% 
    unique() 
}
merge_tags_html <- function(tag_list_col) {
  tag_list_col %>% 
    unlist() %>% unique() %>% 
    paste(., collapse = "\n")
}
merge_tags_named <- function(tag_list_col) {
  tag_list_col %>% 
    unlist(recursive = F) %>% 
    unlist(recursive = F, use.names = T) %>% 
    unique() 
}

# FXNS REFERENCED IN CALLBACKS ----

# read in & merge ferc_docs & ferc_doc_tags from db
get_ferc <- function() {
  # read in ferc_docs and merge w/ table created by inner join b/w
  # ferc_doc_tags and tags lookup
  dbReadTable(con, "ferc_docs") %>% 
    tibble() %>% collect() %>% 
    na_if("NA") %>%
    merge(
      # read in ferc_doc_tags
      dbReadTable(con, "ferc_doc_tags") %>% 
        tibble() %>% collect() %>% 
        mutate(tag_sql_chr = ifelse(
          content_tag == "ALL",
          glue("{tag_category}.{content_tag}") %>% as.character(),
          tag_sql %>% as.character())) %>% 
        filter(tag_sql != "NA") %>% 
        select(-content, -tag_category, -content_tag) %>% 
        # inner_join() with tags lookup to get tag_html & tag_named
        inner_join(
          get_tags() %>% 
            mutate(
              tag_html_nocat = glue(
                "<span class='me-tag me-{cat}'>{tag_nocat}</span>")) %>% 
            select(tag_sql, tag_named, tag_html_nocat) %>% 
            rename(tag_html = tag_html_nocat),
          by = c("tag_sql_chr" = "tag_sql")) %>% 
        select(-tag_sql_chr) %>%
        group_by(rowid) %>% 
        tidyr::nest(
          tag_sql   = tag_sql,          # for UPDATING / storage
          tag_named = tag_named,        # for EDIT INTERFACE
          tag_html  = tag_html),        # for VIEW dtedit table
      # merge params
      by.x = "rowid", by.y = "rowid", all.x = T, incomparables = NA) %>% 
  mutate(
    tag_sql   = map(tag_sql,   merge_tags),
    tag_named = map(tag_named, merge_tags_named),
    tag_html  = map(tag_html,  merge_tags_html),
    document  = ifelse(
      is.na(prj_doc_attachment),
      prj_document,
      glue("{prj_document} - {prj_doc_attachment}")),
    document = ifelse(
      is.na(prj_doc_attach_url),
      document,
      glue('<a href="{prj_doc_attach_url}">{document}</a>')),
    document = as.character(document)) %>% 
    relocate(
      rowid, document, project, detail, 
      tag_sql, tag_named, tag_html) %>% 
    arrange(rowid) %>%
    data.frame()
}

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
ferc.insert.callback <- function(data, row) {

  d <- data %>% slice(row) %>% 
    na_if("NA") %>% na_if("") %>% 
    mutate(rowid = max(get_ferc()$rowid) + 1)
  
  d_docs <- d %>% get_new_docs(dbReadTable(con, "ferc_docs") %>% names())
  d_tags <- d %>% get_new_tags(dbReadTable(con, "ferc_doc_tags") %>% names())

  sql_insert_docs <- glue_data_sql(
    d_docs,
    "INSERT INTO ferc_docs VALUES
      ({rowid}, {detail}, {project},
      {prj_document}, {prj_doc_attachment}, {prj_doc_attach_url},
      {ck_ixn}, {ck_obs}, {ck_mp}, {ck_amp}, {ck_pme}, {ck_bmps})",
    .con = conn)
  
  sql_insert_tags <- glue_data_sql(
    d_tags,
    "INSERT INTO ferc_doc_tags VALUES
      ({content}, {rowid}, {tag_category}, {content_tag}, {tag_sql})",
    .con = conn)
  
  # for debugging
  print(sql_insert_docs)
  print(sql_insert_tags)
  
  res <- try(dbExecute(con, sql_insert_docs))
  if ("try-error" %in% class(res)) stop(res)
  dbClearResult(res)
  
  res <- try(dbExecute(con, sql_insert_tags))
  if ("try-error" %in% class(res)) stop(res)
  dbClearResult(res)
  
  get_ferc()
}

ferc.update.callback <- function(data, olddata, row) {

  d <- data %>% slice(row) %>% 
    tibble() %>% 
    mutate(
      across(starts_with("ck_"), as.logical)) %>% 
    na_if("NA") %>% 
    na_if("") #%>% 
    #na_if(NULL)
  
  # data to UPDATE ferc_docs
  d_docs <- get_new_docs(d)
  
  # data to be APPENDED to ferc_doc_tags
  d_tags <- get_new_tags(d)
  
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
  
  #res <- try(dbExecute(con, sql_update_tags))
  #if ("try-error" %in% class(res)) stop(res)
  DBI::dbAppendTable(conn, "ferc_doc_tags", d_tags)
  get_ferc()
}
  

ferc.delete.callback <- function(data, row) {
  browser()
  
  d <- data %>% slice(row) %>% na_if("NA") %>% na_if("")

  sql_delete <- glue_data_sql(
    d, 
    "DELETE FROM ferc_docs     WHERE rowid = {rowid};
     DELETE FROM ferc_doc_tags WHERE rowid = {rowid}", 
    .con = conn)
  
  print(sql_delete) # debug
  res <- try(dbExecute(con, sql_delete))
  if ("try-error" %in% class(res)) stop(res)
  dbClearResult(res)
  get_ferc()
}

#* get data ----
ferc <- get_ferc() 
tags <- get_tags() 
# tag_lookup     <- dbReadTable(con, "tag_lookup")
ferc_doc_names <- dbReadTable(con, "ferc_docs") %>% names()
ferc_tag_names <- dbReadTable(con, "ferc_doc_tags") %>% names()

# d_tags <- get_tags()
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

  
  # observeEvent(input$create_prj, {
  #   need(validate(input$create_prj))
  #   new_prj <- input$create_prj
  #   
  #   prj_levels <- c(
  #     ferc$project %>% unique() %>% sort() %>% na.omit(),
  #     new_prj)
  #   return(prj_levels)
  # })
  
  #* get labels ----
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
      'Project', 
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
      project            = "selectInput",
      prj_document       = "textInput",
      prj_doc_attachment = "fileInput",
      prj_doc_attach_url = "textInput",
      detail             = "textAreaInput",
      tag_named          = "selectInputMultiple",
      ck_ixn             = "selectInput",
      ck_obs             = "selectInput",
      ck_mp              = "selectInput",
      ck_amp             = "selectInput",
      ck_pme             = "selectInput",
      ck_bmps            = "selectInput"),
    input.choices = list(
      # TODO: add tab/link to app for editing projects and associated documents, attachments and urls
      project   = ferc$project %>% unique() %>% sort() %>% na.omit(),
      ck_ixn    = c(TRUE, FALSE),
      ck_obs    = c(TRUE, FALSE),
      ck_mp     = c(TRUE, FALSE), 
      ck_amp    = c(TRUE, FALSE),
      ck_pme    = c(TRUE, FALSE),
      ck_bmps   = c(TRUE, FALSE),
      tag_named = tag_choices
    ), 
    
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
      
    selectize       = T, 
    
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

    

    

  
  
  

  # IMPORTANT
  # observeEvent(input$attachment_method, {
  #   input <- input$attachment_method
  #   print(input)
  #   print(fercdt$input.types)
  #   
  #   fercdt$input.types <- reactive({
  #     if(!is.null(input)) {
  #       if (input == "url") {
  #         input_types <- c(input.types.2, Attachment = 'textInput') 
  #       } else if (input == "file") {
  #         input_types <- c(input.types.2, Attachment = 'fileInput')
  #       } 
  #       return(input_types)
  #       browser()
  #     }
  #   })
  # })

  
  
  
  
    
   
    
    
    # input.choices.reactive = list(
    #   project.types.list = project.types),
    
    
    # project = list(
    #   placeholder = "Please select an option below",
    #   onInitialize = I('function() { this.setValue(""); }')
    
    
      # arguments$selection <- 'none'
      # arguments$options  <- 
      #   # bind inputs to shiny in order to use values from the input
      #   list(
      #     initComplete    = JS(js.selectize),
      #     preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      #     drawCallback    = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      #     )
      
  
      
      # options(class = 'stripe hover') %>% 
      # do.call(DT::datatable, arguments) %>%  
      #   class    = 'display',
      #   # filter   = 'top',
      #   # colnames = c("ID", "Key interaction detail", "Project", "Doc name", "Tags"),
      #   callback = DT::JS(callback = JS(style.callback))) %>% 
      #   DT::formatStyle(
      #     'Doc', 
      #     # 'project',
      #     fontWeight = 'bold')  
      # backgroundColor = styleEqual(project.types, c('gray', 'pink'))) 
      # DT::formatStyle(
      #   'doc.NAME', 'project', 
      #   backgroundColor = styleEqual(
      #     project.types, 
      #     c("gray", "pink"))
      # DT::formatStyle(
      #   'doc.NAME',
      #   background = styleColorBar(range(ferc$rowid), color = 'lightblue'),
      #   backgroundPosition = 'center')
   
    
    # datatable.options = list(
    #   columnDefs = list(
    #     list(
    #       targets = "_all",
    #       render  = JS(
    #         "function(data, type, row, meta) {",
    #         "return type === 'display' && data != null && data.length > 30 ?",
    #         "'<span title=\"' + data + '\">' + data.substr(0, 200) + '...</span>' : data;",
    #         "}")))),
    
    # class = "display",
    # escape = F, selection = "multiple",
    
   
  
  
  
  
  
  # output$attachment_input <- renderUI({
  #   input.types.2 <- getAttachmentInput(input$attachment_method)
  # })
  # 
  
  # getAttachmentInput <- function(input) {
  #   if (is.null(input)) {
  #     return() 
  #   } else {
  #     if (input == input$url) {
  #       input.types.2 <- c(input.types.2, Attachment = 'textInput') 
  #     } else if (input == input$file) {
  #       input.types.2 <- c(input.types.2, Attachment = 'fileInput')
  #     }
  #     return(input.types.2)
  #   }
  # }
  
 



  


  
  
  # observeEvent(input$choice, {
  #   if (input$choice != "Other") {
  #     project.types.list = project.types
  #   } else {
  #     project.types.list = "hello"
  #   }
  # })
   
  # observeEvent(input$url_link, {
  #   input.types.2 <- getAttachmentInput(input$url_link)
  # })
  # 
  # observeEvent(input$file_upload, {
  #   input.types.2 <- getAttachmentInput(input$url_link)
  # })
  
  
  # observeEvent(input$attachment_choice, {
  #   if (input$attachment_choice == 1) {
  #     attach.options(options_1)
  #   } else {
  #     attach.options(options_2)
  #   }
  # })

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
        "Editable FERC Documents",br(),
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
      uiOutput("ferc_dt_edit"))
))

# fluidRow(
#   column(
#     width = 12,
#     # radioButtons(
#     #    "attachment_method",
#     #    "Select a method to attach document",
#     #    choiceValues = list("url", "file"),
#     #    choiceNames  = list(
#     #      tags$div(
#     #        span(tagList(icon("link"), "Attach URL link"))),
#     #      tags$div(
#     #        span(tagList(icon("upload"), "Upload file"))))),
#     # selected = NULL,
#     # inline = F,
#     hr())),
# HTML(tag_html_choices[1]),

# SHINY APP ----
shinyApp(ui, server)




# d_merged_test <- d_merged_test %>% 
#   mutate(tag = paste(d_merged_test$tag_sql, collapse = "; "))

# for (doc in 1:length(d_merged)) {
#   d_merged[[doc]] <- d_merged[[doc]] %>% 
#     filter(!is.na(tag_sql)) %>% 
#     distinct(tag_sql, .keep_all = T)
#   d_merged[[doc]] <- d_merged[[doc]] %>% 
#     mutate(tag_sql_c = paste(d_merged[[doc]]$tag_sql, collapse = "; ")) %>% 
#     select(-tag_sql) %>% 
#     distinct(rowid, .keep_all = T)
# }
# 
# d <- d_merged %>% bind_rows()





# tbl_ferc_docs     <- tbl(con, "ferc_docs")     # main field: key_interaction
# tbl_ferc_doc_%>% <- tbl(con, "ferc_doc_tags") # linked by rowid

# join ferc_docs & ferc_doc_tags
# d_ferc_docs <- tbl_ferc_docs %>% 
#   left_join(
#     tbl_ferc_doc_tags,
#     by = "rowid") %>% 
#   collect() %>% # retrieves data into a LOCAL TIBBLE (instead of db connex)
#   mutate(id = 1:nrow(.))
# # d_ferc_docs <- d_ferc_docs %>% 
# # mutate(
# #   key_ixn_sub = glue(
# #     "{stringr::str_sub(
# #     d_ferc_docs$key_interaction_detail, 1L, 250)}..."))
# d_ferc_docs <- data.frame(d_ferc_docs)

##### Load books data.frame as a SQLite database
# conn <- dbConnect(RSQLite::SQLite(), "books.sqlite")
# 
# if (!'books' %in% dbListTables(conn) || isTRUE(getOption("shiny.testmode"))) {
#   # the sqlite file doesn't have the right data
#   # OR we are running in test mode (test mode -> reset the data)
#   books <- read.csv('https://raw.githubusercontent.com/DavidPatShuiFong/DTedit/master/inst/shiny_demo/books.csv', stringsAsFactors = FALSE)
#   books$Authors <- strsplit(books$Authors, ';')
#   books$Authors <- lapply(books$Authors, trimws) # Strip white space
#   books$Authors <- unlist(lapply(books$Authors, paste0, collapse = ';'))
#   books$id <- 1:nrow(books) # can also use 'seq_length(nrow(books))'
#   books$Date <- paste0(books$Date, '-01-01')
#   dbWriteTable(conn, "books", books, overwrite = TRUE)
# }



##### Callback functions.
# ferc.insert.callback <- function(data, row) {
#   query <- paste0(
#     "INSERT INTO d (rowid, tag_sql_c) VALUES (",
#     "", max(get_ferc$rowid) + 1, ", ",
#     "'", paste0(data[row,]$tag_sql_c[[1]], collapse = '; '), "', ",
#     ")")
#   print(query) # For debugging
#   res <- dbSendQuery(con, query)
#   dbClearResult(res)
#   return(get_ferc())
# }
# 
# ferc.update.callback <- function(data, olddata, row) {
#   query <- paste0("UPDATE d SET ",
#                   "tag_sql_c = '", paste0(data[row,]$tag_sql_c[[1]], collapse = '; '), "', ",
#                   # "Date = '", as.character(data[row,]$Date), "', ",
#                   # "Title = '", data[row,]$Title, "', ",
#                   # "Publisher = '", as.character(data[row,]$Publisher), "' ",
#                   "WHERE rowid = ", data[row,]$rowid)
#   print(query) # For debugging
#   res <- dbSendQuery(con, query)
#   dbClearResult(res)
#   return(get_ferc())
# }
# 
# ferc.delete.callback <- function(data, row) {
#   query <- paste0('DELETE FROM d WHERE rowid = ', data[row,]$rowid)
#   res <- dbSendQuery(con, query)
#   dbClearResult(res)
#   return(get_ferc())
# }
# 
# ##### Create the Shiny server
# server <- function(input, output, session) {
#   d <- get_ferc()
#   ferc_dt <- dtedit(
#     input, output,
#     name = 'ferc',
#     thedata = d,
#     edit.cols = 'tag_sql_c',
#     edit.label.cols = "Tags",
#     # input.types = c(Title = 'textAreaInput'),
#     input.choices = list(tag_sql_c = unique(unlist(d$tag_sql_c))),
#     view.cols = names(d),
#     callback.update = ferc.update.callback,
#     callback.insert = ferc.insert.callback,
#     callback.delete = ferc.delete.callback
#   )
# 
#   # names <- data.frame(
#   #   Name = character(), Email = character(), Date = numeric(),
#   #   Type = factor(levels = c('Admin', 'User')),
#   #   stringsAsFactors = FALSE)
#   # names$Date <- as.Date(names$Date, origin = '1970-01-01')
#   # namesdt <- dtedit(input, output, name = 'names', names)
# 
#   data_list <- list() # exported list for shinytest
#   shiny::observeEvent(ferc_dt$thedata, {
#     data_list[[length(data_list) + 1]] <<- ferc_dt$thedata
#   })
#   shiny::exportTestValues(data_list = {data_list})
# 
#   cancel.onsessionEnded <- session$onSessionEnded(function() {
#     DBI::dbDisconnect(con)
#   })
# }



# library(shiny)
# 
# library(DTedit)
# 
# server <- function(input, output, session) {
# 
#   Grocery_List <- dtedit(
#     input, output,
#     name = 'Grocery_List',
#     thedata = data.frame(
#       Buy = c('Tea', 'Biscuits', 'Apples'),
#       Quantity = c(7, 2, 5),
#       stringsAsFactors = FALSE
#     ),
#     # edit.cols = c("Buy", "Quantity"),
#     # input.types = c("selectInputMultiple", "numericInput"),
#     input.choices = list(
#       Buy = c('Tea', 'Biscuits', 'Apples'))
#   )}
# 
# ui <- fluidPage(
#   h3('Grocery List'),
#   uiOutput('Grocery_List'))
# 
# # if (interactive())
# shinyApp(ui = ui, server = server)

# library(shiny)
# library(DTedit)
# library(randomNames)
# 
# ##### Create the Shiny server
# server <- function(input, output, session) {
#   
#   names.Type.update.callback <- function(data, olddata, row) {
#     # update a user-type
#     # do not allow an updated user-type which is the same as another
#     ## if this is attempted, show a warning
#     if (data[row,] %in% data[-row,]) {
#       stop(paste0(
#         "Cannot change user-type to '", data[row,],
#         "', that user-type already exists!"
#       ))
#     }
#     return(data)
#   }
#   
#   names.Type.insert.callback <- function(data, row) {
#     # insert a user-type
#     # do not allow a new user-type which is the same as an old one
#     ## if this is attempted, show a warning
#     if (data[row,] %in% data[-row,]) {
#       stop(paste0(
#         "Cannot add '", data[row,],"', that user-type already exists!"
#       ))
#     }
#     return(data)
#   }
#   
#   names.Type.delete.callback <- function(data, row) {
#     # remove a user-type
#     # it is possible for this user-type to be currently used
#     # by an entry in names()  (names has been explicitly passed by reference
#     # to the dtedit function), in which case this function will show a warning
#     if (data[row,] %in%
#         get("input.choices.reactive", parent.frame())[["names"]]()$Type) {
#       stop(paste0("Cannot delete '", data[row,],
#                   "', this user-type currently assigned to a user."))
#     } else {
#       data <- data[-row,, drop = FALSE]
#     }
#     return(data)
#   }
#   
#   names.Like.update.callback <- function(data, olddata, row) {
#     # update a like
#     # do not allow an updated like which is the same as another
#     ## if this is attempted, show a warning
#     if (data[row,] %in% data[-row,]) {
#       stop(paste0(
#         "Cannot change like to '", data[row,],
#         "', that like already exists!")
#       )
#     }
#     return(data)
#   }
#   
#   names.Like.insert.callback <- function(data, row) {
#     # insert a like
#     # do not allow a like which is the same as an old one
#     ## if this is attempted, show a warning
#     if (data[row,] %in% data[-row,]) {
#       stop(paste0("Cannot add '", data[row,],"', that like already exists!"))
#     }
#     return(data)
#   }
#   
#   names.Like.delete.callback <- function(data, row) {
#     # remove a like
#     # it is possible for this like to be currently used
#     # by an entry in names()  (names has been explicitly passed by reference
#     # to the dtedit function), in which case this function will show a warning
#     if (data[row,] %in%
#         unlist(
#           get("input.choices.reactive", parent.frame())[["names"]]()$Like
#         )
#     ) {
#       stop(paste0("Cannot delete '", data[row,],
#                   "', this like currently assigned to a user."))
#     } else {
#       data <- data[-row,, drop = FALSE]
#     }
#     return(data)
#   }
#   
#   names.Like <- reactiveVal()
#   names.Like(data.frame(Likes = c("Apple", "Pear"), stringsAsFactors = FALSE))
#   names.Likedt <- dtedit(
#     input, output,
#     'names.Like',
#     thedata = names.Like,
#     edit.cols = c("Likes"),
#     input.types = c(Likes = "textAreaInput"),
#     view.cols = c("Likes"),
#     input.choices.reactive = list(names = names),
#     # names is never used as an input, but will be checked
#     # during the callback.delete
#     callback.delete = names.Like.delete.callback,
#     callback.insert = names.Like.insert.callback,
#     callback.update = names.Like.update.callback
#   )
#   names.Likes <- reactiveVal(isolate(names.Like()$Likees))
#   
#   names.Type <- reactiveVal()
#   names.Type(data.frame(
#     Types = c("Admin", "User"),
#     stringsAsFactors = FALSE)
#   )
#   names.Typedt <- dtedit(
#     input, output,
#     'names.Type',
#     thedata = names.Type,
#     edit.cols = c("Types"),
#     input.types = c(Types = "textAreaInput"),
#     view.cols = c("Types"),
#     input.choices.reactive = list(names = names),
#     # names is never used as an input, but will be checked
#     # during the callback.delete
#     callback.delete = names.Type.delete.callback,
#     callback.insert = names.Type.insert.callback,
#     callback.update = names.Type.update.callback
#   )
#   
#   names.Types <- reactiveVal(isolate(names.Type()$Types))
#   
#   names <- reactiveVal()
#   names(
#     data.frame(
#       Name=character(), Email=character(),
#       Date=as.Date(integer(), origin='1970-01-01'),
#       Type = character(),
#       Like = character(),
#       # Like = I(list(isolate(factor(character(),
#       #          levels = names.Likes())))),
#       stringsAsFactors=FALSE
#     )
#   )
#   
#   observe({
#     names.Types(names.Typedt$thedata$Types)
#     names.Likes(names.Likedt$thedata$Likes)
#   })
#   
#   namesdt <- dtedit(
#     input, output,
#     'names',
#     thedata = names,
#     input.types = c(
#       Type = "selectInputReactive",
#       Like = "selectInputMultipleReactive"
#     ),
#     input.choices = c(Type = "names.Types", Like = "names.Likes"),
#     input.choices.reactive = list(
#       names.Types = names.Types, names.Likes = names.Likes
#     )
#   )
#   
#   observe({
#     print(namesdt$thedata)
#     names(as.data.frame(namesdt$thedata, stringsasfactors = FALSE))
#     print(paste("Edit count:", namesdt$edit.count))
#   })
#   
#   observeEvent(input$email_clean,{
#     names(names()[0,]) # empty the dataframe
#   })
#   
#   observeEvent(input$email_add, {
#     email <- c("hotmail.com", "yahoo.com",
#                "gmail.com", "outlook.com",
#                "github.com", "bigpond.com",
#                "medscape.com")
#     extra_email <- data.frame( # create random user
#       Name = randomNames::randomNames(
#         name.order = "first.last",
#         name.sep = " "
#       ),
#       Email = paste0(
#         do.call(
#           paste0,
#           replicate(sample(5:8, 1), sample(tolower(LETTERS), 1, TRUE), FALSE)
#         ),
#         '@',sample(email, 1)
#       ),
#       Date = as.Date(Sys.Date() - sample(1:1000, 1), origin = "1970-01=01"),
#       Type = factor(sample(names.Types(), 1), levels = names.Types()),
#       Like = I(list(factor(sample(names.Likes(),
#                                   sample(seq_len(length(names.Likes())), 1)),
#                            levels = names.Likes()))),
#       stringsAsFactors = FALSE
#     )
#     names(data.frame(rbind(names(), extra_email), stringsAsFactors = FALSE))
#   })
#   
#   data_list <- list() # exported list for shinytest
#   shiny::observeEvent(namesdt$thedata, {
#     data_list[[length(data_list) + 1]] <<- namesdt$thedata
#   })
#   shiny::exportTestValues(data_list = {data_list})
#   
# }
# 
# ##### Create the shiny UI
# ui <- fluidPage(
#   tabsetPanel(
#     type = "tabs",
#     tabPanel(
#       "Users",
#       h3('Email Addresses'),
#       uiOutput('names')
#     ),
#     tabPanel(
#       "Add/Delete",
#       wellPanel(
#         actionButton("email_add", "Add an email entry"),
#         actionButton("email_clean", "Delete entire email list")
#       )
#     ),
#     tabPanel(
#       "User Types",
#       wellPanel(
#         uiOutput("names.Type")
#       )
#       
#     ),
#     tabPanel(
#       "Likes",
#       wellPanel(
#         uiOutput("names.Like")
#       )
#       
#     )
#   )
# )
# 
# if (interactive() || isTRUE(getOption("shiny.testmode")))
#   shinyApp(ui = ui, server = server)
