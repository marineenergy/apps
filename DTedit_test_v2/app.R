# https://rpubs.com/DavidFong/DTedit
# https://github.com/DavidPatShuiFong/DTedit/blob/master/inst/shiny_demo/app.R

# TODO: 
# - [ ] make sure update/add/new 
# - [ ] color code tags
# - [ ] fix db connex so can save added/updated/deleted ferc records 
# - [ ] option to add new prj (`create = T` not working)
# - [ ] upgrade to forked copy with checkbox
#       https://github.com/DavidPatShuiFong/DTedit/issues/12#issuecomment-715228402

# DavidPatShuiFong/DTedit w/ support for checkboxInput
# because of * [support for checkboxes / logical input · DavidPatShuiFong/DTedit@cffb3f7](https://github.com/DavidPatShuiFong/DTedit/commit/cffb3f75578605ea3ce97ea05cbc9a85484e1c95)
#   devtools::install_github("DavidPatShuiFong/DTedit@develop")
# still doesn't seem to be loading input.type = "checkboxInput" capability

# DB CONNECTION ----
dir_scripts <<- here::here("scripts")
source(file.path(dir_scripts, "common_2.R"))
source(file.path(dir_scripts, "db.R"))
source(file.path(dir_scripts, "shiny_report.R"))

# LIBRARIES ----
shelf(DavidPatShuiFong/DTedit, DT, glue)


# FXNS FOR get_ferc() ----
merge_tags <- function(tag_list_col, tag_sql) {
  tag_list_col %>% 
    # filter(!is.na(col_name)) %>% 
    distinct(tag_sql, .keep_all = TRUE) %>% 
    pull(tag_sql)
}

merge_tags_html <- function(tag_list_col, tag_html) {
  tag_list_col %>% 
    pull(tag_html) %>% unlist() %>% unique() %>% 
    paste(., collapse = "\n")
}


# READ IN MERGED FERC TAGS DATA FROM DB ----
get_ferc <- function() {
  
 dbReadTable(con, "ferc_docs") %>% 
    tibble() %>% 
    collect() %>% 
    left_join(
      tbl(con, "ferc_doc_tags") %>% 
        select(rowid, tag_sql) %>%
        collect() %>% 
        # new part starts/
        mutate(tag_sql_chr = tag_sql %>% as.character()) %>% 
        left_join(
          get_tags() %>% select(tag_sql, tag_html), 
          by = c("tag_sql_chr" = "tag_sql")) %>% 
        select(-tag_sql_chr) %>% 
        # /end new part
        group_by(rowid) %>% 
        tidyr::nest(
          tag_sql  = tag_sql,
          tag_html = tag_html),
      by = "rowid") %>% 
    na_if("NA") %>% 
    mutate(
      tag_sql  = tag_sql  %>% purrr::map(merge_tags),
      tag_html = tag_html %>% purrr::map(merge_tags_html),
      document = ifelse(
        is.na(prj_doc_attachment),
        prj_document,
        glue("{prj_document} - {prj_doc_attachment}")),
      document = ifelse(
        is.na(prj_doc_attach_url),
        document,
        glue('<a href="{prj_doc_attach_url}">{document}</a>')),
      document = as.character(document)) %>% 
    relocate(rowid, document, project, detail, tag_sql, tag_html) %>% 
    arrange(rowid) %>%
    data.frame()
}


# FXNS REFRENCED IN CALLBACKS ----
get_new_docs <- function(d, flds = ferc_doc_names) {
  d %>% 
    select(-document, -tag_sql) %>% 
    relocate(flds)
}

get_new_tags <- function(d, flds = ferc_tag_names) {
  d %>% 
    unnest(tag_sql = tag_sql) %>% 
    select(rowid, tag_sql) %>% 
    left_join(
      tags %>% 
        rename(
          tag_category = category,
          content_tag  = tag_nocat) %>% 
        select(
          tag_category,
          content_tag,
          tag_sql), 
      by = c("tag_sql")) %>% 
    mutate(content = "ferc_docs") %>% 
    relocate(flds)
}


# CALLBACK FXNS ----
ferc.insert.callback <- function(data, row) {
  browser()
  
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
  # res <- dbSendQuery(con, query)
  dbClearResult(res)
  
  res <- try(dbExecute(con, sql_insert_tags))
  if ("try-error" %in% class(res)) stop(res)
  dbClearResult(res)
  
  get_ferc()
}

ferc.update.callback <- function(data, olddata, row) {
  #browser()

  d <- data %>% slice(row) %>% 
    tibble() %>% 
    mutate(
      across(starts_with("ck_"), as.logical)) %>% 
    na_if("NA") %>% 
    na_if("")
  
  # data to UPDATE ferc_docs
  d_docs <- d %>% get_new_docs()
  
  # data to be APPENDED to ferc_doc_tags
  d_tags <- d %>% get_new_tags()
  
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
  
    # "DELETE FROM ferc_docs WHERE rowid = ", data[row,]$ID)
  print(sql_delete) # debug
  # res <- dbSendQuery(con, query)
  res <- try(dbExecute(con, sql_delete))
  if ("try-error" %in% class(res)) stop(res)
  dbClearResult(res)
  get_ferc()
}

#* get data ----
ferc <- get_ferc() 
tags <- get_tags() # contains tag_html
tag_lookup     <- dbReadTable(con, "tag_lookup")
ferc_doc_names <- dbReadTable(con, "ferc_docs") %>% names()
ferc_tag_names <- dbReadTable(con, "ferc_doc_tags") %>% names()

# SHINY SERVER ----
server <- function(input, output, session) {
  
  
  # prj_levels <- eventReactive(input$create_prj, {
  #   c(input$create_prj, 
  #     ferc$project %>% 
  #       unique() %>% sort() %>% na.omit())
  # })
  
  prj_levels <- get_ferc()$project %>% unique() %>% sort() %>% na.omit()
  
  
  # for server:
  observeEvent(input$submit, {
    browser()
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
  labels <- tibble(
    # actual names as stored in ferc, ferc_docs, and ferc_doc_tags
    ferc_col_names = names(ferc),
    view_col_names = 
      c("ID", "Document", "Project", "Detail", NA, "Tags", 
        NA, "Attachment", NA,
        "Ixn", "Obs", "MP?", "AMP?", "PME?", "BMPs?")) %>%
    mutate(
      view_col = ifelse(is.na(view_col_names), FALSE, TRUE),
      edit_col_names = c(
        "ID",
        NA,
        'Project', 
        'Key interaction detail', 
        NA,
        'Tags',
        'Document name',
        'Upload document attachment',
        'Upload document URL link',
        'Presented as potential interaction?',
        'Described from observations at the project site?',
        'Monitoring plan (MP)?',
        'Adaptive management plan (AMP)?',
        'Protection mitigation and enhancement (PME)?',
        'Best management practices (BMPs) applied?'),
      edit_col = ifelse(
        ferc_col_names %in% c("document", "tag_sql"), FALSE, TRUE))

  
  tag_html_choices <- tags %>% 
    pull(tag_html) 

  
  #* dtedit() object ----
  fercdt <- dtedit(
    input, output,
    name      = 'ferc_new',
    thedata   = get_ferc(),
    # view.cols = labels %>% 
    #   filter(dt_display == TRUE) %>% 
    #   pull(ferc_col_names),
    # view.cols = labels %>% filter(view_col == T) %>% pull(ferc_col_names),
    view.cols = names(ferc %>% select(-prj_document, -prj_doc_attach_url)),
    # edit.cols = labels %>% 
    #   filter(edit_display == TRUE) %>% 
    #   pull(ferc_col_names),
    edit.cols = c(
      'project', 'prj_document', 'prj_doc_attachment', 'prj_doc_attach_url', 
      'detail', 'tag_html', 
      'ck_ixn', 'ck_obs', 'ck_mp', 'ck_amp', 'ck_pme', 'ck_bmps'),
    # edit.cols = c('Doc', 'Detail', 'Tags', 'Project'),
    # edit.label.cols = labels %>% 
    #   filter(edit_display == TRUE) %>% 
    #   pull(edit_dt_names),
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
      tag_html           = "selectInputMultiple",
      ck_ixn             = "selectInput",
      ck_obs             = "selectInput",
      ck_mp              = "selectInput",
      ck_amp             = "selectInput",
      ck_pme             = "selectInput",
      ck_bmps            = "selectInput"),
    input.choices = list(
      # TODO: add tab to app for editing projects and associated documents, attachments and urls
      # project = ferc$project %>% unique() %>% sort() %>% na.omit(),
      project = ferc$project %>% unique() %>% sort() %>% na.omit(),
      # project = ifelse(
      #   exists(prj_levels), prj_levels,
      #   ferc$project %>% unique() %>% sort() %>% na.omit()),
      ck_ixn  = c(TRUE, FALSE),
      ck_obs  = c(TRUE, FALSE),
      ck_mp   = c(TRUE, FALSE), 
      ck_amp  = c(TRUE, FALSE),
      ck_pme  = c(TRUE, FALSE),
      ck_bmps = c(TRUE, FALSE),
      tag_html = tag_html_choices
    ), 
      # tag_sql = tags$tag_sql),
            
       # tags %>% pull(tag_sql),
      # Tags    = tags$tag_sql,
      # Project = ferc$Project %>% levels(),
      # Ixn     = c("YES", "NO"),
      # Obs     = c("YES", "NO"),
      # MP      = c("YES", "NO"),
      # AMP     = c("YES", "NO"),
      # PME     = c("YES", "NO"),
      # BMP     = c("YES", "NO")),
    # input.choices.reactive = list(attach.types.list = attach.options),
    
    datatable.rownames = F,
    
    datatable.call = function(...) {
      arguments <- list(...)
      arguments$escape   <- 0
      arguments$class    <- 'display'
      arguments$colnames <- labels %>% 
        filter(view_col == TRUE) %>% 
        pull(view_col_names)
        # c(
        # "ID", "Document", "Project", "Detail", "Tags", "Attachment", 
        # "Ixn", "Obs", "MP?", "AMP?", "PME?", "BMPs?")
      do.call(DT::datatable, arguments) %>%
        DT::formatStyle('document',  fontWeight = 'bold')
    },
    
    # datatable.options ----
    datatable.options = list(
      colnames = labels %>%
        filter(view_col == TRUE) %>%
        pull(view_col_names),
      columnDefs = list(
        # centered rowid
        list(className = 'dt-center', targets = 0),
        # in-row checkboxes
        list(targets = c(6, 7, 8, 9, 10, 11),
             render = DT::JS(
               "function(data, type, row) {
                  if (data == true) {
                    data = '<div class=\"text-success\"><span class=\"glyphicon glyphicon-ok-circle\"></span></div>';
                  } else if (data == false) {
                    data = '<div class=\"text-danger\"><span class=\"glyphicon glyphicon-remove-circle\"></span></div>';
                  } return data}"))),
      # black heading background
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
      lengthMenu = c(5, 10, 25, 50, 100, nrow(ferc))
    ),
      
    selectize = T, # selectInputMultiple
    # selectize.options = list(
    #   Project = list(
    #     placeholder = "Please select an option below",
    #     onInitialize = I('function() { this.setValue(""); }')),
    # Doc = list(create = TRUE, maxItems = 1)),
    
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
  

  data_list <- list() 
  observeEvent(fercdt$thedata, {
    data_list[[length(data_list) + 1]] <<- fercdt$thedata
  })
  
  
  
  # ferc <- data.frame(get_ferc(), stringsAsFactors = F)
  # 
  # getAtmtInput <- function(input) {
  #   if (is.null(input)) {
  #     return(input.types.base)
  #   } else if (!is.null(input$attachment_method)) {
  #     if (input == "url") {
  #       input.types <- c(input.types.base, "textInput")
  #       return(input.types)
  #     } else if (input$attachment_method == "file") {
  #       input.types <- c(input.types.base, "fileInput")
  #       return(input.types)
  #     } 
  #   }
  # }
  # 
  # input.types <- reactive({
  #   getAtmtInput(input = input$attachment_method)
  # })
  # 
  # 
  
  
  
  #     Attachment = function(input){
  #       if (!is.null(input$attachment_method)) {
  #         if (input$attachment_method == "url") {
  #           return("textInput")
  #         } else if (input$attachment_method == "file") {
  #           return("fileInput")
  #         } 
  #       }
  #     }
  #   )
  #   
  #   
  # )
  #   
  
 

    

    
    # proxy
    # fercdt_proxy <- DT::dataTableProxy("fercdt")
    
    # observeEvent(input$attachment_method, {
    #   input <- input$attachment_method
    #   if (input == "url") {
    #     attach.options("url")
    #   } else if (input == "file") {
    #     attach.options("file")
    #   }
    # })
  
  
  

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
    
    # shiny::tags$head(
    #   shiny::tags$style(HTML("
    #     .select-control.multi .select-input-multiple > div {
    #       background: #d8544c;
    #         color: #fdfdfd;
    #     }
    #   "))),
    
    tabPanel(
      "Projects", #* Projects ----
      h4("Edit input options"),
      # selectInput(
      #   "prj_select", "Select project", 
      #   choices = prj_levels()),
      # actionButton(
      #   "create_new_prj",
      #   "Add a new project",
      #   icon = icon("plus")
      # ),
      # conditionalPanel(
      #   condition = !is.null("input.create_new_prj"),
      #   textInput(
      #     "create_prj",
      #     "Create a new project",
      #     placeholder = paste(
      #       "Existing projects:", 
      #       paste(
      #         ferc$project %>% unique() %>% sort() %>% na.omit(),
      #         collapse = ", ")))),
      # submitButton(
      #   text = "Apply new project",
      #   icon = icon("save")
      # ),
      textInput(
        "add_prj",
        "Create a new project",
        placeholder = paste(
          "Existing projects:",
          paste(
            ferc$project %>% unique() %>% sort() %>% na.omit(),
            collapse = ", "))),
      actionButton(
        "submit",
        "Submit",
        icon("paper-plane"), 
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
    
    tabPanel(
      "Documents", 
      #* Documents ----
      helpText("Editable FERC Documents"),
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
      uiOutput("ferc_new")),
    
    tabPanel(
      "Publications",
      #* Publications ----
      helpText("Editable Tethys Publications"))
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
