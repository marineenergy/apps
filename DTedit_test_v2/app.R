# https://rpubs.com/DavidFong/DTedit
# https://github.com/DavidPatShuiFong/DTedit/blob/master/inst/shiny_demo/app.R

# TODO: 
# - [ ] make sure update/add/new 
# - [ ] color code tags
# - [ ] fix db connex so can save added/updated/deleted ferc records 
# - [ ] option to add new prj (`create = T` not working)
# - [ ] upgrade to forked copy with checkbox
#       https://github.com/DavidPatShuiFong/DTedit/issues/12#issuecomment-715228402

# DB CONNECTION ----
dir_scripts <<- here::here("scripts")
source(file.path(dir_scripts, "common_2.R"))
source(file.path(dir_scripts, "db.R"))
source(file.path(dir_scripts, "shiny_report.R"))

# LIBRARIES ----
shelf(DavidPatShuiFong/DTedit, DT, glue)

# WRITE MERGED DATA TO DB ----
# (IF NOT ALREADY THERE)

# for tags_tbl, containing 1 col called tag_sql
merge_tags <- function(tag_tbl, tag_sql) {
  tag_tbl %>% 
    # filter(!is.na(col_name)) %>% 
    distinct(tag_sql, .keep_all = TRUE) %>% 
    pull(tag_sql)
}


# ex_doc <- dbReadTable(con, "ferc_docs") %>% 
#   tibble() %>% 
#   collect() %>% 
#   left_join(
#     tbl(con, "ferc_doc_tags") %>% 
#       collect() %>% 
#       group_by(rowid) %>% 
#       tidyr::nest(
#         Tag_sql = tag_sql,
#         Tag_category = tag_category,
#         Content_tag = content_tag)) %>% 
#   mutate(Tag_sql    = Tag_sql %>% purrr::map(merge_tags))





# prj_tags <- get_ferc()$Tags[[1]]
# ferc_test <- get_ferc()
# # we want to map this across each project record
# group_tags <- function(prj_tags) {
#   prj_tags <- prj_tags %>%   
#     filter(!is.na(tag_sql)) %>% 
#     distinct(tag_sql)
#   paste(prj_tags$tag_sql, collapse = ";")
# }
# 
#   
#   ferc_docs_tags <- ferc_docs_tags %>% 
#     na_if("NA") %>%
#     group_by(rowid) %>%
#     group_modify(~mergeTags(.x)) %>%
#     data.frame()
#   # t <- tibble(tags_string = paste(prj_tags$tag_sql, collapse = ";"))
# 
# 
# ferc_test <- ferc_test %>% 
#   mutate(Tags_collapsed = purrr::map(ferc_test$Tags, group_tags))
# 
# 
# group_tags(ferc_test$Tags[[4]]) 
# 
# mapx <- purrr::map(ferc_test$Tags, group_tags)
# 
# 
# 
# View(ferc_test)



# group_tags <- function(data) {
#   data <- data %>%
#     filter(!is.na(content_tag)) %>%
#     distinct(content_tag, .keep_all = T)
#   data <- data %>%
#     mutate(Tags = paste(data$content_tag, collapse = ";")) %>%
#     select(-content_tag) %>%
#     distinct(Tags, .keep_all = T)
# }

# Don't need this step because taking directly from ferc_docs & ferc_tags in db
# if (!"ferc" %in% dbListTables(con)) {
#   ferc_docs <- data.frame(tbl(con, "ferc_docs")) %>% 
#     select(
#       rowid,
#       Detail     = key_interaction_detail,
#       Project    = project,
#       doc_name   = 'doc.NAME',
#       doc_attach = 'ATTACHMENT.NAME',
#       doc_url    = url,
#       ck_ixn     = presented_as_potential_interaction, 
#       ck_obs     = decribed_from_observations_at_the_project_site, 
#       ck_mp      = `monitoring_plan_.mp.`, 
#       ck_amp     = `adaptive_management_plan_.amp.`, 
#       ck_pme     = protection_mitigation_and_enhancement, 
#       ck_bmps    = bmps_applied) %>% 
#     mutate(
#       Doc = ifelse(
#         is.na(doc_attach),
#         doc_name,
#         paste0(doc_name, ": ", doc_attach)))
#   ferc_tags <- data.frame(tbl(con, "ferc_doc_tags"))
#   ferc_docs_tags <- ferc_docs %>% left_join(ferc_tags, by = "rowid") %>%
#     collect() %>% 
#     distinct_all() %>% 
#     group_by(across(c(-tag_category, -content_tag, -tag_sql))) %>% 
#     select(-tag_category, -tag_sql) 
#   ferc_docs_tags <- ferc_docs_tags %>% 
#     na_if("NA") %>% 
#     group_by(rowid) %>% 
#     group_modify(~mergeTags(.x)) %>% 
#     data.frame()
#   dbWriteTable(con, "ferc", ferc_docs_tags, overwrite = T)
# }



# READ IN MERGED FERC TAGS DATA FROM DB ----
get_levels <- function(col) {
  col %>% factor(levels = c(unique(col)))
}


# test <- dbReadTable(con, "ferc_docs") %>% 
#   tibble() %>% 
#   collect() %>% 
#   left_join(
#     tbl(con, "ferc_doc_tags") %>% 
#       select(rowid, tag_sql) %>% 
#       collect() %>% 
#       tidyr::nest(tags = tag_sql))
#       # mutate(
#       #   tags = as.vector(tags)))

get_ferc <- function() {
 dbReadTable(con, "ferc_docs") %>% 
    tibble() %>% 
    collect() %>% 
    left_join(
      tbl(con, "ferc_doc_tags") %>% 
        select(rowid, tag_sql) %>% 
        collect() %>% 
        group_by(rowid) %>% 
        tidyr::nest(
          tag_sql = tag_sql)) %>% 
    na_if("NA") %>% 
    mutate(
      #Tags    = Tags    %>% strsplit(";"), # TODO: left_join(tbl(con, "ferc_doc_tags"), by="rowid")
      ID         = rowid,
      Detail     = key_interaction_detail,
      Project    = project %>% get_levels(),
      doc_name   = `doc.NAME`,
      doc_attach = `ATTACHMENT.NAME`,
      URL        = url,
      Tag_sql    = Tag_sql %>% purrr::map(merge_tags),
      ck_ixn     = presented_as_potential_interaction,
      ck_obs     = decribed_from_observations_at_the_project_site,
      ck_mp      = `monitoring_plan_.mp.`,
      ck_amp     = `adaptive_management_plan_.amp.`,
      ck_pme     = protection_mitigation_and_enhancement,
      ck_bmps    = bmps_applied) %>%
    mutate(
      across(starts_with("ck_"), as.character),
      across(starts_with("ck_"), recode, "TRUE"="✓", "FALSE"="☐"),
      Doc = case_when(
        is.na(URL)  ~ doc_name,
        !is.na(URL) ~ as.character(glue('<a href="{URL}">{doc_name}</a>')))) %>% 
    select(
      ID  = rowid, 
      Project, Doc, URL, Detail, Tag_sql, Tag_category, Content_tag,
      Content    = content,
      Attachment = doc_attach,
      Ixn = ck_ixn, 
      Obs = ck_obs, 
      MP  = ck_mp, 
      AMP = ck_amp, 
      PME = ck_pme, 
      BMP = ck_bmps) %>% 
    data.frame()
}



# get_ferc <- function() {
#   dbReadTable(con, "ferc_docs") %>% 
#     tibble() %>% 
#     left_join(
#       dbReadTable(con, "ferc_doc_tags") %>% 
#         tibble() %>% 
#         select(rowid, tag_sql) %>% 
#         collect() %>% 
#         tidyr::nest(tags = tag_sql) %>% 
#         mutate(tags = as.vector(tags))) %>% 
#     rename(
#       ID         = rowid,
#       Detail     = key_interaction_detail,
#       Project    = project,
#       doc_name   = `doc.NAME`,
#       doc_attach = `ATTACHMENT.NAME`,
#       ck_ixn     = presented_as_potential_interaction, 
#       ck_obs     = decribed_from_observations_at_the_project_site, 
#       ck_mp      = `monitoring_plan_.mp.`, 
#       ck_amp     = `adaptive_management_plan_.amp.`, 
#       ck_pme     = protection_mitigation_and_enhancement, 
#       ck_bmps    = bmps_applied) %>% 
#     mutate(
#       across(starts_with("ck_"), as.character),
#       across(starts_with("ck_"), recode, "TRUE"="✓", "FALSE"="☐"),
#       Doc = case_when(
#         is.na(url)  ~ doc_name,
#         !is.na(url) ~ as.character(glue('<a href="{url}">{doc_name}</a>')))) %>% 
#     select(
#       ID, Project, Doc, Detail,
#       URL        = url, 
#       Attachment = doc_attach,
#       Tags       = tags,
#       Ixn        = ck_ixn, 
#       Obs        = ck_obs, 
#       MP         = ck_mp, 
#       AMP        = ck_amp, 
#       PME        = ck_pme, 
#       BMP        = ck_bmps) %>%
#     collect()
# }


# conn <- poolCheckout(con)
# dbBegin(con)

# dbCommit(conn)
# poolReturn(conn)

# GET INPUT CHOICES ----
unique.tags <- get_tags() %>% pull(tag_sql)




# * tags options ----
# ferc_tags_unique <- dbReadTable(con, "ferc_doc_tags") %>% 
#   tibble() %>% 
#   collect() %>% 
#   select(-rowid, -content) %>%
#   na_if("NA") %>% 
#   distinct_all() %>% 
#   mutate(tag_sql = tag_sql %>% as.character()) 
#   # explain() # see SQL
# ferc_tags_unique <- ferc_tags_unique %>% 
#   mutate(
#     tag_sql = case_when(
#       (is.na(tag_sql) & is.na(content_tag)) ~ glue("{tag_category}.{NA}") %>% 
#         as.character(),
#       (is.na(tag_sql)) ~ glue("{tag_category}.{content_tag}") %>% 
#         as.character(),
#       !(is.na(tag_sql)) ~ tag_sql)) %>% 
#   group_by(tag_category) %>% 
#   arrange(content_tag, .by_group = TRUE)
# 
# tags.types <- ferc$Tags %>% bind_rows() %>% unique() %>% tibble() %>% 
#   mutate(tag_sql = tag_sql %>% as.character()) %>% 
#   left_join(ferc_tags_unique, by = "tag_sql", copy = F) %>% 
#   group_by(tag_category) %>% 
#   arrange(content_tag, .by_group = TRUE)
# 
# tags_metadata <- list(content_tag = tags.types) %>% 
#   bind_rows() %>% 
#   left_join(ferc_tags_unique, by = "content_tag", copy = F) %>% 
#   group_by(tag_category) %>% 
#   arrange(content_tag, .by_group = TRUE)


# CALLBACK FXNS ----
ferc.insert.callback <- function(data, row) {
  query <- paste0(
    "INSERT INTO ferc_docs (rowid, key_interaction_detail, project) VALUES (",
    "",  max(get_ferc()$ID) + 1, ", ",
    "'", paste0(data[row,]$Detail), "', ",
    # "'", paste0(data[row,]$Tags[[1]], collapse = ';'), "', ",
    "'", as.character(data[row,]$Project), "' ",
    ")")
  print(query) # For debugging
  # res <- dbSendQuery(conn, query)
  res <- try(dbExecute(con, query))
  if ("try-error" %in% class(res)) stop(res)
  dbClearResult(res)
  get_ferc()
}

ferc.update.callback <- function(data, olddata, row) {
  browser()

  # row to be updated
  # dr <- data %>% slice(row)
  dr <- data %>% filter(ID == row)
  
  # row to be updated w/ tags unnested
  dr_tags <- dr %>% 
    filter(ID == row) %>% 
    select(Tag_sql) %>% 
    tidyr::unnest(tag_sql = Tag_sql) %>% 
    select(-Tag_sql)
  
  query_fd <- glue_data_sql(
    dr,
    "UPDATE ferc_docs 
    SET
      key_interaction_detail = '{Detail}',
      project                = '{Project}'
    WHERE rowid = {ID}", 
    .con = conn)
  
  
  query_ft <- glue_data_sql(
    dr_tags,
    "DELETE FROM ferc_tags
    WHERE rowid = {ID};
    
    UPDATE ferc_doc_tags 
    SET
      tag_sql      = '{Tags}',
      tag_category = '{Tag_category}',
      content_tag  = '{Content_tag}'
    WHERE 
      rowid = {ID}", 
    .con = conn)
  
  # ft <- tbl(con, "ferc_doc_tags") %>% collect()
  # ft_filt <- ft %>% filter(rowid == row) # to compare
  
  
  print(query_fd)
  print(query_ft)
  
  res <- try(dbExecute(con, query_fd))
  if ("try-error" %in% class(res)) stop(res)
  # res <- dbSendQuery(con, query)
  dbClearResult(res)
  
  res <- try(dbExecute(con, query_ft))
  if ("try-error" %in% class(res)) stop(res)
  dbClearResult(res)
  
  get_ferc()
  

  # TODO: delete rows before adding updates; 
  # also: add index so rowid&tag_sql always unique
  
  
  
  # dbAppendTable(con, "ferc_docs", iris)
  # SET
  #   key_interaction_detail = '{Detail}',n
  #   project                = '{Project}'
  # WHERE rowid = {`ID`}", .con = conn) %>% 
}
  
  
  
  
  

  
  # query <- paste0(
  #   "UPDATE ferc_docs fd, ferc_doc_tags ft ",
  #   "SET ",
  #   "T1.key_interaction_detail = '",   data[row,]$Detail, "', ",
  #   #"Tags = '",     paste0(data[row,]$Tags[[1]], collapse = ';'), "', ",
  #   "T1.project = '", as.character(data[row,]$Project), "' ",
  #   # NEED TO FIX THIS
  #   "T2.tag_sql = '", paste0(data[row,]$Tags[[1]], collapse = ';'),
  #   "WHERE rowid = ", data[row,]$ID)
  # query <- paste0(
  #   "UPDATE ferc_docs SET ",
  #   "key_interaction_detail = '",   data[row,]$Detail, "', ",
  #   #"Tags = '",     paste0(data[row,]$Tags[[1]], collapse = ';'), "', ",
  #   "project = '",    as.character(data[row,]$Project), "' ",
  #   "WHERE rowid = ", data[row,]$ID)


ferc.delete.callback <- function(data, row) {
  query <- glue_sql(
    "DELETE FROM ferc_docs WHERE rowid = {ID};
     DELETE FROM ferc_doc_tags WHERE rowid = {ID}", 
    .con = conn)
  
    # "DELETE FROM ferc_docs WHERE rowid = ", data[row,]$ID)
  print(query) # debug
  # res <- dbSendQuery(con, query)
  res <- try(dbExecute(con, query))
  if ("try-error" %in% class(res)) stop(res)
  dbClearResult(res)
  get_ferc()
}

# SHINY SERVER ----
server <- function(input, output, session) {
  
  ferc <- get_ferc()
  tags  <- get_tags()
  ferc_doc_names <- dbReadTable(con, "ferc_docs") %>% names()
  ferc_tag_names <- dbReadTable(con, "ferc_doc_tags") %>% names()
  
  fercdt <- dtedit(
    input, output,
    name      = 'ferc_new',
    thedata   = get_ferc(),
    view.cols = names(
      ferc %>% select(-Content, -URL, -Tag_category, -Content_tag)),
    edit.cols = c(
      'Project', 'Doc', 'URL', 'Attachment', 'Detail', 
      'Tag_sql', 'Ixn', 'Obs', 'MP', 'AMP', 'PME', 'BMP'),
    # edit.cols = c('Doc', 'Detail', 'Tags', 'Project'),
    edit.label.cols = c(
      'Project', 'Doc name', 'Doc URL', 
      'Upload document attachment',
      'Key Interaction Detail', 
      'Tags',
      'Presented as potential interaction?',
      'Described from observations at the project site?',
      'Monitoring plan (MP)?',
      'Adaptive management plan (AMP)?',
      'Protection mitigation and enhancement (PME)?',
      'Best management practices (BMPs) applied?'),
    input.types = c(
      Project     = 'selectInput',
      Doc         = 'textInput',
      URL         = 'textInput',
      Attachment  = 'fileInput',
      Detail      = 'textAreaInput',
      Tag_sql     = 'selectInputMultiple',
      Ixn         = 'selectInput',
      Obs         = 'selectInput',
      MP          = 'selectInput',
      AMP         = 'selectInput',
      PME         = 'selectInput',
      BMP         = 'selectInput'),
    input.choices = list(
      Tag_sql =  tags %>% pull(tag_sql),
      # Tags    = tags$tag_sql,
      Project = ferc$Project %>% levels(),
      Ixn     = c("YES", "NO"),
      Obs     = c("YES", "NO"),
      MP      = c("YES", "NO"),
      AMP     = c("YES", "NO"),
      PME     = c("YES", "NO"),
      BMP     = c("YES", "NO")),
    # input.choices.reactive = list(attach.types.list = attach.options),
    
    datatable.rownames = F,
    datatable.call     = function(...) {
      arguments <- list(...)
      arguments$escape   <- 1
      arguments$class    <- 'display'
      # arguments$callback <- JS(callback = JS(style.callback))
      do.call(DT::datatable, arguments) %>%
        DT::formatStyle('Doc',  fontWeight = 'bold')
    },

    datatable.options = list(
      searchHighlight = T,
      
      # testing js formatting options
      initComplete = JS("
      function(setings, json) {
        $(this.api().table().header()).css({
          'background-color': '#2b2d2f',
          'color': '#fff'
         });
      }"),
      autowidth  = T,
      columnDefs = list(list(className = 'dt-center', targets = 0)),
      pageLength = 5,
      lengthMenu = c(5, 10, 25, 50, 100, nrow(ferc))),
    
    selectize = TRUE,
    # selectize.options = list(
    #   Project = list(
    #     placeholder = "Please select an option below",
    #     onInitialize = I('function() { this.setValue(""); }')),
    # Doc = list(create = TRUE, maxItems = 1)),
    
    modal.size      = 'm', 
    textarea.width  = '500px',
    textarea.height = '200px',
    select.width    = '100%', 
    
    icon.delete     = icon("trash"),
    icon.edit       = icon("edit"),
    icon.add        = icon("plus"),
    icon.copy       = icon("copy"),
    
    title.delete    = 'Delete',
    title.edit      = 'Edit',
    title.add       = 'Add new key interaction detail',
    
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


# SHINY UI ----
ui <- fluidPage(
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  titlePanel("Editable FERC Docs Table"),
  div(
    "Filters by:",
    icon("tags"),
    span(class="me-tag me-technology", "Technology"),
    span(class="me-tag me-stressor",   "Stressor"),
    span(class="me-tag me-receptor",   "Receptor "),
    span(class="me-tag me-phase",      "Phase")),
  helpText(
    HTML("The FERC eLibrary contains environmental compliance project documents, 
          of which excerpts have been manually tagged for reference.")),
  fluidRow(
    column(
      width = 12,
      # radioButtons(
      #    "attachment_method",
      #    "Select a method to attach document",
      #    choiceValues = list("url", "file"),
      #    choiceNames  = list(
      #      tags$div(
      #        span(tagList(icon("link"), "Attach URL link"))),
      #      tags$div(
      #        span(tagList(icon("upload"), "Upload file"))))),
         # selected = NULL,
         # inline = F,
      hr())),
  uiOutput("ferc_new"))

# GLOBAL ----

# global <- function() {
#   con <<- pool::dbPool(
#     drv      = RPostgres::Postgres(),
#     dbname   = db_params$dbname,
#     host     = db_params$host,
#     port     = 5432,
#     user     = db_params$user,
#     password = readLines(db_params$pwd_txt))
#   onStop(function() {
#     message("before close: is valid? ", dbIsValid(con))
#     pool::poolClose(con)
#     message("after close: is valid? ", dbIsValid(con))
#   })
# }
  

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
# tbl_ferc_doc_tags <- tbl(con, "ferc_doc_tags") # linked by rowid

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
