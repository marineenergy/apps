# https://rpubs.com/DavidFong/DTedit
# https://github.com/DavidPatShuiFong/DTedit/blob/master/inst/shiny_demo/app.R

# DB CONNECTION ----
dir_scripts <<- here::here("scripts")
source(file.path(dir_scripts, "common_2.R"))
source(file.path(dir_scripts, "db.R"))

# LIBRARIES ----
library(shiny)
if (!require(librarian)){
  remotes::install_github("DesiQuintans/librarian")
  library(librarian)
}
shelf(DT, DBI, pool, DTedit, RSQLite)


# WRITE MERGED DATA TO DB ----
# (IF NOT ALREADY THERE)

# fxn to concatenate tags per rowid
mergeTags <- function(data) {
  data <- data %>% 
    filter(!is.na(content_tag)) %>% 
    distinct(content_tag, .keep_all = T)
  data <- data %>% 
    mutate(Tags = paste(data$content_tag, collapse = ";")) %>% 
    select(-content_tag) %>% 
    distinct(Tags, .keep_all = T)
}

# write data if not there already 
conn <- poolCheckout(con)
dbBegin(conn)

if (!"ferc" %in% dbListTables(conn)) {
  ferc_docs <- data.frame(tbl(conn, "ferc_docs")) %>% 
    select(
      rowid,
      Detail     = key_interaction_detail,
      Project    = project,
      doc_name   = 'doc.NAME',
      doc_attach = 'ATTACHMENT.NAME',
      doc_url    = url,
      ck_ixn     = presented_as_potential_interaction, 
      ck_obs     = decribed_from_observations_at_the_project_site, 
      ck_mp      = `monitoring_plan_.mp.`, 
      ck_amp     = `adaptive_management_plan_.amp.`, 
      ck_pme     = protection_mitigation_and_enhancement, 
      ck_bmps    = bmps_applied) %>% 
    mutate(
      Doc = ifelse(
        is.na(doc_attach),
        doc_name,
        paste0(doc_name, ": ", doc_attach)))
  ferc_tags <- data.frame(tbl(conn, "ferc_doc_tags"))
  ferc_docs_tags <- ferc_docs %>% left_join(ferc_tags, by = "rowid") %>%
    collect() %>% 
    distinct_all() %>% 
    group_by(across(c(-tag_category, -content_tag, -tag_sql))) %>% 
    select(-tag_category, -tag_sql) 
  ferc_docs_tags <- ferc_docs_tags %>% 
    na_if("NA") %>% 
    group_by(rowid) %>% 
    group_modify(~mergeTags(.x)) %>% 
    data.frame()
  dbWriteTable(conn, "ferc", ferc_docs_tags, overwrite = T)

  # ferc_docs     <- data.frame(tbl(conn, "ferc_docs"))
  # ferc_doc_tags <- data.frame(tbl(conn, "ferc_doc_tags"))
  # ferc_merged   <- ferc_docs %>% 
  #   left_join(ferc_doc_tags, by = "rowid") %>% 
  #   collect() %>% 
  #   group_by(across(.cols = c(-tag_category, -content_tag, -tag_sql))) %>% 
  #   select(-tag_category, -tag_sql) 
  # d <- ferc_merged %>% 
  #   na_if("NA") %>% 
  #   group_by(rowid) %>% 
  #   group_modify(~ mergeTags(.x)) %>% 
  #   data.frame()
  # # d <- d %>% mutate(id = 1:nrow(d)) 
  # dbWriteTable(conn, "ferc", d, overwrite = T)
}

dbCommit(conn)
poolReturn(conn)


# READ IN MERGED FERC TAGS DATA FROM DB ----
getLevels <- function(col) {
  col %>% factor(levels = c(unique(col)))
}

getFerc <- function() {
  conn <- poolCheckout(con) 
  dbBegin(conn)
  
  query <- "SELECT * from ferc"
  res  <- dbSendQuery(conn, query)
  ferc <- dbFetch(res)
  # View(dbFetch(dbSendQuery(conn, "SELECT * FROM ferc")))
  dbClearResult(res)
  
  # strsplit tags
  ferc <- ferc %>% 
    mutate(
      Tags    = Tags    %>% strsplit(";"),
      Project = Project %>% getLevels())
  
  ferc <- ferc %>% 
    mutate(
      across(starts_with("ck_"), as.character),
      across(starts_with("ck_"), recode, "TRUE"="✓", "FALSE"="☐"),
      Doc = case_when(
        is.na(doc_url)  ~ Doc,
        !is.na(doc_url) ~ as.character(glue('<a href="{doc_url}">{Doc}</a>')))) %>% 
    select(
      ID  = rowid, 
      Project, Doc, Detail, Tags,
      Ixn = ck_ixn, 
      Obs = ck_obs, 
      MP  = ck_mp, 
      AMP = ck_amp, 
      PME = ck_pme, 
      BMP = ck_bmps)
  
  dbCommit(conn)
  poolReturn(conn)
  return(ferc)
        
 
      
      # Doc = ifelse(
      #   # if doc_attach is na, doc = doc_name. else, doc = doc_attach
      #   is.na(doc_attach),
      #   doc_name,
      #   paste0(doc_name, ": ", doc_attach)),
      # Doc = ifelse(
      #   is.na(doc_url),
      #   Doc,
      #   glue("<a href='{doc_url}'>{Doc}</a>"))) %>% 


}


# CALLBACK FXNS ----
ferc.insert.callback <- function(data, row) {
  query <- paste0(
    "INSERT INTO ferc (ID, Detail, Tags, Project) VALUES (",
    "",  max(getFerc()$ID) + 1, ", ",
    "'", paste0(data[row,]$Detail), "', ",
    "'", paste0(data[row,]$Tags[[1]], collapse = ';'), "', ",
    "'", as.character(data[row,]$Project), "' ",
    ")")
  print(query) # For debugging
  
  res <- dbSendQuery(conn, query)
  dbClearResult(res)

  return(getFerc())
}

ferc.update.callback <- function(data, olddata, row) {
  query <- paste0(
    "UPDATE ferc SET ",
    "Detail = '",   data[row,]$Detail, "', ",
    "Tags = '",     paste0(data[row,]$Tags[[1]], collapse = ';'), "', ",
    "Project = '",  as.character(data[row,]$Project), "' ",
    "WHERE ID = '", data[row,]$ID)
  print(query) # For debugging
  
  res <- dbSendQuery(conn, query) 
  dbClearResult(res)
  
  return(getFerc())
}

ferc.delete.callback <- function(data, row) {
  query <- paste0('DELETE FROM ferc WHERE ID = ', data[row,]$ID)
  print(query) # debug
  
  res <- dbSendQuery(conn, query)
  dbClearResult(res)
  
  return(getFerc())
}


# SHINY SERVER ----
server <- function(input, output, session) {
  project.types <- levels(ferc$Project)
  # logical.types <- c("YES", "NO")
  tags.types    <- unique(unlist(ferc$Tags))
  
  ferc <- data.frame(getFerc(), stringsAsFactors = F)
  
  # style callback
  style.callback <- c(
    "$('#DataTables_Table_0_length select').css('background-color', 'gray');",
    "$('#DataTables_Table_0_filter input').css('background-color', 'gray');")

  
  # project: selectize callback
  # SLIDER = sapply(1:5, function(i) {
  #   sprintf('<input type="text" id="Slider%d" name="slider" value="5;15" />', i)
  # })
  # 
  # MULTIPLE_SELECT <- 
  #   '<select id="mselect" class="form-control" multiple="multiple">
  #     <option value=""></option>
  #     <option value="A">A</option>
  #     <option value="B">B</option>
  #     <option value="C">C</option>
  #    </select>'
  # 
  # js.selectize <- c(
  #   "function(settings){",
  #   "  $('[id^=Slider]').ionRangeSlider({",
  #   "    type: 'double',",
  #   "    grid: true,",
  #   "    grid_num: 10,",
  #   "    min: 0,",
  #   "    max: 20",
  #   "  });",
  #   "  $('#mselect').selectize()",
  #   "}"
  # )
  
  fercdt <- dtedit(
    input, output,
    name      = 'ferc',
    thedata   = ferc,
    view.cols = names(ferc),
    edit.cols = c(
      'Project', 'Doc', 'Detail', 'Tags', 
      'Ixn', 'Obs', 'MP', 'AMP', 'PME', 'BMP'),
    # edit.cols = c('Doc', 'Detail', 'Tags', 'Project'),
    edit.label.cols = c(
      'Project', 'Doc', 'Key Interaction Detail', 'Tags',
      'Presented as potential interaction?',
      'Described from observations at the project site?',
      'Monitoring plan (MP)?',
      'Adaptive management plan (AMP)?',
      'Protection mitigation and enhancement (PME)?',
      'Best management practices (BMPs) applied?'),
    input.types = c(
      Project = 'selectInput',
      Doc     = 'textInput',
      Detail  = 'textAreaInput',
      Tags    = 'selectInputMultiple',
      Ixn     = 'selectInput',
      Obs     = 'selectInput',
      MP      = 'selectInput',
      AMP     = 'selectInput',
      PME     = 'selectInput',
      BMP     = 'selectInput'),
    input.choices = list(
      Tags    = unique(unlist(ferc$Tags)),
      Project = levels(ferc$Project),
      Ixn     = c("YES", "NO"),
      Obs     = c("YES", "NO"),
      MP      = c("YES", "NO"),
      AMP     = c("YES", "NO"),
      PME     = c("YES", "NO"),
      BMP     = c("YES", "NO")),

    
    # input.choices.reactive = list(
    #   project.types.list = project.types),
    

      # project = list(
      #   placeholder = "Please select an option below",
      #   onInitialize = I('function() { this.setValue(""); }')
      
    
    # style
    datatable.rownames = F,
    
    datatable.call = function(...) {
      arguments <- list(...)
      arguments$escape   <- 1
      arguments$class    <- 'display'
      arguments$callback <- JS(callback = JS(style.callback))
      arguments$selection <- 'none'
      # arguments$options  <- 
      #   # bind inputs to shiny in order to use values from the input
      #   list(
      #     initComplete    = JS(js.selectize),
      #     preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      #     drawCallback    = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      #     )
      
      do.call(DT::datatable, arguments) %>%  
        DT::formatStyle('Doc', fontWeight = 'bold')  
          
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
    },
    
    datatable.options = list(
      searchHighlight = T,
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
    
    selectize = TRUE,
    
    selectize.options = list(
      Project = list(
        placeholder = "Please select an option below",
        onInitialize = I('function() { this.setValue(""); }')
      ),
      # Project  = list(create = TRUE, maxItems = 1),
      Doc      = list(create = TRUE, maxItems = 1)),
    
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
    title.add       = 'Add new',
    
    # callbacks
    callback.update = ferc.update.callback,
    callback.insert = ferc.insert.callback,
    callback.delete = ferc.delete.callback
    
    # options
    # datatable.options = list(pageLength = defaultPageLength)
  )
  
  # test dt
  # names <- data.frame(
  #   Name = character(), Email = character(), Date = numeric(),
  #   Type = factor(levels = c('Admin', 'User')),
  #   stringsAsFactors = FALSE)
  # names$Date <- as.Date(names$Date, origin = '1970-01-01')
  # namesdt <- dtedit(input, output, name = 'names', thedata = names)
  
  
  data_list <- list() # exported list for shinytest
 
  # observeEvent(input$choice, {
  #   if (input$choice != "Other") {
  #     project.types.list = project.types
  #   } else {
  #     project.types.list = "hello"
  #   }
  # })
  
  observeEvent(fercdt$thedata, {
    data_list[[length(data_list) + 1]] <<- fercdt$thedata
  })
  
  shiny::exportTestValues(data_list = {data_list})
  
  cancel.onsessionEnded <- session$onSessionEnded(function() {
    DBI::dbDisconnect(conn)
  })
} 


# SHINY UI ----
ui <- fluidPage(
  h1('FERC docs detail'),
  uiOutput("ferc"),
  hr()
  # hr(), h3('Email Addresses'),
  # uiOutput('names')
)


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


# TODO: 
# [x] remove tag_sql col & remove duplicate (unnecessary) rows
# [x] join all docs back together, now 1:1 ratio of doc:tag
# [ ] try DTedit again



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
#     "", max(getFERC$rowid) + 1, ", ",
#     "'", paste0(data[row,]$tag_sql_c[[1]], collapse = '; '), "', ",
#     ")")
#   print(query) # For debugging
#   res <- dbSendQuery(con, query)
#   dbClearResult(res)
#   return(getFERC())
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
#   return(getFERC())
# }
# 
# ferc.delete.callback <- function(data, row) {
#   query <- paste0('DELETE FROM d WHERE rowid = ', data[row,]$rowid)
#   res <- dbSendQuery(con, query)
#   dbClearResult(res)
#   return(getFERC())
# }
# 
# ##### Create the Shiny server
# server <- function(input, output, session) {
#   d <- getFERC()
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
