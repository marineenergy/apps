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
# * functions ----
mergeTags <- function(data) {
  data <- data %>% 
    filter(!is.na(tag_sql)) %>% 
    distinct(tag_sql, .keep_all = T)
  data <- data %>% 
    mutate(tag_sql_c = paste(data$tag_sql, collapse = ";")) %>% 
    select(-tag_sql) %>% 
    distinct(tag_sql_c, .keep_all = T)
}

getLevels <- function(col) {
  col %>% factor(levels = c(unique(col)))
}

getFerc <- function() {
  conn <- poolCheckout(con)
  dbBegin(conn)
  
  res  <- dbSendQuery(conn, "SELECT * FROM ferc")
  ferc <- dbFetch(res)
  dbClearResult(res)
  
  # strsplit tags
  ferc <- ferc %>% 
    mutate(
      tag_sql  = tag_sql_c %>% strsplit(";"),
      project  = project   %>% getLevels(),
      doc.NAME = doc.NAME  %>% getLevels()) %>% 
    select(-tag_sql_c)
  
  dbCommit(conn)
  poolReturn(conn)
  return(ferc)
}

# * write ferc data if not already there ----
conn <- poolCheckout(con)
dbBegin(conn)

if (!"ferc" %in% dbListTables(conn)) {
  ferc_docs     <- data.frame(tbl(conn, "ferc_docs"))
  ferc_doc_tags <- data.frame(tbl(conn, "ferc_doc_tags"))
  ferc_merged   <- ferc_docs %>% 
    left_join(ferc_doc_tags, by = "rowid") %>% 
    collect() %>% 
    group_by(across(.cols = c(-tag_category, -content_tag, -tag_sql))) %>% 
    select(-tag_category, -content_tag) 
  d <- ferc_merged %>% 
    group_by(rowid) %>% 
    group_modify(~ mergeTags(.x)) %>% 
    data.frame()
  # d <- d %>% mutate(id = 1:nrow(d)) 
  dbWriteTable(conn, "ferc", d, overwrite = T)
}

dbCommit(conn)
poolReturn(conn)



# CALLBACK FXNS ----
ferc.insert.callback <- function(data, row) {
  query <- paste0(
    "INSERT INTO ferc (rowid, key_interaction_detail, tag_sql, project) VALUES (",
    "",  max(getFerc()$rowid) + 1, ", ",
    "'", paste0(data[row,]$key_interaction_detail), "', ",
    "'", paste0(data[row,]$tag_sql[[1]], collapse = ';'), "', ",
    "'", as.character(data[row,]$project), "' ",
    ")")
  print(query) # For debugging
  res <- dbSendQuery(conn, query)
  dbClearResult(res)
  return(getFerc())
}

ferc.update.callback <- function(data, olddata, row) {
  query <- paste0(
    "UPDATE ferc SET ",
    "key_interaction_detail = '", data[row,]$key_interaction_detail, "', ",
    "tag_sql = '", paste0(data[row,]$tag_sql[[1]], collapse = ';'), "', ",
    "project = '", as.character(data[row,]$project), "' ",
    "WHERE rowid = '", data[row,]$rowid)
  print(query) # For debugging
  res <- dbSendQuery(conn, query) 
  dbClearResult(res)
  return(getFerc())
}

ferc.delete.callback <- function(data, row) {
  query <- paste0('DELETE FROM ferc WHERE rowid = ', data[row,]$rowid)
  print(query) # debug
  res <- dbSendQuery(conn, query)
  dbClearResult(res)
  return(getFerc())
}


# SHINY SERVER ----
server <- function(input, output, session) {
  
  ferc <- data.frame(getFerc(), stringsAsFactors = F)
  project.types <- levels(ferc$project)
  
  fercdt <- dtedit(
    input, output,
    name = 'ferc',
    thedata = ferc,
    view.cols = names(ferc)[c(1:4, 14)],
    edit.cols = c('doc.NAME', 'key_interaction_detail', 'tag_sql', 'project'),
    edit.label.cols = c(
      'Doc Name', 'Key Interaction', 'Tags', 'Project'),
    input.types = c(
      key_interaction_detail = 'textAreaInput',
      project  = 'selectInput',
      doc.NAME = 'selectInput'),
    input.choices = list(
      tag_sql  = unique(unlist(ferc$tag_sql)),
      project  = levels(ferc$project),
      doc.NAME = levels(ferc$doc.NAME)),
    
    # input.choices.reactive = list(
    #   project.types.list = project.types),
    

      # project = list(
      #   placeholder = "Please select an option below",
      #   onInitialize = I('function() { this.setValue(""); }')
      
  
    
    # style
    datatable.rownames = F,
    datatable.call = function(...) {
      DT::datatable(...) %>%
        DT::formatStyle(
          'doc.NAME', fontWeight = 'bold'
        )
    },
    
    selectize = TRUE,
    selectize.options = list(
      project  = list(create = TRUE, maxItems = 1),
      doc.NAME = list(create = TRUE, maxItems = 1)),
    
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
    title.add       = 'Add new interaction',
    
    # callbacks
    callback.update = ferc.update.callback,
    callback.insert = ferc.insert.callback,
    callback.delete = ferc.delete.callback,
    
    # options
    # datatable.options = list(pageLength = defaultPageLength)
  )
  
  names <- data.frame(
    Name = character(), Email = character(), Date = numeric(),
    Type = factor(levels = c('Admin', 'User')),
    stringsAsFactors = FALSE)
  names$Date <- as.Date(names$Date, origin = '1970-01-01')
  namesdt <- dtedit(input, output, name = 'names', thedata = names)
  
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
  h3('FERC Docs & Tags'),
  uiOutput("ferc"),
  hr(), h3('Email Addresses'),
  uiOutput('names')
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
