# https://rpubs.com/DavidFong/DTedit
# https://github.com/DavidPatShuiFong/DTedit/blob/master/inst/shiny_demo/app.R

# libraries ----
dir_scripts <<- here::here("scripts")
source(file.path(dir_scripts, "common_2.R"))
source(file.path(dir_scripts, "db.R"))

library(shiny)
# library(librarian)

if (!require(librarian)){
  remotes::install_github("DesiQuintans/librarian")
}
shelf(DT, DTedit, RSQLite)
  # DBI, dplyr, DT, fs, glue, here, htmltools, htmlwidgets, kableExtra, knitr, 
  # purrr, readr,  RPostgres, rmarkdown, rvest, tibble, tidyr,
  # scales, sf, shiny, sp, stringr, urltools, yaml)
# library(RSQLite)
# library(DTedit)

# ferc_docs example ----
# #38 read & write to ferc_docs and ferc_doc_tags

# ferc_docs

##### load ferc_docs
# conn <- dbConnect(RSQLite::SQLite(), "books.sqlite")
# conn <- dbConnect(RSQLite::SQLite(), "ferc_docs")
# ferc_docs <- readr::read_csv(here::here("data/ferc_docs.csv")) 

# get data ----
# get tables from db -- lazy tbls (no nrow)
tbl_ferc_docs     <- tbl(con, "ferc_docs")     # main field: key_interaction
tbl_ferc_doc_tags <- tbl(con, "ferc_doc_tags") # linked by rowid

# join ferc_docs & ferc_doc_tags
d_ferc_docs <- tbl_ferc_docs %>% 
  left_join(
    tbl_ferc_doc_tags,
    by = "rowid") %>% 
  collect() %>% # retrieves data into a LOCAL TIBBLE (instead of db connex)
  mutate(id = 1:nrow(.))
# d_ferc_docs <- d_ferc_docs %>% 
  # mutate(
  #   key_ixn_sub = glue(
  #     "{stringr::str_sub(
  #     d_ferc_docs$key_interaction_detail, 1L, 250)}..."))
d_ferc_docs <- data.frame(d_ferc_docs)
# add/update a single row?
# write whole table:
#   DBI::dbWriteTable(con, "")


# conn <- dbConnect(RSQLite::SQLite(), "books.sqlite")

# "ferc_docs" %in% dbListTables(con)

if (!'ferc_docs' %in% dbListTables(con) || isTRUE(getOption("shiny.testmode"))) {
  # the sqlite file doesn't have the right data
  # OR we are running in test mode (test mode -> reset the data)
  # books <- read.csv('https://raw.githubusercontent.com/DavidPatShuiFong/DTedit/master/inst/shiny_demo/books.csv', stringsAsFactors = FALSE)
  d_ferc_docs$tag_sql <- strsplit(d_ferc_docs$tag_sql, ';')
  d_ferc_docs$tag_sql <- lapply(d_ferc_docs$tag_sql, trimws) # Strip white space
  d_ferc_docs$tag_sql <- unlist(lapply(d_ferc_docs$tag_sql, paste0, collapse = ';'))
  d_ferc_docs$id      <- 1:nrow(d_ferc_docs) # can also use 'seq_length(nrow(books))'
  # d_ferc_docs$Date <- paste0(books$Date, '-01-01')
  dbWriteTable(con, "d_ferc_docs", d_ferc_docs, overwrite = TRUE)
}

getFERC <- function() {
  
  res <- dbSendQuery(con, "SELECT * FROM d_ferc_docs")
  ferc <- dbFetch(res)
  dbClearResult(res)
  d_ferc_docs$tag_sql <- strsplit(d_ferc_docs$tag_sql, ';')
  # books$Date <- as.Date(books$Date)
  d_ferc_docs$tag_sql <- as.factor(d_ferc_docs$tag_sql)
  return(d_ferc_docs)
}

# callback fxns ----

# *insert new records ----
ferc.insert.callback <- function(data, row) {
  
  query <- paste0(
    "INSERT INTO d_ferc_docs (tag_category, content_tag, tag_sql, id) VALUES(",
    "'", data[row,]$tag_category, "', ",
    "'", data[row,]$content_tag,  "', ",
    "'", paste0(data[row,]$tag_sql[[1]], collapse = ';'), "', ",
    "",  max(getFERC()$id) + 1, "' ",
    ")")
  
  print(query) # for debug
  res <- dbSendQuery(con, query)
  dbClearResult(res)
  return(getFERC())
  # newdata <- rbind(data, newdata)
  # return(newdata)
}

# books.insert.callback <- function(data, row) {
#   query <- paste0(
#     "INSERT INTO books (id, Authors, Date, Title, Publisher) VALUES (",
#     "", max(getBooks()$id) + 1, ", ",
#     "'", paste0(data[row,]$Authors[[1]], collapse = ';'), "', ",
#     "'", as.character(data[row,]$Date), "', ",
#     "'", data[row,]$Title, "', ",
#     "'", as.character(data[row,]$Publisher), "' ",
#     ")")
#   print(query) # For debugging
#   res <- dbSendQuery(conn, query)
#   dbClearResult(res)
#   return(getBooks())
# }

# *update existing records ----
ferc.update.callback <- function(data, olddata, row) {
  query <- paste0(
    "UPDATE d_ferc_docs 
    SET ",
          "tag_category = '", data[row,]$tag_category, "', ",
          "content_tag = '",  data[row,]$content_tag, "', ",
          "tag_sql = '",      paste0(data[row,]$tag_sql[[1]], collapse = ';'), "', ",
    "WHERE id = ",     data[row,]$id, ";")
  print(query) # For debugging
  res <- dbSendQuery(con, query)
  dbClearResult(res)
  return(getFERC())
  # newdata[row,] <- data[1,]
  # return(newdata)
}

# books.update.callback <- function(data, olddata, row) {
#   query <- paste0(
#     "UPDATE books SET ",
#     "Authors = '", paste0(data[row,]$Authors[[1]], collapse = ';'), "', ",
#     "Date = '", as.character(data[row,]$Date), "', ",
#     "Title = '", data[row,]$Title, "', ",
#     "Publisher = '", as.character(data[row,]$Publisher), "' ",
#     "WHERE id = ", data[row,]$id)
#   print(query) # For debugging
#   res <- dbSendQuery(conn, query)
#   dbClearResult(res)
#   return(getBooks())
# }




# books.delete.callback <- function(data, row) {
#   query <- paste0('DELETE FROM books WHERE id = ', data[row,]$id)
#   res <- dbSendQuery(conn, query)
#   dbClearResult(res)
#   return(getBooks())
# }

# *delete records ----
ferc.delete.callback <- function(data, row) {
  query <- paste0(
    "DELETE FROM d_ferc_docs 
    WHERE id = ", data[row,]$id)
  res <- dbSendQuery(con, query)
  dbClearResult(res)
  return(getFERC())
  # newdata <- data[-row]
  # d[row,] <- data[-row,]
  # return(newdata)
}


# ferc.callback.actionButton <- function(data, row, buttonID) {
#   # data - the current copy of 'thedata'
#   # row - the row number of the clicked button
#   # buttonID - the buttonID of the clicked button
#   print(data$key_interaction_detail)
#   
#   if (substr(buttonID, 1, nchar("random")) == "random") {
#     # in this demonstration, all the buttons are 'random'
#     # but it is possible to define more than one column of buttons
#     # data[row, "Quantity"] <- sample(1:10, 1)
#   }
#   return(data)
# }
  


# shiny server ----
server <- function(input, output, session) {
  d_ferc_docs <- getFERC()
  ferc_dt <- dtedit(
    input, output,
    name = 'ferc_dtedit',
    thedata = d_ferc_docs,
    edit.cols = c("tag_category", "content_tag", "tag_sql"),
    # edit.cols = c('Title', 'Authors', 'Date', 'Publisher'),
    edit.label.cols = c('Tag Category', 'Tag Content', 'Tag'),
    input.types = c(key_interaction_detail = 'textAreaInput'),
    input.choices = list(tag_sql = unique(unlist(d_ferc_docs$tag_sql))),
    view.cols = names(d_ferc_docs),
    callback.update = books.update.callback,
    callback.insert = books.insert.callback,
    callback.delete = books.delete.callback)
  
  names <- data.frame(
    Name = character(), Email = character(), Date = numeric(),
    Type = factor(levels = c('Admin', 'User')),
    stringsAsFactors = FALSE)
  names$Date <- as.Date(names$Date, origin = '1970-01-01')
  namesdt <- dtedit(input, output, name = 'names', names)
  
  data_list <- list() # exported list for shinytest
  shiny::observeEvent(ferc_dt$thedata, {
    data_list[[length(data_list) + 1]] <<- ferc_dt$thedata
  })
  shiny::exportTestValues(data_list = {data_list})
  
  cancel.onsessionEnded <- session$onSessionEnded(function() {
    DBI::dbDisconnect(conn)
  })
}

# # shiny server ----
# server <- function(input, output, session) {
#   # books <- getBooks()
#   ferc.dt <- dtedit(
#     input, output,
#     name = "ferc_edit",
#     thedata = d_ferc_docs,
#     edit.cols = names(d_ferc_docs),
#     edit.label.cols = names(d_ferc_docs),
#     view.cols = names(d_ferc_docs),
#     class = "cell-border strip compact hover",
#     datatable.options = list(
#       columnDefs = list(list(
#         autoWidth  = T,
#         targets = "_all",
#         render = JS(
#           "function(data, type, row, meta) {",
#           "return type === 'display' && data.length > 200 ?",
#           "'<span title=\"' + data + '\">' + data.substr(0, 200) + '...</span>' : data;",
#           "}")
#         # ,
#         # className = 'dt-body-left'
#         )),
#       LengthMenu = c(5, 30, 50),
#       pageLength = 10, 
#       server = T),

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
    
    # action.buttons = list(
    #   view.more = list(
    #     columnLabel = "test",
    #     buttonLabel = "View more",
    #     buttonPrefix = "random")),
    
    # input.types = c(key_interaction_detail = 'textAreaInput'),
    # input.choices = list(Authors = unique(unlist(books$Authors))),
    # selectize = TRUE,
    # modal.size = "m",
    # text.width = "100%",
    # textarea.width = "570px",
    # textarea.height = "200px",
    # defaultPageLength = 10,
    # title.delete = "Delete",
    # title.edit = "Edit",
    # title.add = "Add new",
    # label.delete = "Delete",
    # label.edit = "Edit",
    # label.add = "New",
    # label.copy = "Copy",
  #   show.delete = TRUE,
  #   show.update = TRUE,
  #   show.insert = TRUE,
  #   show.copy   = TRUE,
  #   callback.update = ferc.update.callback,
  #   callback.insert = ferc.insert.callback,
  #   callback.delete = ferc.delete.callback,
  #   # callback.actionButton = ferc.callback.actionButton
  # )
# }
  
  # names <- data.frame(
  #   Name = character(), Email = character(), Date = numeric(),
  #   Type = factor(levels = c('Admin', 'User')),
  #   stringsAsFactors = FALSE)
  # names$Date <- as.Date(names$Date, origin = '1970-01-01')
  # namesdt <- dtedit(input, output, name = 'names', names)
  # 
  # data_list <- list() # exported list for shinytest
  # shiny::observeEvent(booksdt$thedata, {
  #   data_list[[length(data_list) + 1]] <<- booksdt$thedata
  # })
  # shiny::exportTestValues(data_list = {data_list})
  # 
  # cancel.onsessionEnded <- session$onSessionEnded(function() {
  #   DBI::dbDisconnect(conn)
  # })
# }

# shiny ui ----
# ui <- fluidPage(
#   h3("FERC docs and tags"),
#   uiOutput("ferc_edit"),
#   hr(), 
#   # uiOutput('names')
# )

ui <- fluidPage(
  h3('FERC'),
  uiOutput('d_ferc_docs'),
  hr(), h3('Email Addresses'),
  uiOutput('names')
)


# shiny app ----
shinyApp(ui, server)









# 
# 
# ##### Load books data.frame as a SQLite database
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
# 
# getBooks <- function() {
#   res <- dbSendQuery(conn, "SELECT * FROM books")
#   books <- dbFetch(res)
#   dbClearResult(res)
#   books$Authors <- strsplit(books$Authors, ';')
#   books$Date <- as.Date(books$Date)
#   books$Publisher <- as.factor(books$Publisher)
#   return(books)
# }
# 
# ##### Callback functions.
# 
# #### Create the Shiny server
# server <- function(input, output, session) {
#   books <- getBooks()
#   booksdt <- dtedit(
#     input, output,
#     name = 'books',
#     thedata = books,
#     edit.cols = c('Title', 'Authors', 'Date', 'Publisher'),
#     edit.label.cols = c(
#       'Book Title', 'Authors', 'Publication Date', 'Publisher'
#     ),
#     input.types = c(Title = 'textAreaInput'),
#     input.choices = list(Authors = unique(unlist(books$Authors))),
#     view.cols = names(books)[c(5,1,3)],
#     callback.update = books.update.callback,
#     callback.insert = books.insert.callback,
#     callback.delete = books.delete.callback
#   )
# 
#   names <- data.frame(
#     Name = character(), Email = character(), Date = numeric(),
#     Type = factor(levels = c('Admin', 'User')),
#     stringsAsFactors = FALSE)
#   names$Date <- as.Date(names$Date, origin = '1970-01-01')
#   namesdt <- dtedit(input, output, name = 'names', names)
# 
#   data_list <- list() # exported list for shinytest
#   shiny::observeEvent(booksdt$thedata, {
#     data_list[[length(data_list) + 1]] <<- booksdt$thedata
#   })
#   shiny::exportTestValues(data_list = {data_list})
# 
#   cancel.onsessionEnded <- session$onSessionEnded(function() {
#     DBI::dbDisconnect(conn)
#   })
# }
# 
# ##### Create the shiny UI
# ui <- fluidPage(
#   h3('Books'),
#   uiOutput('books'),
#   hr(), h3('Email Addresses'),
#   uiOutput('names')
# )
# 
# shinyApp(ui = ui, server = server)
# 
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
# 
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
# #### Create the shiny UI
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
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ferc.insert.callback <- function(data, row) {
#   d <- rbind(data, d)
#   return(d)
# }
# ferc.update.callback <- function(data, olddata, row) {
#   d[row,] <- data[1,]
#   return(d)
# }
# 
# ferc.delete.callback <- function(data, row) {
#   d[row,] <- data[-row,]
#   return(d)
# }
# 
# getFERC <- function(ferc_docs) {
#   # res <- dbSendQuery(conn, "SELECT * FROM books")
#   # docs <- dbFetch(res)
#   # dbClearResult(res)
#   
#   con
#   
#   docs            <- ferc_docs
#   docs$doc        <- strsplit(docs$doc, ',')
#   docs$receptor   <- as.factor(docs$receptor)
#   docs$stressor   <- as.factor(docs$stressor)
#   docs$technology <- as.factor(docs$technology)
#   docs$phase      <- as.factor(docs$phase)
#   return(docs)
# }
# 
# docs <- getFERC(ferc_docs) 
# 
# # ui
# ui <- fluidPage(
#   h3('Tags'),
#   selectInput(
#     "cols", "columns", 
#     multiple = T, 
#     selected = names(docs),
#     choices  = names(docs)),
#   uiOutput('ferc_docs_edit'),
#   # hr(), h3('Email Addresses'),
#   # uiOutput('h')
# )
# 
# # create dtedit object within server fxn 
# server <- function(input, output, session) {
#   
#   observeEvent(input$cols, {
#     ferc_docs_editable <- DTedit::dtedit(
#       input, output,
#       name            = 'ferc_docs_edit',
#       thedata         = head(docs),
#       edit.cols       = c('receptor', 'stressor', 'technology', 'phase'),
#       # edit.label.cols = c('Receptor', 'Stressor', 'Technology', 'Phase'),
#       # input.types     = c(receptor  = 'textAreaInput'),
#       # view.cols       = c('receptor', 'stressor', 'technology', 'phase'),
#       view.cols       = input$cols,
#       callback.update = ferc.update.callback,
#       callback.insert = ferc.insert.callback,
#       callback.delete = ferc.delete.callback)
#     ferc_docs_editable
#   })
#   
#  
#   
# }
# 
# shinyApp(ui = ui, server = server)




#####
















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
# 
# getBooks <- function() {
#   res <- dbSendQuery(conn, "SELECT * FROM books")
#   books <- dbFetch(res)
#   dbClearResult(res)
#   books$Authors <- strsplit(books$Authors, ';')
#   books$Date <- as.Date(books$Date)
#   books$Publisher <- as.factor(books$Publisher)
#   return(books)
# }
# 
# ##### Callback functions.
# books.insert.callback <- function(data, row) {
#   query <- paste0(
#     "INSERT INTO books (id, Authors, Date, Title, Publisher) VALUES (",
#     "", max(getBooks()$id) + 1, ", ",
#     "'", paste0(data[row,]$Authors[[1]], collapse = ';'), "', ",
#     "'", as.character(data[row,]$Date), "', ",
#     "'", data[row,]$Title, "', ",
#     "'", as.character(data[row,]$Publisher), "' ",
#     ")")
#   print(query) # For debugging
#   res <- dbSendQuery(conn, query)
#   dbClearResult(res)
#   return(getBooks())
# }
# 
# ferc.update.callback <- function(data, olddata, row) {
#   query <- paste0("UPDATE books SET ",
#                   "Authors = '", paste0(data[row,]$Authors[[1]], collapse = ';'), "', ",
#                   "Date = '", as.character(data[row,]$Date), "', ",
#                   "Title = '", data[row,]$Title, "', ",
#                   "Publisher = '", as.character(data[row,]$Publisher), "' ",
#                   "WHERE id = ", data[row,]$id)
#   print(query) # For debugging
#   res <- dbSendQuery(conn, query)
#   dbClearResult(res)
#   return(getBooks())
# }
# 
# ferc.delete.callback <- function(data, row) {
#   query <- paste0('DELETE FROM books WHERE id = ', data[row,]$id)
#   res <- dbSendQuery(conn, query)
#   dbClearResult(res)
#   return(getBooks())
# }
# 
# ##### Create the Shiny server
# server <- function(input, output, session) {
#   books <- getBooks()
#   booksdt <- dtedit(
#     input, output,
#     name = 'books',
#     thedata = books,
#     edit.cols = c('Title', 'Authors', 'Date', 'Publisher'),
#     edit.label.cols = c(
#       'Book Title', 'Authors', 'Publication Date', 'Publisher'
#     ),
#     input.types = c(Title = 'textAreaInput'),
#     input.choices = list(Authors = unique(unlist(books$Authors))),
#     view.cols = names(books)[c(5,1,3)],
#     callback.update = books.update.callback,
#     callback.insert = books.insert.callback,
#     callback.delete = books.delete.callback
#   )
#   
#   names <- data.frame(
#     Name = character(), Email = character(), Date = numeric(),
#     Type = factor(levels = c('Admin', 'User')),
#     stringsAsFactors = FALSE)
#   names$Date <- as.Date(names$Date, origin = '1970-01-01')
#   namesdt <- dtedit(input, output, name = 'names', names)
#   
#   data_list <- list() # exported list for shinytest
#   shiny::observeEvent(booksdt$thedata, {
#     data_list[[length(data_list) + 1]] <<- booksdt$thedata
#   })
#   shiny::exportTestValues(data_list = {data_list})
#   
#   cancel.onsessionEnded <- session$onSessionEnded(function() {
#     DBI::dbDisconnect(conn)
#   })
# }
# 
# ##### Create the shiny UI
# ui <- fluidPage(
#   h3('Books'),
#   uiOutput('books'),
#   hr(), h3('Email Addresses'),
#   uiOutput('names')
# )
# 
# shinyApp(ui = ui, server = server)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ### books example ----
# 
# ##### Load books data.frame as a SQLite database
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
# 
# getBooks <- function() {
#   res <- dbSendQuery(conn, "SELECT * FROM books")
#   books <- dbFetch(res)
#   dbClearResult(res)
#   books$Authors <- strsplit(books$Authors, ';')
#   books$Date <- as.Date(books$Date)
#   books$Publisher <- as.factor(books$Publisher)
#   return(books)
# }
# 
# ##### Callback functions.
# books.insert.callback <- function(data, row) {
#   query <- paste0(
#     "INSERT INTO books (id, Authors, Date, Title, Publisher) VALUES (",
#     "", max(getBooks()$id) + 1, ", ",
#     "'", paste0(data[row,]$Authors[[1]], collapse = ';'), "', ",
#     "'", as.character(data[row,]$Date), "', ",
#     "'", data[row,]$Title, "', ",
#     "'", as.character(data[row,]$Publisher), "' ",
#     ")")
#   print(query) # For debugging
#   res <- dbSendQuery(conn, query)
#   dbClearResult(res)
#   return(getBooks())
# }
# 
# books.update.callback <- function(data, olddata, row) {
#   query <- paste0("UPDATE books SET ",
#                   "Authors = '", paste0(data[row,]$Authors[[1]], collapse = ';'), "', ",
#                   "Date = '", as.character(data[row,]$Date), "', ",
#                   "Title = '", data[row,]$Title, "', ",
#                   "Publisher = '", as.character(data[row,]$Publisher), "' ",
#                   "WHERE id = ", data[row,]$id)
#   print(query) # For debugging
#   res <- dbSendQuery(conn, query)
#   dbClearResult(res)
#   return(getBooks())
# }
# 
# books.delete.callback <- function(data, row) {
#   query <- paste0('DELETE FROM books WHERE id = ', data[row,]$id)
#   res <- dbSendQuery(conn, query)
#   dbClearResult(res)
#   return(getBooks())
# }
# 
# ##### Create the Shiny server
# server <- function(input, output, session) {
#   books <- getBooks()
#   booksdt <- dtedit(
#     input, output,
#     name = 'books',
#     thedata = books,
#     edit.cols = c('Title', 'Authors', 'Date', 'Publisher'),
#     edit.label.cols = c(
#       'Book Title', 'Authors', 'Publication Date', 'Publisher'
#     ),
#     input.types = c(Title = 'textAreaInput'),
#     input.choices = list(Authors = unique(unlist(books$Authors))),
#     view.cols = names(books)[c(5,1,3)],
#     callback.update = books.update.callback,
#     callback.insert = books.insert.callback,
#     callback.delete = books.delete.callback
#   )
# 
#   names <- data.frame(
#     Name = character(), Email = character(), Date = numeric(),
#     Type = factor(levels = c('Admin', 'User')),
#     stringsAsFactors = FALSE)
#   names$Date <- as.Date(names$Date, origin = '1970-01-01')
#   namesdt <- dtedit(input, output, name = 'names', names)
# 
#   data_list <- list() # exported list for shinytest
#   shiny::observeEvent(booksdt$thedata, {
#     data_list[[length(data_list) + 1]] <<- booksdt$thedata
#   })
#   shiny::exportTestValues(data_list = {data_list})
# 
#   cancel.onsessionEnded <- session$onSessionEnded(function() {
#     DBI::dbDisconnect(conn)
#   })
# }
# 
# ##### Create the shiny UI
# ui <- fluidPage(
#   h3('Books'),
#   uiOutput('books'),
#   hr(), h3('Email Addresses'),
#   uiOutput('names')
# )
# 
# shinyApp(ui = ui, server = server)

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
