# if (!require(librarian)){
#   remotes::install_github("DesiQuintans/librarian")
#   library(librarian)
# }
# shelf(
#   DBI, dplyr, DT, fs, glue, here, RPostgres, stringr)

db_params <- list(
  dbname  = "gis",
  host    = "postgis",
  user    = "admin",
  pwd_txt = "/share/.password_mhk-env.us")

# Caleb's machine
if (Sys.info()[["sysname"]] != "Linux"){
  db_params <- list(
    dbname  = "dev",
    host    = "localhost",
    user    = "cgrant",
    pwd_txt = "../../pwd.txt")
}

# Ben's laptop
if (Sys.info()[["user"]] == "bbest" & Sys.info()[["sysname"]] == "Darwin"){
  db_params <- list(
    dbname  = "gis",
    host    = "marineenergy.app",
    user    = "admin",
    pwd_txt = "~/private/dbpass_marineenergy.app.txt") 
}

con <<- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname   = db_params$dbname,
  host     = db_params$host,
  port     = 5432,
  user     = db_params$user,
  password = readLines(db_params$pwd_txt))

# tbls <- dbListTables(con) %>% sort(); tbls

# helper functions ----

dbRenameTable <- function(con, old, new){
  if (old %in% dbListTables(con))
    DBI::dbExecute(con, glue("ALTER TABLE {old} RENAME TO {new}"))
}
#dbRenameTable(con, "cetmap_bia", "d_cetmap_bia")

dbSafeNames = function(names) {
  # make names db safe: no '.' or other illegal characters,
  # all lower case and unique
  names = gsub('[^a-z0-9]+','_', tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

drop_d <- function(d_tbl){
  DBI::dbSendQuery(con, glue("SELECT DropGeometryTable ('public','{d_tbl}');"))
}
# TODO: rename drop_d -> drop_tbl



