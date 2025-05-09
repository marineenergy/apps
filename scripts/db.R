librarian::shelf(
  dbplyr, dplyr, pool, shiny, stringr)

db_params <- switch(machine, # common.R:machine
  Caleb  =
    list(
      dbname  = "dev",
      host    = "localhost",
      user    = "cgrant",
      pwd_txt = "../../pwd.txt"),
  Ben =
    list(
      dbname  = "gis",
      host    = "marineenergy.app",
      user    = "admin",
      pwd_txt = "~/private/dbpass_marineenergy.app.txt"),
  list(
    dbname  = "gis",
    host    = "postgis",
    user    = "admin",
    pwd_txt = "/share/.password_mhk-env.us"))

# use conn only for special cases, like glue_data_sql() formatting
# conn <<- DBI::dbConnect(
#   RPostgres::Postgres(),
#   dbname   = db_params$dbname,
#   host     = db_params$host,
#   port     = 5432,
#   user     = db_params$user,
#   password = readLines(db_params$pwd_txt))

# use con on all other functions
# https://shiny.rstudio.com/articles/pool-advanced.html
con <- pool::dbPool(
  drv      = RPostgres::Postgres(),
  dbname   = db_params$dbname,
  host     = db_params$host,
  port     = 5432,
  user     = db_params$user,
  password = readLines(db_params$pwd_txt))

shiny::onStop(function() {
  suppressWarnings({
    pool::poolClose(con)
    #DBI::dbDisconnect(conn)
    })
})

# use conn to preview SQL, but con for st_read() to get spatial geometries
# conn <<- connections::connection_open(
#   RPostgres::Postgres(),
#   dbname   = db_params$dbname,
#   host     = db_params$host,
#   port     = 5432,
#   user     = db_params$user,
#   password = readLines(db_params$pwd_txt))
# instead of above, use following pattern: conn <- poolCheckout(con); ...; poolReturn(conn)
# conn <- poolCheckout(con)
# sql_insert_docs <- glue_data_sql(
#   d_docs,
#   "INSERT INTO ferc_docs VALUES
#       ({rowid}, {detail}, {project},
#       {prj_document}, {prj_doc_attachment}, {prj_doc_attach_url},
#       {ck_ixn}, {ck_obs}, {ck_mp}, {ck_amp}, {ck_pme}, {ck_bmps})",
#   .con = conn)
# poolReturn(conn)

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
