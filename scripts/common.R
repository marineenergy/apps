if (!require(librarian)){
  remotes::install_github("DesiQuintans/librarian")
  library(librarian)
}
shelf(
  dplyr, pool, shiny)

here <- here::here
glue <- glue::glue

os   <- Sys.info()[["sysname"]]
user <- Sys.info()[["user"]]
machine <<- dplyr::case_when(
  user == "bbest" & os == "Darwin" ~ "Ben",
  os   != "Linux"                  ~ "Caleb",
  T                                ~ "server")

dir_scripts <<- switch(
  machine,
  Ben = "~/github/marineenergy/apps_dev/scripts",
  "/share/github/apps_dev/scripts")

dir_data <<- switch(
  machine,
  Ben = "~/github/marineenergy/apps_dev/data",
  "/share/github/apps_dev/data")

dir_api <<- "/share/github/api"

get_gsheet_data <- function(sheet = "tags", sheet_id  = "https://docs.google.com/spreadsheets/d/1MTlWQgBeV4eNbM2JXNXU3Y-_Y6QcOOfjWFyKWfdMIQM/edit"){
  librarian::shelf(googlesheets4)
  
  # google sheet key from Google Console service account
  #   https://console.cloud.google.com/iam-admin/serviceaccounts/details/111453668228815650069/keys?authuser=2&organizationId=651265387478&project=marineenergy4gargle
  gs4_auth_json <- "/share/data/marineenergy4gargle.json" 
  # tags tab in [data | marineenergy.app - Google Sheet](https://docs.google.com/spreadsheets/d/1MTlWQgBeV4eNbM2JXNXU3Y-_Y6QcOOfjWFyKWfdMIQM/edit#gid=662531985)
  #   + shared sheet with: shares@marineenergy4gargle.iam.gserviceaccount.com
  # sheet_id  <- "1MTlWQgBeV4eNbM2JXNXU3Y-_Y6QcOOfjWFyKWfdMIQM"
  
  # googledrive
  stopifnot(file.exists(gs4_auth_json))
  gs4_auth(path = gs4_auth_json)
  
  # rename original tags
  # DBI::dbSendQuery(con, "ALTER TABLE tags RENAME TO tags_0;")
  # dbListTables(con) %>% sort()
  
  
  # read tags from gsheet
  read_sheet(sheet_id, sheet)
}
