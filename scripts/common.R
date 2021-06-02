if (!require(librarian)){
  remotes::install_github("DesiQuintans/librarian")
  library(librarian)
}
shelf(
  dplyr)

here <- here::here
glue <- glue::glue

os   <- Sys.info()[["sysname"]]
user <- Sys.info()[["user"]]
machine <<- dplyr::case_when(
  os   != "Linux"                  ~ "Caleb",
  user == "bbest" & os == "Darwin" ~ "Ben",
  T                                ~ "server")

dir_scripts <<- switch(
  machine,
  Ben = "~/github/marineenergy/apps_dev/scripts",
  "/share/github/apps_dev/scripts")

dir_data <<- switch(
  machine,
  Ben = "~/github/marineenergy/apps_dev/data",
  "/share/github/apps_dev/data")
