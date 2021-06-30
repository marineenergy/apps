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

dir_data    <<- here::here("data")

