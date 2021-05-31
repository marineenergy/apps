if (!require(librarian)){
  remotes::install_github("DesiQuintans/librarian")
  library(librarian)
}
shelf(
  DBI, dplyr, DT, fs, glue, here, htmltools, htmlwidgets, kableExtra, knitr, 
  leaflet, markdown, r-spatial/mapview, # https://github.com/r-spatial/mapview/issues/324
  purrr, readr,  RPostgres, rmarkdown, rvest, tibble, tidyr,
  scales, sf, shiny, sp, stringr, urltools, yaml)
# ggmap, gt, webshot, png

source(here::here("scripts/common.R"))
source(file.path(dir_scripts, "db.R"))
source(file.path(dir_scripts, "shp_mc.R"))
source(file.path(dir_scripts, "report-v1.R"))
source(file.path(dir_scripts, "update.R"))
