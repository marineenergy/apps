library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(markdown)
library(leaflet)
library(mapedit)
library(sf)
library(here)
library(dplyr)
library(tidyr)
library(readr)
library(glue)
library(yaml)
library(geojsonsf)
library(rhandsontable)
library(DBI)
library(RPostgres)
library(knitr)
library(kableExtra)

#library(geosphere)
#data(merc)

# TODO: 
# - handle login (see )
# - Report as Rmd; download as html, docx, pdf, pptx
# - Get report_map to work
# - save to [session-id]/config.yml (with saved=T/F), study.geojson,...
#   - session-id: glue("{date::now()}_{tools::md5sum(files)}")
#   - bookmark state, save url, restore
#   - remove old saved=F

# setwd(here("draw-site"))

# tech_choices: csv to list; for selTech
tech <- read_csv("data/tech_choices.csv")
d <- tech %>% 
  group_by(tech1) %>% 
  summarize(
    tech2 = paste(sort(tech2), collapse = "|"))
tech_choices <- pull(d, tech2) %>% strsplit("\\|")
names(tech_choices) <- pull(d, tech1)

m <- leaflet() %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  setView(-93.4, 37.4, 4)

s_r_csv       <- here("data/tethys_stressor_receptor.csv")
# TODO: make per user session
s_r_ckbox_csv <- here("data/shiny_stressor_receptor_ckbox.csv")

if (!file.exists(s_r_ckbox_csv)){
  read_csv(s_r_csv) %>% 
    mutate(
      ckbox = F) %>% 
    select(stressor, receptor, ckbox) %>% 
    pivot_wider(names_from = "stressor", values_from = ckbox) %>% 
    write_csv(s_r_ckbox_csv) 
}

# connect to database
pass <- readLines("/share/.password_mhk-env.us")
con  <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname   = "gis",
  host     = "postgis",
  port     = 5432,
  user     = "admin",
  password = pass)
