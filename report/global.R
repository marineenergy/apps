library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(markdown)
library(leaflet)
library(mapedit)
library(sf)
library(here)
library(dplyr)
library(tidyr)
library(readr)
library(glue)

# TODO: 
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

map <- leaflet() %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  setView(-93.4, 37.4, 4)

