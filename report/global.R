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

#library(geosphere)
#data(merc)

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

