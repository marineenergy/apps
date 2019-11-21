library(shiny)
library(leaflet)
library(mapedit)
library(sf)

# getwd()
# p <- read_sf('mapedit/new_geom.geojson')
# st_centroid(p)
# st_bbox(p)
map <- leaflet() %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  setView(-93.4, 37.4, 4)
