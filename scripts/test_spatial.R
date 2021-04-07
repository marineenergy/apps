source(here::here("functions.R"))

aoi_ca <- 'POLYGON ((-127.5725 32.49444, -127.5725 43.96411, -116.6537 43.96411, -116.6537 32.49444, -127.5725 32.49444))'

datasets <- tbl(con, "datasets") %>% 
  filter(ready) %>% 
  collect()

test_ds <- function(ds, aoi){
  message(glue("dataset: {ds}"))
  tabulate_dataset_shp_within_aoi(ds, aoi, debug = F)
}

walk(datasets$code, test_ds, aoi = aoi_ca)