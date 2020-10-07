if (!require(librarian)){
  remotes::install_github("DesiQuintans/librarian")
  library(librarian)
}
shelf(
  # database
  DBI, RPostgres,
  # spatial
  leaflet, sf, sp,
  # tidyverse
  dplyr, purrr, readr, tibble, tidyr,
  # todo: use these
  # googledrive,
  # report
  DT, knitr, rmarkdown, shiny,
  # utility
  fs, glue, here, stringr)
here <- here::here

dir_mc       <- '/share/data/marinecadastre.gov'
csv_mc       <- file.path(dir_mc, '_datasets.csv')
csv_mc_paths <- file.path(dir_mc, '_datasets_paths.csv')

pass <- readLines("/share/.password_mhk-env.us")
con  <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname   = "gis",
  host     = "postgis",
  port     = 5432,
  user     = "admin",
  password = pass)

# tbls <- dbListTables(con); tbls

# helper functions
drop_d <- function(d_tbl){
  dbSendQuery(con, glue("SELECT DropGeometryTable ('public','{d_tbl}');"))
}

dbSafeNames = function(names) {
  # make names db safe: no '.' or other illegal characters,
  # all lower case and unique
  names = gsub('[^a-z0-9]+','_', tolower(names))
  names = make.names(names, unique=TRUE, allow_=TRUE)
  names = gsub('.','_',names, fixed=TRUE)
  names
}

dbRenameTable <- function(con, old, new){
  if (old %in% dbListTables(con))
    dbExecute(con, glue("ALTER TABLE {old} RENAME TO {new}"))
}

# rename old table
dbRenameTable(con, "cetmap_bia", "d_cetmap_bia")


titleshp2tbl <- function(title, shp){
  # title = "2012 - 2017 Outer Continental Shelf Oil and Gas Leasing Program"
  # shp   = "GOM_CGM_2012_2017_PFP_Area.shp"
  tbl <- glue("{title} | {str_sub(shp)}")
  if (nchar(tbl) > 63){ # max 63 characters for Postgres table name  
    nt <- nchar(title)
    ns <- nchar(shp)
    tbl <- glue("{str_sub(title, end=(nt - ns - 4))}~ | {shp}")
  }
  tbl
}

shp2tbl <- function(shp_path){
  # title = "2012 - 2017 Outer Continental Shelf Oil and Gas Leasing Program"
  # shp   = "GOM_CGM_2012_2017_PFP_Area.shp"
  #shp_path <- shps$shp_path[1]
  # which.max(map_int(shps$shp_path, function(x) nchar(basename(x) %>% fs::path_ext_remove())))
  #shp_path <- shps$shp_path[325]

  tbl_sfx <- basename(shp_path) %>% fs::path_ext_remove()
  tbl     <- glue("shp_{tbl_sfx}") %>% str_sub(end=63)

  tbl
}

shp2db <- function(shp, tbl, redo = F){
  # redo = F
  # shp  = "/share/data/marinecadastre.gov/Pacific Northwest Physiographic Habitat/V4_0_SGH_WA_OR_NCA_dir/cromsos/ShapefileData/V4_0_SGH_WA_OR_NCA/V4_0_SGH_WA_OR_NCA.shp"
  # tbl  = "Pacific Northwest Physiographic Habitat - V4_0_SGH_WA_OR_NCA.shp"
  # tbl = "shp_geology"; shp = dataset_shps4db %>% filter(tbl == !!tbl) %>% pull(shp)

  message(glue("LOAD: {tbl}"))
  
  if (!tbl %in% dbListTables(con) | redo){
    d_sf <- read_sf(shp)
    
    names(d_sf) <- dbSafeNames(names(d_sf))
    
    # d_tbl  <- "d_efh" # prefix: mc_ for MarineCadastre, ds_ for dataset?
    # d_redo <- FALSE
    
    # project to geographic coordinate reference system if need be
    if (is.na(st_crs(d_sf))){
      message("  st_set_crs(4326)")
      st_crs(d_sf) = 4326
    }
    if (st_crs(d_sf) != st_crs(4326)){
      message("  st_transform(crs = 4326)")
      d_sf <- st_transform(d_sf, crs = 4326)
    }
    
    message("  st_write()")
    st_write(d_sf, con, tbl)
    
  } else {
    message("  already loaded")
  }
  
  if (tbl %in% dbListTables(con)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}