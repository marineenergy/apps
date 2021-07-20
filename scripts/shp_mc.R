# functions for handling MarineCadastre shapefiles

dir_mc       <- '/share/data/marinecadastre.gov'
csv_mc       <- file.path(dir_mc, '_datasets.csv')
csv_mc_paths <- file.path(dir_mc, '_datasets_paths.csv')

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

gdb2db <- function(gdb, lyr, tbl, redo = F){
  # redo = F
  # gdb <- "/share/data/marinecadastre.gov/Federal and State Waters/FederalAndStateWaters_dir/FederalAndStateWaters.gdb"
  # lyr <- "FederalAndStateWaters"
  # tbl <- "gdb_FederalAndStateWaters"
  # Error in st_geometry.sf(x) : 
  #   attr(obj, "sf_column") does not point to a geometry column.
  # Did you rename it, without setting st_geometry(obj) <- "newname"? 
  
  message(glue("LOAD: {tbl}"))
  
  if (!tbl %in% dbListTables(con) | redo){
    d_sf <- read_sf(gdb, lyr)

    idx_geom <- which(names(d_sf) == attr(d_sf, "sf_column"))
    names(d_sf) <- dbSafeNames(names(d_sf))
    d_sf <- st_set_geometry(d_sf, names(d_sf)[idx_geom]) 
    
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


datasets_gsheet2db <- function(tbl = "datasets", redo = T){
  # tbl = "datasets" OR "datasets_mc"
  # tbl = "datasets"; redo = T
  
  # datasets_marinecadastre.gov.csv - Google Sheet
  #   edit online: https://docs.google.com/spreadsheets/d/1MMVqPr39R5gAyZdY2iJIkkIdYqgEBJYQeGqDk1z-RKQ/edit#gid=0
  gid <- "1MMVqPr39R5gAyZdY2iJIkkIdYqgEBJYQeGqDk1z-RKQ"
  csv <- glue("https://docs.google.com/spreadsheets/d/{gid}/gviz/tq?tqx=out:csv&sheet={tbl}")
  
  d <- read_csv(csv, col_types = cols()) %>% 
    select(-starts_with("X")) %>% 
    filter(!is.na(code)) %>% 
    mutate(across(is.logical, replace_na, F))
  #d
  
  if (!tbl %in% dbListTables(con) | redo)
    dbWriteTable(con, tbl, d, overwrite=T)
}
# datasets_gsheet2db(redo=T)
