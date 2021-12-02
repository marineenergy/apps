shelf(DT, glue, tidyr)

map_projects <- function(prj_sites){
  prj_sites <- prj_sites %>% 
    mutate(
      label_html = label_html %>% lapply(htmltools::HTML),
      popup_html = popup_html %>% lapply(htmltools::HTML))
  
  leaflet::leaflet(
    data    = prj_sites, width = "100%",
    options = leaflet::leafletOptions(
      zoomControl = F)) %>% 
    leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% 
    leaflet::addMarkers(
      label = ~label_html,
      popup = ~popup_html) %>% 
    htmlwidgets::onRender("function(el, x) {
          L.control.zoom({ position: 'topright' }).addTo(this)
    }")
}

get_tags <- function(){
  tbl(con, "tags") %>% 
    collect() %>% 
    #filter(tag != category) %>% 
    mutate(
      tag_sql   = as.character(tag_sql),
      tag_named = purrr::map2(tag_sql, tag_nocat, setNames),
      tag_html  = glue("<span class='me-tag me-{cat}'>{tag}</span>")) %>% 
    arrange(desc(category), tag)
}

get_tags_nocat <- function(){
  tbl(con, "tags") %>% 
    collect() %>% # View()
    #filter(tag != category) %>% 
    mutate(
      tag_sql   = as.character(tag_sql),
      tag_named = purrr::map2(tag_sql, tag_nocat, setNames),
      tag_html  = glue("<span class='me-tag me-{cat}'>{tag_nocat}</span>")) %>% 
    arrange(desc(category), tag)
}

get_tags_html <- function(rid, tbl_tags = "ferc_doc_tags"){
  # rid = 1; tbl_tags = "ferc_doc_tags"
  tbl(con, tbl_tags) %>% 
    filter(rowid == !!rid, !is.na(tag_sql)) %>% 
    # select(rowid, tag_sql) %>% 
    distinct(rowid, tag_sql) %>% 
    collect() %>% 
    mutate(
      tag_sql = as.character(tag_sql)) %>%
    left_join(
      d_tags %>% 
        select(tag_sql, category, tag, tag_html),
      by = "tag_sql") %>% 
    filter(!is.na(tag)) %>% 
    arrange(desc(category), tag) %>% 
    pull(tag_html) %>% 
    paste(collapse = " ")
}


# spatial 
# will map across rows of d ('code')
get_spatial_intersection <- function(dataset_code, aoi_wkt){
  librarian::shelf(glue, sf)
  # summarize shapefile dataset from area of interest, with temporary in-memory query
 
  # TODO: pull latest datasets: https://docs.google.com/spreadsheets/d/1MMVqPr39R5gAyZdY2iJIkkIdYqgEBJYQeGqDk1z-RKQ/edit#gid=936111013
  # datasets_gsheet2db(redo = T)

  
  # test vals
  # dataset_code = "cetacean-bia"
  # aoi_wkt = "POLYGON ((-105.9082 22.73295, -105.9082 35.65492, -70.13672 35.65492, -70.13672 22.73295, -105.9082 22.73295))"
  # aoi_wkt = params$aoi_wkt
    
  # test values: 
  # dataset_code <- "gloria"
  # aoi_wkt <- "POLYGON ((-104.7656 22.97593, -104.7656 41.15991, -77.87109 41.15991, -77.87109 22.97593, -104.7656 22.97593))"
  # NOTES:
  {   
  # aoi_wkt = "POLYGON ((-67.06819 44.99416, -67.1857 44.94707, -67.21651 44.88058, -67.15834 44.78871, -67.04385 44.81789, -66.91015 44.86279, -67.06819 44.99416))"
  # crud()$finished <- "POLYGON ((-67.06819 44.99416, -67.1857 44.94707, -67.21651 44.88058, -67.15834 44.78871, -67.04385 44.81789, -66.91015 44.86279, -67.06819 44.99416))"

  # dataset_code = "cetacean-bia";
  # dataset_code = "cetacean-pacific-summer"; aoi_wkt = "POLYGON ((-67.06819 44.99416, -67.1857 44.94707, -67.21651 44.88058, -67.15834 44.78871, -67.04385 44.81789, -66.91015 44.86279, -67.06819 44.99416))"
  # params <- yaml::yaml.load("
  # title: Testing
  # aoi_wkt:
  # - POLYGON ((-115.9245 32.26236, -115.9245 32.26565, -115.9206 32.26565, -115.9206
  #               32.26236, -115.9245 32.26236))
  # - POLYGON ((-121.9503 33.01519, -121.9503 35.51658, -115.8711 35.51658, -115.8711
  #             33.01519, -121.9503 33.01519))")
  # aoi_wkt <- params$aoi_wkt
  # dataset_code = "oil-gas-wells"
  # aoi_wkt      = "POLYGON ((-157.4273 55.22198, -157.4273 61.76097, -143.1428 61.76097, -143.1428 55.22198, -157.4273 55.22198))"
  # dataset_code='fed-sand-gravel-lease'
  # aoi_wkt='POLYGON ((-175.4932 15.34568, -175.4932 27.93566, -151.813 27.93566, -151.813 15.34568, -175.4932 15.34568))'
  
  # dataset_code='monuments'
  # aoi_wkt='POLYGON ((-180.0668 16.98081, -180.0668 29.87807, -153.4797 29.87807, -153.4797 16.98081, -180.0668 16.98081))'
  }
  
  message(glue("get_spatial_intersection(dataset_code='{dataset_code}', aoi_wkt='{paste(aoi_wkt, collapse=';')}')"))
  
  if (is.null(aoi_wkt)) {
    return("Please draw a location to get a summary of the intersecting features for this dataset.")
  }
  
  ds <- tbl(con, "mc_spatial") %>% 
    collect() %>% 
    filter(code == !!dataset_code) %>%  # will map across dataset_code fld of d
    filter(ready) %>% 
    replace_na(list(buffer_nm = 0)) %>% 
    # mutate(buffer_nm = 0) %>% # TEST EFFECTS OF THIS
    mutate(across(st_intersection, function(x) ifelse(is.na(x), F, x)))
      # st_intersection = ifelse( # TODO: fix spatial data
      #   is.na(st_intersection), 
      #   as.logical('FALSE'), 
      #   st_intersection))
  
  #browser()
  if (is.null(aoi_wkt) | is.na(aoi_wkt)){
    # if no Location
    #aoi_sql <- glue("'ST_MakeEnvelope(-180,-90,180,90,4326)'::geometry")
    aoi_sql <- "world"
  } else if (length(aoi_wkt) == 1) {
    # if 1 Location
    aoi_sql <- glue("'SRID=4326;{aoi_wkt}'::geometry")
  } else if (length(aoi_wkt > 1)) {
    # if >1 Location
    aoi_wkts <- glue("'SRID=4326;{aoi_wkt}'::geometry")
    aoi_sql  <- glue("ST_COLLECT(\n{paste(aoi_wkts, collapse=',\n')})") 
  }
  #browser()
  
  if (aoi_sql == "world"){
    #browser()
    
    #if(dataset_code == "oil-gas-wells") browser()
    # ds$select_sql %>% str_replace_all(",\\W*geometry", "") %>% cat()
    # ∆ summarize_sql: +"as X" b/c ERROR:  subquery in FROM must have an alias
    # coastal-channels
    # column "geom" does not exist
    # LINE 13:         st_force2d(geom) as geometry
    
    #x_ds <- st_read(con, query = glue(ds$select_sql))
    # get non-spatial entirety of table
    #x_df <- dbGetQuery(con, ds$select_sql %>% str_replace_all(",\\W*geometry", "")) %>% tibble()
    x_df <- dbGetQuery(con, ds$select_sql_nogeom) %>% tibble()
    
    if (!is.na(ds$summarize_r))
      eval(parse(text=ds$summarize_r))
    
    return(x_df)
  }
  
  if (ds$st_intersection){
    # Area weighted statistics ARE required
    ixn_sql <- str_replace({ds$select_sql}, 'geometry', 'geometry, st_intersection(ds.geometry, buf_aoi.geom) as ixn ')
    
    
    #if (dataset_code == 'ocs-lease-blk') browser()
    
    if (!is.na(ds$summarize_sql)){
      x_df <- dbGetQuery(
        con,
        glue("
          with
            buf_aoi as (
              select ST_BUFFER({aoi_sql}, {ds$buffer_nm}) as geom),
            tmp_aoi as (
              {ixn_sql} as ds, buf_aoi
              where st_intersects(ds.geometry, buf_aoi.geom))
            {ds$summarize_sql}
          "))
      
    } else {
      x_sf <- st_read(
        con, 
        glue("
          with
            buf_aoi as (
              select ST_BUFFER({aoi_sql}, {ds$buffer_nm}) as geom)
            {ixn_sql} as ds, buf_aoi
            where st_intersects(ds.geometry, buf_aoi.geom)
          "))
      x_df <- st_drop_geometry(x_sf)
      
      if (!is.na(ds$summarize_r))
        eval(parse(text=ds$summarize_r))
    }
    
  } else {
    # Area weighted statistics NOT required
    if (!is.na(ds$summarize_sql)){
      x_df <- dbGetQuery(
        con, glue("
          with 
            buf_aoi as (
              select ST_BUFFER({aoi_sql}, {ds$buffer_nm}) as geom ),
            tmp_aoi as (
              {ds$select_sql} as ds
              inner join buf_aoi on st_intersects(ds.geometry, buf_aoi.geom) )
           {ds$summarize_sql}
           "))
    } else {
      x_sf <- st_read(
        con, query = glue("
          with 
            buf_aoi as (
              select ST_BUFFER({aoi_sql}, {ds$buffer_nm} * 1852) as geom)
            {ds$select_sql} as ds
            inner join buf_aoi on st_intersects(ds.geometry, buf_aoi.geom )
            "))
      x_df <- st_drop_geometry(x_sf)
      
      if (!is.na(ds$summarize_r))
        eval(parse(text=ds$summarize_r))
    }
  }
  
  
  
  
  # Different set of queries required for data sets that do or
  #   do not need area weighted statistics 
  
  # if area weighted statistics ARE required
  # if (ds$st_intersection == T) {    
  #   ixn_sql <- str_replace(
  #     {ds$select_sql}, 'geometry', 
  #     'geometry, st_intersection(ds.geometry, buf_aoi.geom) as ixn ')
  #   
  #   # if a summarize_sql query exists
  #   if (!is.na(ds$summarize_sql)){
  #     x_df <- dbGetQuery(con, glue("
  #       with
  #         buf_aoi as (
  #           select ST_BUFFER({aoi_sql}, {ds$buffer_nm}) as geom),
  #         tmp_aoi as (
  #           {ixn_sql} as ds, buf_aoi
  #           where st_intersects(ds.geometry, buf_aoi.geom))
  #         {ds$summarize_sql}
  #       "))
  #   # if no summarize sql query
  #   } else {
  #     x_sf <- st_read(
  #       con, 
  #       glue("
  #         with
  #           buf_aoi as (
  #             select ST_BUFFER({aoi_sql}, {ds$buffer_nm}) as geom)
  #           {ixn_sql} as ds, buf_aoi
  #           where st_intersects(ds.geometry, buf_aoi.geom)
  #         "))
  #     x_df <- st_drop_geometry(x_sf)
  #     
  #     if (!is.na(ds$summarize_r))
  #       eval(parse(text=ds$summarize_r))
  #   }
  #   
  # } else { # Area weighted statistics NOT required: if(ds$st_intersection != T)
  #   if (!is.na(ds$summarize_sql)){
  #     x_df <- dbGetQuery(
  #       con, glue("
  #         with 
  #           buf_aoi as (
  #             select ST_BUFFER({aoi_sql}, {ds$buffer_nm}) as geom ),
  #           tmp_aoi as (
  #             {ds$select_sql} as ds
  #             inner join buf_aoi on st_intersects(ds.geometry, buf_aoi.geom) )
  #          {ds$summarize_sql}
  #          "))
  #   } else {
  #     x_sf <- st_read(
  #       con, glue("
  #         with 
  #           buf_aoi as (
  #             select ST_BUFFER({aoi_sql}, {ds$buffer_nm} * 1852) as geom)
  #           {ds$select_sql} as ds
  #           inner join buf_aoi on st_intersects(ds.geometry, buf_aoi.geom )
  #           "))
  #     x_df <- st_drop_geometry(x_sf)
  #     
  #     if (!is.na(ds$summarize_r))
  #       eval(parse(text=ds$summarize_r))
  #   }
  # }
  
  x_df %>% collect() %>% tibble()
}
  
# dataset_code <- "oil-gas-wells"
# dataset_code <- "cetacean-pacific-summer"
# aoi_wkt <- "POLYGON ((-104.7656 22.97593, -104.7656 41.15991, -77.87109 41.15991, -77.87109 22.97593, -104.7656 22.97593))"
# # testing get_spatial_intersection()
# test_df<-get_spatial_intersection(dataset_code = dataset_code, aoi_wkt = aoi_wkt) 

# testing mapping get_spatial_intersection()
# spatial query / intersection based on aoi
# vals$ixns <- list(
#   c("Receptor.Birds.Passerines", 
#     "Stressor.BehavioralInteraction"))






 # from analyze_spatial.Rmd -----
tabulate_dataset_shp_within_aoi3 <- function(dataset_code, aoi_wkt, output = "kable"){
  # summarize shapefile dataset from area of interest
  
  # dataset_code = "cetacean-bia"; aoi_wkt = params$aoi_wkt; output = "kable"
  # dataset_code = "cetacean-pacific-summer"; aoi_wkt = params$aoi_wkt; output = "kable"
  
  message(glue("tab..._shp_within_aoi(dataset_code='{dataset_code}', aoi_wkt='{paste(aoi_wkt, collapse=';')}')"))
  
  if (is.null(aoi_wkt))
    return("Please draw a Location to get a summary of the intersecting features for this dataset.")
  
  ds <- tbl(con, "datasets") %>% 
    filter(code == !!dataset_code) %>% 
    replace_na(list(buffer_nm = 0)) %>% 
    collect()
  
  if (length(aoi_wkt) > 1){
    aoi_wkts <- glue("'SRID=4326;{aoi_wkt}'::geometry")
    aoi_sql  <- glue("ST_COLLECT(\n{paste(aoi_wkts, collapse=',\n')})") # Is this recreating the ST_COLLECT statement
    # for every item in <aoi_wkt> array?
  } else {
    # aoi_sql <- glue("'SRID=4326;{aoi_wkt}'")
    aoi_sql <- glue("'SRID=4326;{aoi_wkt}'::geometry")
  }
  
  # Different set of queries required for data sets that do or
  #   do not need area weighted statistics 
  if (ds$st_intersection){
    # Area weighted statistics ARE required
    ixn_sql <- str_replace({ds$select_sql}, 'geometry', 'geometry, st_intersection(ds.geometry, buf_aoi.geom) as ixn ')
    
    if (!is.na(ds$summarize_sql)){
      x_df <- dbGetQuery(
        con,
        glue("
          with
            buf_aoi as (
              select ST_BUFFER({aoi_sql}, {ds$buffer_nm}) as geom),
            tmp_aoi as (
              {ixn_sql} as ds, buf_aoi
              where st_intersects(ds.geometry, buf_aoi.geom))
            {ds$summarize_sql}
          "))
    } else {
      x_sf <- st_read(
        con, 
        glue("
          with
            buf_aoi as (
              select ST_BUFFER({aoi_sql}, {ds$buffer_nm}) as geom)
            {ixn_sql} as ds, buf_aoi
            where st_intersects(ds.geometry, buf_aoi.geom)
          "))
      x_df <- st_drop_geometry(x_sf)
      
      if (!is.na(ds$summarize_r))
        eval(parse(text=ds$summarize_r))
    }
    
  } else {
    # Area weighted statistics NOT required
    if (!is.na(ds$summarize_sql)){
      x_df <- dbGetQuery(
        con, glue("
          with 
            buf_aoi as (
              select ST_BUFFER({aoi_sql}, {ds$buffer_nm}) as geom ),
            tmp_aoi as (
              {ds$select_sql} as ds
              inner join buf_aoi on st_intersects(ds.geometry, buf_aoi.geom) )
           {ds$summarize_sql}
           "))
    } else {
      x_sf <- st_read(
        con, query = glue("
          with 
            buf_aoi as (
              select ST_BUFFER({aoi_sql}, {ds$buffer_nm} * 1852) as geom)
            {ds$select_sql} as ds
            inner join buf_aoi on st_intersects(ds.geometry, buf_aoi.geom )
            "))
      x_df <- st_drop_geometry(x_sf)
      
      if (!is.na(ds$summarize_r))
        eval(parse(text=ds$summarize_r))
    }
  }
  if (output == "tibble"){
    return(x_df)
  }
  
  x_spatial <- ifelse(
    ds$buffer_nm == 0,
    glue("\n\nSpatial: within site", .trim = F),
    glue("\n\nSpatial: within {ds$buffer_nm} nautical miles of site", .trim = F))
  
  if (knitr::is_html_output()){
    x_caption <- HTML(markdownToHTML(
      text = glue("Source: [{ds$src_name}]({ds$src_url}){x_spatial}"),
      fragment.only = T))
    
    tbl <- x_df %>% 
      kbl(caption = x_caption) %>%
      kable_styling(
        # full_width = F, position = "left", # position = "float_right"
        bootstrap_options = c("striped", "hover", "condensed", "responsive"))
    
  } else {
    x_caption <- glue("Source: [{ds$src_name}]({ds$src_url}){x_spatial}")
    
    tbl <- x_df %>% 
      kable(caption = x_caption, format = "pipe")
  }
  
  tbl
}











get_rowids_with_ixn <- function(db_tbl, ixn){
  # db_tbl = "tethys_mgt_tags"; ixn = c("Receptor.Fish", "Stressor.PhysicalInteraction.Collision")
  # db_tbl = "mc_spatial_tags"; ixn = values$ixns %>% unlist()
  # ixn = list(c(""Receptor.Birds","Stressor.HabitatChange"))
  sql <- glue("SELECT rowid FROM {db_tbl} WHERE tag_sql ~ '{ixn}.*'") %>% 
    paste(collapse = "\nINTERSECT\n")
  DBI::dbGetQuery(con, sql) %>% 
    pull(rowid)
}

ixns_to_colorhtml_df <- function(ixns, df_tags){
  # from tag_sql character vector produce data.frame of Interaction with colored HTML tags
  # shiny: tbl_ixns
  # _report: 
  
  if (length(ixns) == 0)
    return(tibble(Interaction = character(0)))
  
  if (class(ixns) == "character"){
    # individual interactions, like headers in report
    d <- tibble(
      tag_sql = ixns) %>% 
      tidyr::unnest(tag_sql) %>% 
      left_join(
        df_tags, by = "tag_sql") %>% 
      # arrange by category inverse alphabetical:
      #   Technology, Stressor, Receptor, Phase, Management
      arrange(desc(category), tag) %>% 
      summarize(
        Interaction = paste(tag_html, collapse = " "), .groups = "drop")
    return(d)
  }
  # multiple interactions, like tbl_ixns in app
  tibble(
    rowid   = 1:length(ixns),
    tag_sql = ixns) %>% 
    tidyr::unnest(tag_sql) %>% 
    left_join(
      df_tags, by = "tag_sql") %>% 
    # arrange by category inverse alphabetical:
    #   Technology, Stressor, Receptor, Phase, Management
    arrange(rowid, desc(category), tag) %>% 
    group_by(rowid) %>% 
    summarize(
      Interaction = paste(tag_html, collapse = " "), .groups = "drop") %>% 
    select(-rowid)
}

ixn_to_colorhtml <- function(ixns, df_tags, is_html = NULL){
  if (is.null(is_html))
    is_html <- knitr::is_html_output()
  if (is_html){
    ixns_to_colorhtml_df(ixns, df_tags) %>% 
      pull(Interaction) %>% 
      paste(collapse = ', ')
  } else {
    ixns
  }
}

load_projects <- function(ixns=NULL){
  # p_csvs <- list.files("/share/github/apps/data", "project_.*")
  # file.copy(file.path("/share/github/apps/data", p_csvs), file.path("/share/github/apps_dev/data", p_csvs), overwrite = T)
  prj_sites_csv        <<- file.path(dir_data, "project_sites.csv")
  prj_times_csv        <<- file.path(dir_data, "project_times.csv")
  prj_permits_csv      <<- file.path(dir_data, "project_permits.csv")
  prj_permit_types_csv <<- file.path(dir_data, "project_permit_types.csv")
  
  # TODO: load prj_*  into db, read from db
  prj_sites <<- readr::read_csv(prj_sites_csv, col_types = readr::cols()) %>% 
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F)
  
  d_times <<- readr::read_csv(prj_times_csv, col_types = readr::cols())  %>% 
    arrange(technology_type, project) %>% 
    mutate(technology_type = factor(technology_type))
  
  d_times <<- d_times %>% 
    mutate(
      # order projects by technology_type, then project
      project = factor(project, levels = d_times$project))
  # levels(d_times$project) # Igiugig,...,Yakutat
  
  d_permits <<- readr::read_csv(prj_permits_csv, col_types = readr::cols()) %>% 
    left_join(
      d_times %>% 
        select(project, technology_type), 
      by = "project") %>% 
    arrange(technology_type, project)
  d_permits <<- d_permits %>% 
    mutate(
      # order projects by technology_type, then project
      project = factor(project, levels = distinct(d_permits, project) %>% pull(project)))
  # levels(d_permits$project) # Igiugig,...,Yakutat
  
  # order permit_types
  permit_types <<- readr::read_csv(prj_permit_types_csv, col_types = readr::cols()) %>% 
    pull(permit_types)
  permit_types <<- permit_types %>% 
    intersect(d_permits$permit_type)
  d_permits <<- d_permits %>% 
    mutate(
      # order permit types by permit_types
      permit_type = permit_type %>% factor(levels = permit_types))

  # # extract technology from interaction tags
  # tags2tech <- c(
  #   "Technology.Riverine" = "Riverine Energy",
  #   "Technology.Tidal"    = "Tidal Energy",
  #   "Technology.Wave"     = "Wave Energy")
  # if (!is.null(ixns)){
  #   # if ixns exist
  #   # tech contains only the tags2tech names that are also in values$ixns
  #   tech <- tags2tech[intersect(names(tags2tech), values$ixns %>% unlist())]
  #   # tech <- tags2tech[1:2]
  # } else {
  #   # else tech is all of tags2tech
  #   tech <- tags2tech
  # }

  # # filter by technology
  # prj_sites <<- prj_sites %>%
  #   filter(technology_type %in% tech)
  # d_times <<- d_times %>%
  #   filter(technology_type %in% tech)
  # d_permits <<- d_permits %>%
  #   filter(technology_type %in% tech)

  prj_sites$label_html <- prj_sites$label_html %>% lapply(htmltools::HTML)
  prj_sites$popup_html <- prj_sites$popup_html %>% lapply(htmltools::HTML)
  
  # colors & symbols
  #project_statuses <<- unique(d_times$project_status)
  project_statuses <<- c("Active Project", "Inactive Project")
  cols_type   <<- colorRampPalette(RColorBrewer::brewer.pal(n=11, name = 'PiYG'))(length(permit_types))
  cols_status <<- c("#30A4E1", "#999999") # Active/Inactive Projects
  cols        <<- setNames(
    c(cols_type, cols_status), 
    c(permit_types, project_statuses))
  symbls_type  <<- c(rep('triangle-up', 3), 'triangle-down', 'triangle-up', 'triangle-down', 'triangle-up', 'triangle-down', rep('triangle-up', 3))
  symbls_status <<- rep(NA, 2)
  symbls <<- setNames(
    c(symbls_type,  symbls_status), 
    c(permit_types, project_statuses))
  
  # technology_type numbers for horizontal line and label placement along y axis
  n_tech <<- d_times %>% 
    group_by(technology_type) %>% 
    summarize(n = n())
  n_riv <<- n_tech %>% filter(technology_type == "Riverine Energy") %>% pull(n)
  n_tid <<- n_tech %>% filter(technology_type == "Tidal Energy")    %>% pull(n)
  n_wav <<- n_tech %>% filter(technology_type == "Wave Energy")     %>% pull(n)
}

# filter projects by selected technology
filter_prj_by_tech <- function(tech, prj_sites, d_times, d_permits) {
  
  prj_sites <<- prj_sites %>% filter(technology_type %in% tech) %>% 
    mutate(
      project = factor(
        project,
        levels = prj_sites %>% sf::st_drop_geometry() %>% distinct(project) %>% pull(project)))
  
  d_times   <<- d_times   %>% filter(technology_type %in% tech) %>% 
    mutate(
      project = factor(
        project, 
        levels = d_times %>% distinct(project) %>% pull(project)))
  
  d_permits <<- d_permits %>% filter(technology_type %in% tech) %>% 
    mutate(
      project = factor(
        project, 
        levels = d_permits %>% distinct(project) %>% pull(project)))
        # levels = d_permits %>% 
        #   filter(technology_type %in% tech) %>% 
        #   distinct(project) %>% 
        #   pull(project)))
}

# calculate y placement of antns and tech lines 
calculate_y_tech <- function(tech) {
  if ("Riverine Energy" %in% tech) {
    n_riv_sel <- n_riv
  } else if (!("Riverine Energy" %in% tech)) {
    n_riv_sel <- as.integer(0)
  }
  if ("Wave Energy" %in% tech) {
    n_wav_sel <- n_wav
  } else if (!("Wave Energy" %in% tech)) {
    n_wav_sel <- as.integer(0)
  }
  if ("Tidal Energy" %in% tech) {
    n_tid_sel <- n_tid
  } else if (!("Tidal Energy" %in% tech)) {
    n_tid_sel <- as.integer(0)
  }
  n_projects <<- n_riv_sel + n_wav_sel + n_tid_sel
  p_riv_sel  <<- n_riv_sel/n_projects
  p_tid_sel  <<- n_tid_sel/n_projects
  p_wav_sel  <<- n_wav_sel/n_projects
  
  p_tech_tbl <<- 
    tibble(
      tech        = c("Riverine Energy", "Tidal Energy", "Wave Energy"),
      p_tech_sel  = c(p_riv_sel,          p_tid_sel,     p_wav_sel),
      n_tech      = c(n_riv,              n_tid,         n_wav)) %>% 
    mutate(
      p_tech_all  = n_tech/(sum(n_tech)),
      name        = stringr::str_replace(tech, " Energy", ""))
}


# plot_projects() helper functions ----
add_prj_sgmts <- function(fig, time_data) {
  fig %>% 
    plotly::add_segments(
      data   = time_data %>% filter(technology_type %in% tech), # %>%
       # TODO: squeez labels by wrapping lines or some such
       # mutate(project = recode(project, `Portsmouth Memorial Bridge`="Portsmouth\n Memorial\n Bridge")),
       x     = ~date_beg,
       xend  = ~date_end,
       y     = ~time_data$project[time_data$technology_type %in% tech],
       yend  = ~time_data$project[time_data$technology_type %in% tech],
       color = ~project_status,
       line  = list(width = 10)) 
}

add_prj_mkrs <- function(fig, permit_data) {
  fig %>% 
    plotly::add_markers(
      data      = permit_data %>% filter(technology_type %in% tech),
      x         = ~license_date, 
      y         = ~permit_data$project[permit_data$technology_type %in% tech],
      symbol    = ~permit_type,
      symbols   = symbls_type,
      color     = ~permit_type,
      colors    = cols_type, 
      size      = 10,
      hoverinfo = "text",
      hovertext = paste(
        '<b>License Date:</b> '    , permit_data$license_date, 
        '<br><b>Project Name:</b> ', permit_data$project, 
        '<br><b>Permit Type:</b> ' , permit_data$permit_type,
        '<br><b>Technology:</b> '  , permit_data$technology_type)
    )
}

lgnd_x_y <- function(fig, time_data) {
  fig %>% 
    plotly::layout(
      xaxis = list(
        #title = 'Date',
        title     = '',
        showline  = FALSE,
        showgrid  = FALSE),
      yaxis = list(
        title     = '',
        autorange = "reversed",
        domain    = c(0, length(unique(time_data$project[time_data$technology_type %in% tech]))),
        range     = c(0,1),
        showline  = FALSE,
        showgrid  = TRUE,
        type = "category",
        text = c(unique(time_data$project)),
        tickfont = list(size = 8)),
      margin = list(
        r = 15,
        t = 25,
        b = 40,
        l = 25,
        pad = list(r = 20, t = 0, b = 0, l = 50)),
      legend = list(
        orientation = 'h',
        font = list(size = 10)))
}


get_antn_info <- function(tech, p_tech_tbl){
  calculate_y_tech(tech)
  ybase      <<- list()
  ybase[[1]] <<- 1.005      
  ybase[[2]] <<- ybase[[1]] - p_tech_tbl$p_tech_sel[1]
  ybase[[3]] <<- ybase[[2]] - p_tech_tbl$p_tech_sel[2]
  
  # antns <<- list()
  
  y_tech_antn <<- list()
  for (i in 1:nrow(p_tech_tbl)) {
    y_tech_antn[[i]] <<- ybase[[i]] - 0.5*(p_tech_tbl$p_tech_sel[i])
  }
    
    
    # antns[[i]] <<- list(
    #   x         = 1,
    #   y         = ybase[[i]] - 0.5*(p_tech_tbl$p_tech_sel[i]),
    #   showarrow = FALSE,
    #   text      = glue("<b>{p_tech_tbl$name[i]}</b>"),
    #   xref      = "paper",
    #   yref      = "paper",
    #   align     = "center",
    #   font      = list(size = 8),
    #   textangle = "90",
    #   yshift    = 8)
  
}

add_tech_text <- function(fig, p_tech_sel, y_tech, tech_name){
  get_antn_info(tech = tech, p_tech_tbl = p_tech_tbl)
  if (p_tech_sel == 0) {
    fig 
  } else {
    fig %>% 
      plotly::layout(
        annotations = list(
          x         = 1,
          y         = y_tech,
          showarrow = FALSE,
          text      = glue("<b>{tech_name}</b>"),
          xref      = "paper",
          yref      = "paper",
          align     = "center",
          font      = list(size = 8),
          textangle = "90",
          yshift    = 8))
  }
}

#   # tech lines -----
#   n_tech_types  <- p_tech_tbl %>%
#     filter(p_tech_sel != 0) %>%
#     nrow()
# 
#   tech_ln_1 <- add_line(y_tech = top - 1.1*(p_tech_tbl$p_tech_sel[1]))
#   tech_ln_2 <- add_line(y_tech = top - 1.1*(p_tech_tbl$p_tech_sel[2]))
#   # tech_ln_3 <- add_line(y_tech = 1 - p_tech_tbl$p_tech_sel[3])
# 
#   # function ----
#   if (n_tech_types %in% c(0, 1)) {
#     fig %>% plotly::layout(
#       shapes = list(rectangle))
#   } else if (n_tech_types > 1) {
#     fig %>%
#       plotly::layout(
#         shapes = list(
#           rectangle,
#           tech_ln_1,
#           tech_ln_2))
#   }
# }


add_lines <- function(fig, tech){
  
  # rectangle bdr ----
  top       <- 0.975
  bottom    <- 0.025
  left      <- 0
  right     <- 1.5
  
  rectangle <- list(
    type      = "rect", xref = "paper", yref  = "paper",
    fillcolor = "transparent", 
    line      = list(color = "black",   width = 0.8), 
    opacity   = 1,
    x0        = left,
    x1        = right,
    y0        = bottom,
    y1        = top)
  
  # tech lines ----
  tech         <- tech
  n_tech_all   <- c("Riverine Energy" = n_riv, 
                    "Tidal Energy"    = n_tid, 
                    "Wave Energy"     = n_wav)
  n_tech       <<- n_tech_all[tech]
  n_tech_types <<- length(tech)
  
  if (n_tech > 1){
    y_lns <- ((n_tech[-length(n_tech)])) %>% 
      cumsum() %>% 
      as.list()
  }
  
  if (n_tech_types > 1){
    # convert y_lns to list format
    lines <- list()
    for (i in 1:length(y_lns)) {
      # lines[[names(y_lns[i])]] <- add_line(y_tech = y_lns[[i]])
      lines[[i]] <- list(
        type      = "line", xref = "paper", yref  = "y",
        line      = list(color = "black",   width = 0.8), 
        opacity   = 1,
        x0        = 0,
        x1        = 1.5,
        y0        = y_lns[[i]] - (0.5),
        y1        = y_lns[[i]] - (0.5))
    }
    
    for (i in 1:length(lines)) {
      lines_list <- list(rectangle, lines[[1]], lines[[i]])
    }
  }
  # browser()

  # add_lines() function ----
  if (n_tech_types %in% c(0, 1)) {
    fig %>% plotly::layout(
      shapes = list(rectangle))
  } else if (n_tech_types > 1) {
    fig %>% 
        plotly::layout(
          shapes = lines_list)
  }
}

# for initial plotly projects plot
plot_projects <- function(){
  load_projects()
  
  tech <<- c("Riverine Energy", "Tidal Energy", "Wave Energy")
  filter_prj_by_tech(tech, prj_sites, d_times, d_permits)
  calculate_y_tech(tech)
  fig <- plotly::plot_ly(colors = cols, symbols = symbls, height = 700) 
  fig <- fig %>% 
    lgnd_x_y(time_data       = d_times)   %>% 
    add_prj_sgmts(time_data  = d_times)   %>% 
    add_prj_mkrs(permit_data = d_permits) %>% 
    add_lines(tech) %>% 
    add_tech_text(
      p_tech_sel = p_tech_tbl$p_tech_sel[1],
      tech_name  = p_tech_tbl$name[1],
      y_tech     = y_tech_antn[[1]]) %>% 
    add_tech_text(
      p_tech_sel = p_tech_tbl$p_tech_sel[2],
      tech_name  = p_tech_tbl$name[2],
      y_tech     = y_tech_antn[[2]]) %>% 
    add_tech_text(
      p_tech_sel = p_tech_tbl$p_tech_sel[3],
      tech_name  = p_tech_tbl$name[3],
      y_tech     = y_tech_antn[[3]])  
  fig
}

# for plot filtered by tech selection
update_project_plot <- function(){
  load_projects()
  tech <<- tech
  filter_prj_by_tech(tech, prj_sites, d_times, d_permits)
  calculate_y_tech(tech)
  fig <- plotly::plot_ly(colors = cols, symbols = symbls, height = 700) 
  fig <- fig %>% 
    lgnd_x_y(time_data       = d_times)   %>% 
    add_prj_sgmts(time_data  = d_times)   %>% 
    add_prj_mkrs(permit_data = d_permits) %>% 
    add_lines(tech) %>% 
    add_tech_text(
      p_tech_sel = p_tech_tbl$p_tech_sel[1],
      tech_name  = p_tech_tbl$name[1],
      y_tech     = y_tech_antn[[1]]) %>% 
    add_tech_text(
      p_tech_sel = p_tech_tbl$p_tech_sel[2],
      tech_name  = p_tech_tbl$name[2],
      y_tech     = y_tech_antn[[2]]) %>% 
    add_tech_text(
      p_tech_sel = p_tech_tbl$p_tech_sel[3],
      tech_name  = p_tech_tbl$name[3],
      y_tech     = y_tech_antn[[3]])  
  
  # update plot
  tech <<- tech
  filter_prj_by_tech(tech, prj_sites, d_times, d_permits)
  calculate_y_tech(tech)
  fig <- plotly::plot_ly(colors = cols, symbols = symbls, height = 700) 
  fig <- fig %>% 
    lgnd_x_y(time_data       = d_times)   %>% 
    add_prj_sgmts(time_data  = d_times)   %>% 
    add_prj_mkrs(permit_data = d_permits) %>% 
    add_lines(tech) %>% 
    add_tech_text(
      p_tech_sel = p_tech_tbl$p_tech_sel[1],
      tech_name  = p_tech_tbl$name[1],
      y_tech     = y_tech_antn[[1]]) %>% 
    add_tech_text(
      p_tech_sel = p_tech_tbl$p_tech_sel[2],
      tech_name  = p_tech_tbl$name[2],
      y_tech     = y_tech_antn[[2]]) %>% 
    add_tech_text(
      p_tech_sel = p_tech_tbl$p_tech_sel[3],
      tech_name  = p_tech_tbl$name[3],
      y_tech     = y_tech_antn[[3]])  
  fig
}


tags_sql_to_html <- function(ixns, df_tags){
  
}

# documents ----
d_docs <- tbl(con, "ferc_docs") %>% 
  left_join(
    tbl(con, "ferc_doc_tags"),
    by = "rowid") %>% 
  arrange(desc(rowid))
d_docs_n <- tbl(con, "ferc_docs") %>% summarize(n = n()) %>% pull(n)

get_docs_tbl <- function(d, ixns, cks){
  
  if (length(ixns) > 0){
    rowids <- sapply(ixns, get_rowids_with_ixn, db_tbl = "ferc_doc_tags") %>% 
      unlist() %>% unique()
    d <- d %>%
      filter(rowid %in% !!rowids)
  }
  if (length(cks) > 0){
    for (col_bln in cks){
      d <- d %>% # collect() %>% nrow() # 1426
        filter(.data[[col_bln]] == TRUE) # %>% collect() %>% nrow()
      # message(glue("ck_docs == `{col_bln}` nrow: {d %>% collect() %>% nrow()}"))
    }
  }
  d <- d_to_tags_html(d)
  
  d %>% 
    mutate(
      # TODO: include in scripts/update_tags.R:update_tags()
      across(where(is.character), na_if, "NA"),
      across(starts_with("ck_"), as.character),
      across(starts_with("ck_"), recode, "TRUE"="✓", "FALSE"="☐"),
      Doc = ifelse(
        is.na(prj_doc_attachment),
        prj_document,
        paste0(prj_document, ": ", prj_doc_attachment)),
      Doc = ifelse(
        is.na(prj_doc_attach_url),
        Doc,
        glue("<a href='{prj_doc_attach_url}'>{Doc}</a>"))) %>% 
    #names()
    select(
      ID, Project=project, Document=Doc, Detail=detail, Tags,
      Ixn = ck_ixn, 
      Obs = ck_obs, 
      MP  = ck_mp, 
      AMP = ck_amp, 
      PME = ck_pme, 
      BMP = ck_bmps)
}
