librarian::shelf(DT, glue, leaflet, plotly, purrr, readr, sf, tidyr)
options(readr.show_col_types = FALSE)

add_lines <- function(fig, d_sites){
  
  # rectangle bdr ----
  y_mar <- 0.025
  x_ext <- 0.3
  brdr <- list(
    type      = "rect", 
    xref      = "paper", 
    yref      = "paper",
    fillcolor = "transparent", 
    line      = list(
      color = "black",  
      width = 0.8), 
    opacity   = 1,
    x0        = 0,
    x1        = 1 + x_ext,
    y0        = 0 + y_mar,
    y1        = 1 - y_mar)
  
  # tech lines ----
  #tech <- c("Riverine", "Tidal", "Wave")
  #tech <- c("Riverine", "Tidal", "Wave")
  # tech         <- unique(d_sites$tag_technology)
  # n_tech_types <- length(tech)
  y_tech <- table(d_sites$tag_technology)
  y_lns  <- cumsum(y_tech[-length(y_tech)])
  
  hline <- function(y = 0, color = "black") {
    list(
      type = "line",
      x0 = 0,
      x1 = 1 + x_ext,
      xref = "paper",
      y0 = y - 0.5,
      y1 = y - 0.5,
      line = list(color = color))
  }
  lns <- lapply(y_lns, hline) %>% unname()
  
  fig %>% 
    layout(
      shapes = c(list(brdr), lns))
}

add_prj_mkrs <- function(fig, d_permits, symbls_type, cols_type) {
  fig %>% 
    plotly::add_markers(
      data      = d_permits,
      x         = ~license_date, 
      y         = ~project,
      symbol    = ~permit_type,
      symbols   = symbls_type,
      color     = ~permit_type,
      colors    = cols_type, 
      size      = 10,
      hoverinfo = "text",
      hovertext = paste(
        '<b>License Date:</b> '    , d_permits$license_date,
        '<br><b>Project Name:</b> ', d_permits$project,
        '<br><b>Permit Type:</b> ' , d_permits$permit_type,
        '<br><b>Technology:</b> '  , d_permits$tag_technology))
}

add_prj_sgmts <- function(fig, d_sites) {
  fig %>% 
    plotly::add_segments(
      data   = d_sites, # %>%
      # TODO: squeeze labels by wrapping lines or some such
      # mutate(project = recode(project, `Portsmouth Memorial Bridge`="Portsmouth\n Memorial\n Bridge")),
      x     = ~date_beg,
      xend  = ~date_end,
      y     = ~project,
      yend  = ~project,
      color = ~project_status,
      line  = list(width = 10)) 
}

add_tech_labels <- function(fig, d_sites){
  
  if (nrow(d_sites) == 0)
    return(fig)
  
  y_ends <- cumsum(c(Origin=0, table(d_sites$tag_technology)))
  y_tech <- tibble(
    name = names(y_ends),
    end  = y_ends) %>% 
    mutate(
      mid = (lag(end) + end) / 2) %>% 
    slice(-1)
  
  fig %>% 
    add_annotations(
      x         = 1.03,
      y         = y_tech$mid - 0.7,
      showarrow = FALSE,
      text      = glue("<b>{y_tech$name}</b>"),
      xref      = "paper",
      #yref      = "y",
      #align     = "center",
      font      = list(size = 8),
      textangle = "90")
}

d_to_tags_html <- function(d){
  y <- d %>% 
    left_join(
      tbl_tags %>% 
        select(tag_sql, cat, tag_nocat),
      by = "tag_sql") %>% 
    mutate(
      tag_html = paste0("<span class='me-tag me-", cat, "'>", tag_nocat, "</span>")) %>% 
    arrange(rowid, desc(cat), tag_nocat) %>% 
    select(-tag_sql, -cat, -tag_nocat)
  
  cols_grpby <- setdiff(colnames(y), c("tag", "tag_html", "tag_category"))
  
  #browser()
  
  y %>% 
    group_by(
      !!!syms(cols_grpby)) %>% 
    summarize(
      Tags = str_flatten(tag_html, collapse = " "),
      .groups = "drop") %>% 
    rename(ID = rowid) %>% 
    arrange(ID) %>% 
    collect() %>% 
    ungroup()
}

get_content_data <- function(ixns, type = "publications", ...){
  
  # ixns = list(c("Stressor.Noise.Airborne", "Receptor.MarineMammals"))
  # ixns = list(c("Stressor.Noise.Underwater", "Receptor.Fish.DemersalFish", "Consequence.Collision"))
  # type = "publications"; ixns = list(c("Stressor.Noise.Airborne", "Receptor.MarineMammals"), c("Receptor.Fish", "Management.Compliance"))
  # type = "publications"; ixns = list(c("Stressor.Noise.Airborne", "Receptor.MarineMammals"), c("Stressor.Noise.Airborne", "Receptor.MarineMammals"))
  # type = "publications"; ixns = list(c("Stressor.Noise.Airborne", "Receptor.MarineMammals"), c("Technology.Tidal", "Receptor.Fish", "Consequence.Collision"))
  # type = "publications"; ixns = list(c("Stressor.Noise.Airborne", "Receptor.MarineMammals"), c("Technology.Tidal", "Receptor.Fish", "Consequence.Collision"), c("Receptor.Fish", "Management.Compliance"))
  
  tbl_tags <- get_content_tags_tbl(type)
  get_rowids_per_ixn <- function(tags){
    if (length(tags) > 0){
      sql <- glue("SELECT rowid FROM {tbl_tags} WHERE tag_sql ~ '{tags}.*'") %>% 
        paste(collapse = "\nINTERSECT\n")
    } else {
      sql <- glue("SELECT DISTINCT rowid FROM {tbl_tags}")
    }
    DBI::dbGetQuery(con, sql) %>% 
      pull(rowid)
  }
  
  rowids <- map(
    ixns, get_rowids_per_ixn) %>% 
    unlist() %>% sort() %>% unique()
  
  # functions of lazy data, ie before collect(), per content type
  get_prj_tags <- function(){
    tbl(con, "project_sites") %>% 
      left_join(
        tbl(con, "project_tags"), by = "rowid")  }
  get_mgt_tags <- function(){
    tbl(con, "tethys_mgt") %>% 
      select(rowid, Interaction, `Specific Management Measures`, `Implications of Measure`) %>% 
      left_join(
        tbl(con, "tethys_mgt_tags"), by = "rowid")  }
  get_doc_tags <- function(){
    tbl(con, "ferc_docs") %>% 
      left_join(
        tbl(con, "ferc_doc_tags"),
        by = "rowid")  }
  get_pub_tags <- function(){
    tbl(con, "tethys_pubs") %>% 
      select(rowid, uri, title) %>% 
      left_join(
        tbl(con, "tethys_pub_tags") %>% 
          select(-uri),
        by = "rowid")  }
  get_spa_tags <- function(){
    tbl(con, "mc_spatial") %>% 
      filter(ready) %>%
      left_join(
        tbl(con, "mc_spatial_tags"),
        by = "rowid")  }
  
  
  # get lazy data per content type
  d <- list(
    projects     = get_prj_tags(),
    management   = get_mgt_tags(),
    documents    = get_doc_tags(),
    publications = get_pub_tags(),
    spatial      = get_spa_tags())[[type]]
  
  if (!is.null(rowids))
    d <- filter(d, rowid %in% !!rowids)
  
  d <- d_to_tags_html(d) %>%  
    mutate(
      across(where(is.character), na_if, "NA"))
  
  if (!is.null(attr(ixns, "message")))
    attr(d, "message") <- attr(ixns, "message")
  
  d 
}

get_content_ixns <- function(ixns, type = "publications"){
  # type = "publications"; ixns = list(c("Stressor.Noise.Airborne", "Receptor.MarineMammals"), c("Receptor.Fish", "Management.Compliance"))
  # type = "publications"; ixns = list(c("Stressor.Noise.Airborne", "Receptor.MarineMammals"), c("Stressor.Noise.Airborne", "Receptor.MarineMammals"))
  # type = "publications"; ixns = list(c("Stressor.Noise.Airborne", "Receptor.MarineMammals"), c("Technology.Tidal", "Receptor.Fish", "Consequence.Collision"))
  # type = "publications"; ixns = list(c("Stressor.Noise.Airborne", "Receptor.MarineMammals"), c("Technology.Tidal", "Receptor.Fish", "Consequence.Collision"), c("Receptor.Fish", "Management.Compliance"))
  
  # ixns_0 <- ixns
  attr(ixns, "message") <- NULL
  
  # subset interactions by categories available to content type
  content_cats <- get_content_tag_categories(type)
  tags_dropped <- NULL
  ixns <- map(ixns, function(ixn){
    # TODO: add message about which content tags in inxns getting filtered by content type
    ixn_cats <- str_extract(ixn, "^[A-z0-9]+")
    ixn <- ixn[ixn_cats %in% content_cats] })
  
  # strip missing children from tags
  # ixns_1 <- ixns # ixns <- ixns_1
  tags_parented <- NULL
  tbl_tags <- get_content_tags_tbl(type)
  nrow_tag <- function(tag) {
    dbGetQuery(con, glue("{paste('SELECT COUNT(*) AS count FROM', tbl_tags)} WHERE tag_sql = '{tag}'")) %>% 
      pull(count) }
  ixns <- map(ixns, function(ixn) {
    map_chr(ixn, function(tag) {
      tag_0 <- tag
      q <- str_split(tag, pattern = "\\.", simplify = F)[[1]]
      while (nrow_tag(tag) == 0 && length(q) > 2) 
        tag <- paste(q[1:(length(q) - 1)], collapse = ".")
      if (tag != tag_0)
        assign(
          "tags_parented",
          c(tags_parented, setNames(tag_0, tag)),
          envir = parent.env(parent.env(environment(NULL))))
      tag }) })
  
  if (length(tags_parented) > 0 ){
    d_tags_parented <- tibble(
      tag_0 = tags_parented,
      tag   = names(tags_parented)) %>% 
      mutate(
        tag_0_html = map_chr(tag_0, ixn_to_colorhtml, df_tags, is_html=T),
        tag_html   = map_chr(tag,   ixn_to_colorhtml, df_tags, is_html=T)) %>% 
      group_by(
        tag, tag_html) %>% 
      summarize(
        tags      = paste(tag_0     , collapse = ','),
        tags_html = paste(tag_0_html, collapse = ', '),
        tags_list = list(tag_0),
        .groups   = "drop")
    tags_parented_html <- with(d_tags_parented, glue(
      "{tags_html} -> {tag_html}")) %>% 
      paste(collapse = "; ")
    attr(ixns, "message") <- glue(
      # c(ixns, glue(
      "The following tag{ifelse(nrow(d_tags_parented) > 1,'s have',' has')} 
      been modified to the available parent tag{ifelse(nrow(d_tags_parented) > 1, 's', '')}: 
      {tags_parented_html}.")
  }
  
  ixns
}

get_content_tag_categories <- function(type, html=F){
  # content = "documents"
  tbl   <- get_content_tags_tbl(type)
  
  cats <- dbGetQuery(con,  glue("SELECT DISTINCT subltree(tag_sql, 0, 1) AS tag_cat FROM {tbl};")) %>% 
    pull("tag_cat") %>% as.character() %>% na.omit() %>% rev()
  
  if (html){
    h <- tibble(
      cat  = cats,
      html = map(cat, function(x) span(class=glue("me-tag me-{tolower(x)}"),  x)))
    cats <- tagList(h$html)
  }
  
  cats
}

get_content_tags_tbl <- function(type = "publications"){
  c(
    projects     = "project_tags",
    management   = "tethys_mgt_tags",
    documents    = "ferc_doc_tags",
    publications = "tethys_pub_tags",
    spatial      = "mc_spatial_tags")[type]
}

get_docs_tbl <- function(ixns = NULL, cks = NULL, type = "documents"){
  ixns  <- get_content_ixns(ixns, type)
  d     <- get_content_data(ixns, type)
  
  # filter by checkboxes
  if (!is.null(cks) && length(cks) > 0){
    for (col_bln in cks){
      d <- d %>% 
        filter(.data[[col_bln]] == TRUE)  }  }
  
  d <- d %>% 
    mutate(
      # TODO: include in scripts/update_tags.R:update_tags()
      across(starts_with("ck_"), as.character),
      across(starts_with("ck_"), recode, "TRUE"="✓", "FALSE"="☐"),
      Doc = ifelse(
        is.na(prj_doc_attachment),
        prj_document,
        paste0(prj_document, ": ", prj_doc_attachment)),
      Doc = ifelse(
        is.na(prj_doc_attach_url),
        Doc,
        glue("<a href='{prj_doc_attach_url}' target='_blank'>{Doc}</a>"))) %>% 
    select(
      ID, Project=project, Document=Doc, Detail=detail, Tags,
      Ixn = ck_ixn, 
      Obs = ck_obs, 
      MP  = ck_mp, 
      AMP = ck_amp, 
      PME = ck_pme, 
      BMP = ck_bmps)
  
  if (!is.null(attr(ixns, "message")))
    attr(d, "message") <- attr(ixns, "message")
  
  d
}

get_mgt_tbl <- function(ixns = NULL, type = "management"){
  ixns  <- get_content_ixns(ixns, type)
  d     <- get_content_data(ixns, type)
  d
}

get_projects_tbl <- function(ixns = NULL, type = "projects"){
  ixns <- get_content_ixns(ixns, type)
  d    <- get_content_data(ixns, type)
  d
}

get_pubs_tbl <- function(ixns = NULL, type = "publications"){
  ixns <- get_content_ixns(ixns, type)
  d    <- get_content_data(ixns, type) %>% 
    mutate(
      Title = as.character(glue(
        "<a href='{uri}' target='_blank'>{title}</a>")))
  d
}

get_spatial_intersection <- function(dataset_code, aoi_wkt){
  librarian::shelf(glue, sf)
  # summarize shapefile dataset from area of interest, with temporary in-memory query
  
  # TODO: pull latest datasets: https://docs.google.com/spreadsheets/d/1MMVqPr39R5gAyZdY2iJIkkIdYqgEBJYQeGqDk1z-RKQ/edit#gid=936111013
  # datasets_gsheet2db(redo = T)
  
  # dataset_code <- "coastal-channels"
  # dataset_code <- "oil-gas-wells"
  # dataset_code <- "cetacean-pacific-summer"
  # aoi_wkt <- ""
  # aoi_wkt <- "POLYGON ((-104.7656 22.97593, -104.7656 41.15991, -77.87109 41.15991, -77.87109 22.97593, -104.7656 22.97593))"
  # # testing get_spatial_intersection()
  # test_df<-get_spatial_intersection(dataset_code = dataset_code, aoi_wkt = aoi_wkt) 
  
  # testing mapping get_spatial_intersection()
  # spatial query / intersection based on aoi
  # vals$ixns <- list(
  #   c("Receptor.Birds.Passerines", 
  #     "Stressor.BehavioralInteraction"))
  
  # test vals
  # dataset_code = "cetacean-bia"
  # aoi_wkt = "POLYGON ((-121.1485 32.58968, -121.1485 34.61747, -116.8416 34.61747, -116.8416 32.58968, -121.1485 32.58968))"
  # aoi_wkt = params$aoi_wkt
  
  # test values: 
  # dataset_code <- "gloria"
  # aoi_wkt <- "POLYGON ((-104.7656 22.97593, -104.7656 41.15991, -77.87109 41.15991, -77.87109 22.97593, -104.7656 22.97593))"
  # NOTES:
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
  
  #browser()
  message(glue("get_spatial_intersection(dataset_code='{dataset_code}', aoi_wkt='{paste(aoi_wkt, collapse=';')}')"))
  
  # if (is.null(aoi_wkt)) {
  #   return("Please draw a location to get a summary of the intersecting features for this dataset.")
  # }
  
  ds <- tbl(con, "mc_spatial") %>% 
    collect() %>% 
    filter(code == !!dataset_code) %>%  # will map across dataset_code fld of d
    filter(ready) %>% 
    replace_na(list(buffer_nm = 0)) %>% 
    # mutate(buffer_nm = 0) %>% # TEST EFFECTS OF THIS
    mutate(across(st_intersection, function(x){
      x <- ifelse(is.na(x), F, x)
      x <- ifelse(x == "T", T, x)
      x}))
  # st_intersection = ifelse( # TODO: fix spatial data
  #   is.na(st_intersection), 
  #   as.logical('FALSE'), 
  #   st_intersection))
  
  #browser()
  if (is.null(aoi_wkt) || is.na(aoi_wkt) || aoi_wkt == ""){
    # if no Location
    #aoi_sql <- glue("ST_MakeEnvelope(-180,-90,180,90,4326)::geometry")
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
    
    # TODO: summarize_sql so not HUGE table like cetacean-pacific-summer
    
    if (!is.na(ds$summarize_r))
      eval(parse(text=ds$summarize_r))
    
    return(x_df)
  }
  
  if (ds$st_intersection){
    # Area weighted statistics ARE required
    ixn_sql <- str_replace({ds$select_sql}, 'geometry', 'geometry, st_intersection(ds.geometry, buf_aoi.geom) as ixn ')
    
    # dataset_code <- "coastal-channels"
    #ixn_sql <- str_replace({ds$select_sql}, 'geometry', 'geometry, ST_MakeValid(st_intersection(ds.geometry, buf_aoi.geom)) as ixn ')
    # ixn_sql %>% cat()
    # ixn_sql <- '
    # select
    #   objnam,
    #   themelayer,
    #   inform,
    #   drval1,
    #   quasou,
    #   quasou_txt,
    #   sordat,
    #   fairway,
    #   dsnm,
    #   dataaccess,
    #   datatype,
    #   st_force2d(geometry) as geometry, ST_MakeValid(st_intersection(ds.geometry, buf_aoi.geom)) as ixn
    # from shp_maintainedchannels'
    # r <- DBI::dbGetQuery(con, ixn_sql)
    # d <- st_read(con, 'shp_maintainedchannels')
    # d <- tbl(con, 'shp_maintainedchannels')
    #if (dataset_code == 'ocs-lease-blk') browser()
    
    if (!is.na(ds$summarize_sql)){
      # ds$summarize_sql  <- '
      #   select 
      #     fairway as "Fairway",
      #     round(sum(st_area(geometry::geography) * (st_area(ixn::geography) / st_area(geometry::geography))) / 4047) as "AOI Acres"
      #   from tmp_aoi
      #   group by fairway
      #   order by fairway'
      
      x_df <- dbGetQuery(
        con,
        glue("
          with
            buf_aoi as (
              select ST_BUFFER({aoi_sql}::geography, {ds$buffer_nm} * 1852) as geom),
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
              select ST_BUFFER({aoi_sql}::geography, {ds$buffer_nm} * 1852) as geom)
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
              select ST_BUFFER({aoi_sql}::geography, {ds$buffer_nm} * 1852) as geom ),
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
              select ST_BUFFER({aoi_sql}::geography, {ds$buffer_nm} * 1852) as geom)
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
  #           select ST_BUFFER({aoi_sql}::geography, {ds$buffer_nm} * 1852) as geom),
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
  #             select ST_BUFFER({aoi_sql}::geography, {ds$buffer_nm} * 1852) as geom)
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
  #             select ST_BUFFER({aoi_sql}::geography, {ds$buffer_nm} * 1852) as geom ),
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
  #             select ST_BUFFER({aoi_sql}::geography, {ds$buffer_nm} * 1852) as geom)
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

# TODO: add spatial msg
get_spatial_tbl <- function(ixns = NULL, aoi_wkt = NA, type = "spatial"){
  
  ixns  <- get_content_ixns(ixns, type)
  d     <- get_content_data(ixns, type) %>%
    mutate(
      Title = as.character(glue("{title} (Source: <a href='{src_url}'>{src_name}</a>)"))) %>%
    arrange(Title)

  if (is.null(aoi_wkt) || is.na(aoi_wkt) || aoi_wkt == "")
    return(d)
  
  d %>%
    mutate(
      sp_data = map(code, get_spatial_intersection, aoi_wkt))
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

lgnd_x_y <- function(fig, d_sites) {
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
        #domain    = c(0, length(unique(time_data$project[time_data$technology_type %in% tech]))),
        domain    = c(0, nrow(d_sites)),
        range     = c(0,1),
        showline  = FALSE,
        showgrid  = TRUE,
        type = "category",
        #text = c(unique(d_sites$project)),
        text = d_sites$project,
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

map_projects <- function(){

  prj_sites <- tbl(con, "project_sites") %>% 
    collect() %>% 
    mutate(
      label_html = label_html %>% lapply(htmltools::HTML),
      popup_html = popup_html %>% lapply(htmltools::HTML))
  
  leaflet::leaflet(
    data    = prj_sites, width = "100%",
    options = leaflet::leafletOptions(
      zoomControl = F)) %>% 
    leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% 
    leaflet::addMarkers(
      lat   = ~latitude,
      lng   = ~longitude,
      label = ~label_html,
      popup = ~popup_html) %>% 
    htmlwidgets::onRender("function(el, x) {
          L.control.zoom({ position: 'topright' }).addTo(this)
    }")
}

md2html <- function(x){
  if (purrr::is_empty(x) || is.na(x))
    return("")
  htmltools::HTML(markdown::markdownToHTML(
    text = x, fragment.only = T))
}

plot_project_timelines <- function(d_projects){
  # just use d_projects$project; rest from database
  
  prj_permit_types_csv <<- file.path(dir_data, "project_permit_types.csv")
  
  d_sites <- tbl(con, "project_sites") %>% 
    collect() %>% 
    filter(
      project %in% d_projects$project) %>% 
    arrange(tag_technology, project) %>% 
    mutate(
      tag_technology = factor(tag_technology))
  d_sites <- d_sites %>% 
    mutate(
      project = factor(project, levels = d_sites$project))
  
  # TODO: load prj_*  into db, read from db
  
  d_permits <- tbl(con, "project_permits") %>% 
    collect() %>% 
    left_join(
      d_sites %>% 
        select(project, tag_technology),
      by = "project") %>% 
    arrange(tag_technology, project)
  
  # ordered permit_types
  permit_types <- readr::read_csv(prj_permit_types_csv) %>%
    pull(permit_type) %>% 
    intersect(
      d_permits$permit_type)

  d_permits <- d_permits %>% 
    mutate(
      # order projects by technology_type, then project
      project     = factor(project, levels = distinct(d_permits, project) %>% pull(project)),
      # order permit types by permit_types
      permit_type = factor(permit_type, levels = permit_types))

  d_sites$label_html <- lapply(d_sites$label_html, htmltools::HTML)
  d_sites$popup_html <- lapply(d_sites$popup_html, htmltools::HTML)
  
  # colors & symbols
  project_statuses <- c("Active Project", "Inactive Project")
  cols_type   <- colorRampPalette(RColorBrewer::brewer.pal(n=11, name = 'PiYG'))(length(permit_types))
  cols_status <- c("#30A4E1", "#999999") # Active/Inactive Projects
  cols        <- setNames(
    c(cols_type, cols_status), 
    c(permit_types, project_statuses))
  symbls_type  <- c(rep('triangle-up', 3), 'triangle-down', 'triangle-up', 'triangle-down', 'triangle-up', 'triangle-down', rep('triangle-up', 3))
  stopifnot(length(permit_types) == length(symbls_type))
  symbls_status <- rep(NA, 2)
  symbls <- setNames(
    c(symbls_type,  symbls_status), 
    c(permit_types, project_statuses))
  
  # technology_type numbers for horizontal line and label placement along y axis
  n_tech <- d_sites %>% 
    group_by(tag_technology) %>% 
    summarize(n = n())
  n_riv <- sum(d_sites$tag_technology == "Riverine")
  n_tid <- sum(d_sites$tag_technology == "Tidal")
  n_wav <- sum(d_sites$tag_technology == "Wave")
  
  d_permits <- d_permits %>% 
    semi_join(
      d_sites,
      by = "project")
  
  n_projects <- nrow(d_projects)
  p_riv <- n_riv/n_projects
  p_tid <- n_tid/n_projects
  p_wav <- n_wav/n_projects
  
  p_tech_tbl <- 
    tibble(
      tech    = c("Riverine", "Tidal", "Wave"),
      p_tech  = c(p_riv,      p_tid,   p_wav),
      n_tech  = c(n_riv,      n_tid,   n_wav))
  
  fig <- plotly::plot_ly(colors = cols, symbols = symbls, height = 700) 
  fig <- fig %>% 
    lgnd_x_y(d_sites) %>% 
    add_prj_sgmts(d_sites) %>% 
    add_prj_mkrs(d_permits, symbls_type, cols_type) %>% 
    add_lines(d_sites) %>% 
    add_tech_labels(d_sites)
  fig
}

sf_to_wkt <- function(sf){
  
  if (!"sf" %in% class(sf))
    return(NULL)
  
  sf %>% 
    pull(geometry) %>% 
    sf::st_as_text()
}

tabulate_dataset_shp_within_aoi3 <- function(dataset_code, aoi_wkt, output = "kable"){
  # from analyze_spatial.Rmd -----
    # summarize shapefile dataset from area of interest
  
  # dataset_code = "cetacean-bia"; aoi_wkt = params$aoi_wkt; output = "kable"
  # dataset_code = "cetacean-pacific-summer"; aoi_wkt = params$aoi_wkt; output = "kable"
  
  # dataset_code='oil-gas-lease'; aoi_wkt='POLYGON ((-165.5718 5.996829, -165.5718 62.37497, -30.61896 62.37497, -30.61896 5.996829, -165.5718 5.996829))'; output = "kable"
  
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
    # Area weighted statistics ARE required; cat(ds$select_sql)
    
#     ds$select_sql <- 'select
#         sanctuary, 
#         area_name, 
#         mms_region, 
#         mms_plan_a as mms_plan_area, 
#         notes_1 as category,
#         notes as subcategory, 
#         notes_12 as e_cfr,
#         nm as nautical_miles,
#         acres,  
#         geometry
# from (
#         select 
#                 sanctuary, area_name, mms_region, mms_plan_a, 
#                 notes, notes_1, notes_12, acres, nm, ST_MakeValid(geometry) AS geometry
#         from "shp_WithdrawalUpdate10-2017"
#         UNION
#         select
#                 sanctuary, area_name, mms_region, mms_plan_a, 
#                 notes, notes_1, notes_12, acres, nm, ST_MakeValid(geometry) AS geometry
#         from "shp_AlaskaRegionWithdrawArea"
#         UNION
#         select
#                 sanctuary, area_name, mms_region, mms_plan_a, 
#                 notes, notes_1, notes_12, null as acres, null as nm, ST_MakeValid(geometry) AS geometry
#         from "shp_PacificRegionWithdrawAreas")' # ST_MakeValid(geometry)
    
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

# load gsheet_params ----
gsheet_params_csv <- here("data/gsheet_parameters.csv")
reload_params     <- F
if (!file.exists(gsheet_params_csv) | reload_params){
  gsheet_params <- get_gsheet_data("parameters") %>% 
    filter(output == "report") %>% select(-output)
  write_csv(gsheet_params, gsheet_params_csv)
}
gsheet_params <- read_csv(gsheet_params_csv)

# load tags ----
tbl_tags <- tbl(con, "tags")
df_tags  <- tbl(con, "tags") %>%
  mutate(
    tag_sql  = as.character(tag_sql),
    tag_html = paste0("<span class='me-tag me-", cat, "'>", tag_nocat, "</span>")) %>% 
  collect()

# load projects ----
#load_projects()
d_projects <- tbl(con, "project_sites") %>% 
  collect()
projects_tech_avail <- tbl(con, "project_sites") %>% 
  left_join(
    tbl(con, "project_tags"), by = "rowid") %>% 
  group_by(tag_sql) %>% 
  summarize() %>% 
  pull(tag_sql) %>% 
  as.character()

# load management ----
d_mgt_n <- tbl(con, "tethys_mgt") %>% summarize(n = n()) %>% pull(n)

# load documents ----
d_docs_n <- tbl(con, "ferc_docs") %>% summarize(n = n()) %>% pull(n)

# load publications ----
d_pubs_n <- tbl(con, "tethys_pubs") %>% summarize(n = n()) %>% pull(n)

# load spatial ----
d_spatial_n <- tbl(con, "mc_spatial") %>% summarize(n = n()) %>% pull(n)

