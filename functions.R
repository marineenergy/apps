if (!require(librarian)){
  remotes::install_github("DesiQuintans/librarian")
  library(librarian)
}
shelf(
  DBI, dplyr, DT, fs, glue, here, htmltools, htmlwidgets, kableExtra, knitr, 
  leaflet, markdown, r-spatial/mapview, # https://github.com/r-spatial/mapview/issues/324
  purrr, readr,  RPostgres, rmarkdown, rvest, tibble, tidyr,
  scales, sf, shiny, sp, stringr, urltools)
# ggmap, gt, webshot, png

here <- here::here

# if (!webshot::is_phantomjs_installed())
#   webshot::install_phantomjs()

# webshot::install_phantomjs()
# webshot.js returned failure value: 1
# https://www.vultr.com/docs/how-to-install-phantomjs-on-ubuntu-16-04
# /usr/local/bin/phantomjs
#   Auto configuration failed
#   139821528473216:error:25066067:DSO support routines:DLFCN_LOAD:could not load the shared library:dso_dlfcn.c:185:filename(libssl_conf.so): libssl_conf.so: cannot open shared object file: No such file or directory
# sudo vi /etc/environment; export OPENSSL_CONF=/etc/ssl/; echo $OPENSSL_CONF
Sys.setenv(OPENSSL_CONF="/etc/ssl/") # for webshot

dir_mc       <- '/share/data/marinecadastre.gov'
csv_mc       <- file.path(dir_mc, '_datasets.csv')
csv_mc_paths <- file.path(dir_mc, '_datasets_paths.csv')

not_linux <- Sys.info()[["sysname"]] != "Linux"

if (not_linux){
  # presumably Caleb's machine
  db_params <- list(
    dbname  = "dev",
    host    = "localhost",
    user    = "cgrant",
    pwd_txt = "../../pwd.txt")
} else {
  # presumably server
  db_params <- list(
    dbname  = "gis",
    host    = "postgis",
    user    = "admin",
    pwd_txt = "/share/.password_mhk-env.us")
}

# Ben's laptop
if (Sys.info()[["user"]] == "bbest" & Sys.info()[["sysname"]] == "Darwin"){
  db_params <- list(
    dbname  = "gis",
    host    = "marineenergy.app",
    user    = "admin",
    pwd_txt = "~/private/dbpass_marineenergy.app.txt") 
}

con <<- dbConnect(
  RPostgres::Postgres(),
  dbname   = db_params$dbname,
  host     = db_params$host,
  port     = 5432,
  user     = db_params$user,
  password = readLines(db_params$pwd_txt))

# tbls <- dbListTables(con) %>% sort(); tbls

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

tabulate_dataset_shp_within_aoi_old <- function(dataset_code, aoi_wkt, output = "kable"){
  # summarize shapefile dataset from area of interest, with temporary in-memory query (Common Table Expressions; vs on disk temp tables)
  
  # TODO: pull latest datasets: https://docs.google.com/spreadsheets/d/1MMVqPr39R5gAyZdY2iJIkkIdYqgEBJYQeGqDk1z-RKQ/edit#gid=936111013
  # datasets_gsheet2db(redo = T)
  
  # dataset_code = "cetacean-bia"; aoi_wkt = params$aoi_wkt
  # dataset_code = "efh"; aoi_wkt = "POLYGON ((-67.06819 44.99416, -67.1857 44.94707, -67.21651 44.88058, -67.15834 44.78871, -67.04385 44.81789, -66.91015 44.86279, -67.06819 44.99416))"
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
  
  # Use CTE instead of temporary tables
  # TODO
  #    Add conditional to check if ds$summarize_r
  #    Drop geometry column in x_df?
  
  # DEBUG, TODO: turn off this manual override for https://github.com/mhk-env/mhk-env_api/issues/4
  if (dataset_code == "cetacean-pacific-summer")
    ds$st_intersection = F
  sql_intersection <- ifelse(ds$st_intersection,'ST_INTERSECTION','ST_INTERSECTS')
  
  if (!is.na(ds$summarize_sql)){
    x_df <- dbGetQuery(
      con,
      glue("
        with 
          tmp_selarea as (
            select ST_BUFFER({aoi_sql}, {ds$buffer_nm}) as geom ),
          tmp_aoi as (
            {ds$select_sql} as ds
            inner join tmp_selarea on {sql_intersection}(ds.geometry, tmp_selarea.geom) )
         {ds$summarize_sql}
         "))
  } else {
    x_sf <- st_read(
      con, query = glue("
        with 
          tmp_selarea as (
            select ST_BUFFER({aoi_sql}, {ds$buffer_nm} * 1852) as geom)
          {ds$select_sql} as ds
          inner join tmp_selarea on {sql_intersection}(ds.geometry, tmp_selarea.geom )
          "))
    x_df <- st_drop_geometry(x_sf)
    
    if (!is.na(ds$summarize_r))
      eval(parse(text=ds$summarize_r))
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

get_aoi_sql <- function(aoi_wkt){
  
  if (is.null(aoi_wkt))
    return(null)
  
  if (length(aoi_wkt) > 1){
    aoi_wkts <- glue("'SRID=4326;{aoi_wkt}'::geometry")
    aoi_sql  <- glue("ST_COLLECT(\n{paste(aoi_wkts, collapse=',\n')})") # Is this recreating the ST_COLLECT statement
    # for every item in <aoi_wkt> array?
  } else {
    # aoi_sql <- glue("'SRID=4326;{aoi_wkt}'")
    aoi_sql <- glue("'SRID=4326;{aoi_wkt}'::geometry")
  }
  aoi_sql
}

tabulate_dataset_shp_within_aoi <- function(dataset_code, aoi_wkt, output = "kable", debug = T){
  # summarize shapefile dataset from area of interest
  
  # dataset_code = "cetacean-bia"; aoi_wkt = params$aoi_wkt; output = "kable"
  # dataset_code = "cetacean-pacific-summer"; aoi_wkt = params$aoi_wkt; output = "kable"
  
  # dataset_code = "pipelines"; aoi_wkt='POLYGON ((-124.316 35.95711, -119.627 31.65049, -116.5146 35.0183, -120.8413 37.45895, -124.316 35.95711))'
  
  if (debug)
    message(glue("tab..._shp_within_aoi(dataset_code='{dataset_code}', aoi_wkt='{paste(aoi_wkt, collapse=';')}')"))
  
  if (is.null(aoi_wkt))
    return("Please draw a Location to get a summary of the intersecting features for this dataset.")
  
  ds <- tbl(con, "datasets") %>% 
    filter(code == !!dataset_code) %>% 
    replace_na(list(buffer_nm = 0)) %>% 
    collect()
  
  aoi_sql <- get_aoi_sql(aoi_wkt)
  
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
              select ST_BUFFER(({aoi_sql})::geography, {ds$buffer_nm} * 1852) as geom),
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
              select ST_BUFFER(({aoi_sql})::geography, {ds$buffer_nm} * 1852) as geom)
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
              select ST_BUFFER(({aoi_sql})::geography, {ds$buffer_nm} * 1852) as geom ),
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
              select ST_BUFFER(({aoi_sql})::geography, {ds$buffer_nm} * 1852) as geom)
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
tabulate_tethys_literature_from_tags <- function(tags){
  
  # tags <- params$stressors[1]
  # tags <- "EMF"
  #
  # tags = c("Marine Mammals AND Noise", "Fish AND EMF")
  # tags <- str_split(tags, " AND ") %>% unlist()
  # tags <- str_split("Birds AND Collision AND Marine Energy (General)", " AND ") %>% unlist()
  
  res <- dbGetQuery(
    con, 
    glue("
    SELECT uri, title, COUNT(tag_text) AS tag_cnt from (
        SELECT 
          uri, 
          data ->'title' ->> 0 AS title,
          json_array_elements(data->'tags') ->> 0 as tag_text
        FROM tethys_pubs) q 
     WHERE tag_text IN ('{paste(tags, collapse = \"','\")}')
     GROUP BY uri, title")) %>% 
    filter(tag_cnt == length(tags))
  
  caption_md <- glue("Literature from [Tethys Knowledge Base](https://tethys.pnnl.gov/knowledge-base-all).")
  
  if (knitr::is_html_output()){
    caption_html <- HTML(markdownToHTML(
      text = caption_md,
      fragment.only = T))
    
    res %>%
    mutate(
      Title = map2_chr(
        title, uri,
        function(x, y)
          glue("<a href={y} target='_blank'>{x}</a>"))) %>%
      select(Title) %>%
      arrange(Title) %>%
      datatable(
        caption = caption_html,
        escape = F)
  } else {
    
    glue("{caption_md}:\n\n", .trim=F) %>% cat()
    
    res %>%
      mutate(
        li_title = glue("1. [{title}]({uri})")) %>%
      pull(li_title) %>% 
      paste(collapse = "\n") %>% 
      cat()
    
    # res %>%
    #   tibble() %>% 
    #   mutate(
    #     Title = glue("[{title}]({uri})")) %>%
    #   select(Title) %>% 
    #   gt() %>%
    #   fmt_markdown(columns = vars(Title)) %>% 
    #   cols_align(align = "left", columns = vars(Title)) %>% 
    #   tab_options(
    #     row.striping.include_table_body = T,
    #     table.width = pct(100)) %>% 
    #   tab_header(
    #     title    = md(caption_md), 
    #     subtitle = md("&nbsp;")) # need blank subtitle for now: https://github.com/rstudio/gt/issues/197)
    # tab_source_note(md(caption_md))
    
    # tbl %>% 
    #   mutate(test = paste0("\\href{http://", link, "}{", test, "}")) %>%
    #   kable("latex", escape = F, booktabs = T) %>%
    #   kable_styling(bootstrap_options = c("hover", "condensed")) 
    # 
    # 
    # res %>%
    #   mutate(
    #     Title = cell_spec(title, "html", link = uri)) %>% 
    #   select(Title) %>% 
    #   kbl(caption = caption_md, escape = F) %>%
    #   kable_styling(
    #     # full_width = F, position = "left", # position = "float_right"
    #     bootstrap_options = c("striped", "hover", "condensed", "responsive"))
  }
}

tabulate_tethys_literature_from_tags_gen <- function(tags){
  # drop Marine Energy since includes all
  tags <- setdiff(tags, "Marine Energy")
  
  s_tags <- glue("'{paste(tags, collapse = \"', '\")}'")
  
  q_tags <- paste(
    glue("
        SELECT uri
        FROM tethys_pub_tags
        WHERE tag IN ({s_tags})"), collapse = "\n UNION \n")
  # cat(q_tags)
  q_pubs <- glue(
    "
      SELECT DISTINCT q.uri, p.title FROM (\n{q_tags}\n) q 
      INNER JOIN (
        SELECT 
          uri,
          data -> 'title' ->> 0  AS title 
        FROM tethys_pubs) p ON q.uri = p.uri
      ORDER BY p.title")
  
  res <- dbGetQuery(con, q_pubs)  %>% 
    tibble() %>% 
    mutate(
      title = str_trim(title))
  
  caption_md <- glue("Literature from [Tethys Knowledge Base](https://tethys.pnnl.gov/knowledge-base-all).")
  
  if (knitr::is_html_output()){
    caption_html <- HTML(markdownToHTML(
      text = caption_md,
      fragment.only = T))
    
    res %>%
      mutate(
        Title = map2_chr(
          title, uri,
          function(x, y)
            glue("<a href={y} target='_blank'>{x}</a>"))) %>%
      select(Title) %>%
      arrange(Title) %>%
      datatable(
        caption = caption_html,
        escape = F)
  } else {
    
    glue("{caption_md}:\n\n", .trim=F) %>% cat()
    
    res %>%
      mutate(
        li_title = glue("1. [{title}]({uri})")) %>%
      pull(li_title) %>% 
      paste(collapse = "\n") %>% 
      cat()
  }
}

tabulate_tethys_literature_from_tags_gen_oldjson <- function(tags){
  
  # tags <- params$stressors[1]
  # tags <- "EMF"
  #
  # tags = c("Marine Mammals AND Noise", "Fish AND EMF")
  # tags <- str_split(tags, " AND ") %>% unlist()
  # tags <- str_split("Birds AND Collision AND Marine Energy", " AND ") %>% unlist()
  # tags <- str_split("Birds AND Collision AND Current", " AND ") %>% unlist()
  # tags <- str_split("Environment AND Human Dimensions AND Current", " AND ") %>% unlist()

  # Tech Ideal:
  # tech <- read_csv(here("data/tags.csv")) %>% 
  #   filter(
  #     facet == "technology",
  #     item_label %in% tags) %>% 
  #   pull(item_label)
  
  # Tech Actual:
  # https://tethys.pnnl.gov/knowledge-base-marine-energy
  # Technology ---
  # * Marine Energy (2433)
  #   - Tidal (878)
  #   - Wave (674)
  #   - Riverine (87)
  #   - OTEC (57)
  #   - Ocean Current (32)
  #   - Salinity Gradient (9)
  # * Wind Energy (493)
  #   - Offshore Wind (418)
  #   - Land-Based Wind (9)
  #
  # dbGetQuery(
  #   con,
  #   glue("
  #       SELECT json_array_elements(data->'technologyType') ->> 0 as tech_text
  #       FROM tethys_pubs")) %>%
  #   group_by(tech_text) %>%
  #   summarize(n = n())
  # 
  # 4 Wave                591
  # 1 Current             873 # Tidal
  # 2 OTEC                 57
  # 3 Salinity Gradient     8
  
  message(glue("tab..._tags_gen(): {paste(tags, collapse='|')}"))
  
  # tags = c("Birds", "Marine Energy")
  
  tech_avail <- c("Marine Energy", "OTEC", "Salinity Gradient", "Current", "Wave")
  
  tech_tags <- intersect(tags, tech_avail)
  s_r_tags  <- setdiff(tags, tech_tags)
  
  # drop Marine Energy since includes all, but use above to exclude from s_r_tags
  tech_tags <- setdiff(tech_tags, "Marine Energy")
  
  s_r_res <- dbGetQuery(
    con, 
    glue("
        SELECT uri, title, COUNT(tag_text) AS tag_cnt FROM (
            SELECT 
              uri, 
              data ->'title'                    ->> 0 AS title,
              json_array_elements(data->'tags') ->> 0 as tag_text
            FROM tethys_pubs) q 
         WHERE 
          tag_text IN ('{paste(s_r_tags, collapse = \"','\")}')
         GROUP BY uri, title")) %>% 
    filter(tag_cnt == length(s_r_tags)) %>% 
    select(uri, title) %>% 
    tibble()
  
  if (length(tech_tags) > 0){
    tech_res <- dbGetQuery(
      con, 
      glue("
        SELECT uri, title, COUNT(tag_text) AS tag_cnt FROM (
            SELECT 
              uri, 
              data ->'title'                              ->> 0 AS title,
              json_array_elements(data->'technologyType') ->> 0 as tag_text
            FROM tethys_pubs) q 
         WHERE 
          tag_text IN ('{paste(tech_tags, collapse = \"','\")}')
         GROUP BY uri, title")) %>% 
      filter(tag_cnt == length(tech_tags)) %>% 
      select(uri, title) %>% 
      tibble()
    
    res <- inner_join(
      s_r_res,
      tech_res,
      by = c("uri", "title"))
    
  } else {
    res <- s_r_res
  }

  caption_md <- glue("Literature from [Tethys Knowledge Base](https://tethys.pnnl.gov/knowledge-base-all).")
  
  if (knitr::is_html_output()){
    caption_html <- HTML(markdownToHTML(
      text = caption_md,
      fragment.only = T))
    
    res %>%
    mutate(
      Title = map2_chr(
        title, uri,
        function(x, y)
          glue("<a href={y} target='_blank'>{x}</a>"))) %>%
      select(Title) %>%
      arrange(Title) %>%
      datatable(
        caption = caption_html,
        escape = F)
  } else {
    
    glue("{caption_md}:\n\n", .trim=F) %>% cat()
    
    res %>%
      mutate(
        li_title = glue("1. [{title}]({uri})")) %>%
      pull(li_title) %>% 
      paste(collapse = "\n") %>% 
      cat()
    
    # res %>%
    #   tibble() %>% 
    #   mutate(
    #     Title = glue("[{title}]({uri})")) %>%
    #   select(Title) %>% 
    #   gt() %>%
    #   fmt_markdown(columns = vars(Title)) %>% 
    #   cols_align(align = "left", columns = vars(Title)) %>% 
    #   tab_options(
    #     row.striping.include_table_body = T,
    #     table.width = pct(100)) %>% 
    #   tab_header(
    #     title    = md(caption_md), 
    #     subtitle = md("&nbsp;")) # need blank subtitle for now: https://github.com/rstudio/gt/issues/197)
    # tab_source_note(md(caption_md))
    
    # tbl %>% 
    #   mutate(test = paste0("\\href{http://", link, "}{", test, "}")) %>%
    #   kable("latex", escape = F, booktabs = T) %>%
    #   kable_styling(bootstrap_options = c("hover", "condensed")) 
    # 
    # 
    # res %>%
    #   mutate(
    #     Title = cell_spec(title, "html", link = uri)) %>% 
    #   select(Title) %>% 
    #   kbl(caption = caption_md, escape = F) %>%
    #   kable_styling(
    #     # full_width = F, position = "left", # position = "float_right"
    #     bootstrap_options = c("striped", "hover", "condensed", "responsive"))
  }
}

knit_tethys_literature_from_tags_gen <- function(tags){
  
  lapply(tags, function(tag) {
    knit_expand('_docs-tethys_gen.Rmd') }) %>% 
    knit_child(text = unlist(.), quiet = T) %>% 
    cat(sep = '\n\n')
}


knit_tethys_literature_from_tags <- function(tags){
  
  
  lapply(tags, function(tag) {
    knit_expand('_docs-tethys.Rmd') }) %>% 
    knit_child(text = unlist(.), quiet = T) %>% 
    cat(sep = '\n\n')
}


html_init <- function(){
  html_tags <<- tagList()
}

html_add <- function(content){
  html_tags <<- tagList(html_tags, content)
}

html_out <- function(){
  html_tags
}

update_tags <- function(){
  shelf(googlesheets4)
  
  # google sheet key from Google Console service account
  #   https://console.cloud.google.com/iam-admin/serviceaccounts/details/111453668228815650069/keys?authuser=2&organizationId=651265387478&project=marineenergy4gargle
  gs4_auth_json <- "/share/data/marineenergy4gargle.json" 
  # tags tab in [data | marineenergy.app - Google Sheet](https://docs.google.com/spreadsheets/d/1MTlWQgBeV4eNbM2JXNXU3Y-_Y6QcOOfjWFyKWfdMIQM/edit#gid=662531985)
  #   + shared sheet with: shares@marineenergy4gargle.iam.gserviceaccount.com
  sheet_id  <- "1MTlWQgBeV4eNbM2JXNXU3Y-_Y6QcOOfjWFyKWfdMIQM"
  sheet_tab <- "tags"
  
  # rename original tags
  # DBI::dbSendQuery(con, "ALTER TABLE tags RENAME TO tags_0;")
  # dbListTables(con) %>% sort()
  
  # googledrive
  stopifnot(file.exists(gs4_auth_json))
  gs4_auth(path = gs4_auth_json)
  
  # read tags from gsheet
  tags <- read_sheet(sheet_id, sheet_tab)
  
  # write tags to db
  dbWriteTable(con, "tags", tags, overwrite=T)
  
  dbSendStatement(con, "ALTER TABLE tags ALTER COLUMN tag_sql TYPE ltree USING text2ltree(tag_sql);")
  
}

update_tethys_docs <- function(){
  # update db tables: tethys_pubs, tethys_pub_tags; plus data/tethys_docs.[json|csv]
  
  shelf(jsonlite)
  
  tethys_docs_url  <- glue("https://tethys.pnnl.gov/api/primre_export")
  tethys_docs_json <- here("data/tethys_docs.json") # TODO: rm data/tethys.json
  tethys_docs_csv  <- here("data/tethys_docs.csv")  # TODO: rm data/tethys.csv
  
  download.file(tethys_docs_url, tethys_docs_json)
  
  tethys <- read_json(tethys_docs_json)
  #tethys_content <- tethys[["..JSON"]][[1]]
  tethys_content <- tethys
  # tethys_content[[1]]
  # names(tethys_content[[1]])
  #  [1] "URI"              "type"             "landingPage"      "sourceURL"        "title"           
  #  [6] "description"      "author"           "organization"     "originationDate"  "spatial"         
  # [11] "technologyType"   "tags"             "modifiedDate"     "signatureProject"
  # names(tethys_content[[1]]) %>% paste(collapse = " TEXT, ")
  # URI TEXT, type TEXT, landingPage TEXT, sourceURL TEXT, title TEXT, description TEXT, author TEXT, organization TEXT, originationDate TEXT, spatial TEXT, technologyType TEXT, tags TEXT, modifiedDate TEXT, signatureProject TEXT
  
  tethys_uris <- map_chr(tethys_content, "URI")
  tethys_data <- map_chr(tethys_content, toJSON) %>% 
    str_replace_all("'","''")
  
  tibble(
    uri = tethys_uris,
    data = tethys_data) %>% 
    write_csv(tethys_docs_csv)
  
  # TODO: rename table tethys_pubs -> tethys_docs and read fxns in Shiny report app
  # dbRemoveTable(con, "tethys_pubs")
  sql <- glue("
    CREATE TABLE IF NOT EXISTS tethys_pubs (
  	  uri TEXT NOT NULL PRIMARY KEY,
  	  data JSON NOT NULL);")
  dbExecute(con, sql)
  dbExecute(con, "DELETE FROM tethys_pubs;")
  
  # run once in Terminal to install software and test connection to database:
  #   sudo apt-get update; sudo apt-get install postgresql-client
  # pgpassword set at top:
  pass <- readLines("/share/.password_mhk-env.us")
  Sys.setenv(PGPASSWORD=pass) # for psql command line
  cmd <- glue('cat {tethys_docs_csv} | psql -h postgis -p 5432 -U admin -c "COPY tethys_pubs (uri, data) FROM STDIN WITH (FORMAT CSV, HEADER TRUE);" gis')
  system(cmd)
  
  #  [1] "URI"              "type"             "landingPage"      "sourceURL"        "title"           
  #  [6] "description"      "author"           "organization"     "originationDate"  "spatial"         
  # [11] "technologyType"   "tags"             "modifiedDate"     "signatureProject"
  
  # update tables for easier querying
  docs <- dbGetQuery(
    con, 
    "SELECT 
     uri, 
     data -> 'title'         ->> 0  AS title, 
     data -> 'tags'                 AS tags, 
     data -> 'technologyType'       AS technologyType
   FROM tethys_pubs") %>% 
    arrange(uri) %>% 
    tibble()
  # docs # 6,484 rows
  # docs %>% head(10) %>% View()
  
  # TODO: evaluate counts of tags, esp. "Environment"
  doc_tags <- dbGetQuery(
    con, 
    "SELECT 
     uri, 
     json_array_elements(data->'tags')           ->> 0 as tag
   FROM tethys_pubs
  UNION
   SELECT
     uri, 
     json_array_elements(data->'technologyType') ->> 0 as tag
   FROM tethys_pubs") %>% 
    arrange(uri, tag) %>% 
    tibble()
  
  # TODO: rename table tethys_pub_tags -> tethys_doc_tags and read fxns in Shiny report app
  dbWriteTable(con, "tethys_pub_tags", doc_tags, overwrite=T)
  # doc_tags # 14,505 rows
  # doc_tags # 16,034 rows after UNION
  
  # TODO: explore docs without tags
  # docs_tech <- dbGetQuery(
  #   con, 
  #   "SELECT
  #    uri, 
  #    json_array_elements(data->'technologyType') ->> 0 as tag
  #  FROM tethys_pubs") %>% 
  #   arrange(uri, tag_tech) %>% 
  #   tibble()
  # docs_tech
  #
  # docs_without_tags <- setdiff(docs %>% select(uri), doc_tags %>% select(uri))
  # docs_without_tags # 0 rows
  # docs_without_tech <- setdiff(docs %>% select(uri), docs_tech %>% select(uri)) %>% 
  #   left_join(
  #     pubs %>% 
  #       select(uri, title, tags)) %>% 
  #   arrange(desc(title))
  # docs_without_tech # 5,158 rows
  # View(docs_without_tech)
}

update_tethys_mgt <- function(){
  
  mgt_url <- "https://tethys.pnnl.gov/management-measures"
  mgt_csv <- here("data/tethys_mgt.csv")
  
  read_html(mgt_url) %>% 
    html_table() %>% 
    .[[1]] %>% 
    write_csv(mgt_csv)
}


update_tethys_tags <- function(){
  
  url      <- "https://tethys.pnnl.gov/knowledge-base-marine-energy"
  tags_csv <- here("data/tethys_tags.csv") # TODO: rm data/tags.csv (OLD)
  
  # helper functions ----
  
  get_facet_items <- function(facet_name){
    # facet_name = "technology"
    item_nodes <- facet_nodes[[which(facet_names == facet_name)]] %>% 
      html_nodes(".facet-item")
    
    map_df(item_nodes, get_facet_item)
  }
  
  get_facet_item <- function(item_node){
    # item_node <- item_nodes[[1]]
    
    # keys <- c("category" = 1, "id" = 2)
    # stopifnot(key %in% names(keys))
    
    label <- item_node  %>%
      html_node("a span.facet-item__value") %>% 
      html_text()
    
    keys <- item_node  %>%
      html_node("a") %>%
      html_attr("data-drupal-facet-item-id") %>% 
      str_split("-") %>% 
      .[[1]]
    
    tibble(facet = keys[1], item_id = keys[2], item_label = label)
  }
  
  # scrape html ----
  html <- read_html(url)
  
  facet_nodes <- html_nodes(html, ".js-facets-checkbox-links")
  facet_names <- html_attr(facet_nodes, "data-drupal-facet-alias")
  
  # explore
  #html_structure(facet_nodes[[1]])
  #html_text(facet_nodes[[1]]) %>% cat()
  #as_list(facet_nodes[[1]])
  
  d <- tibble(facet = facet_names) %>% 
    group_by(facet) %>%
    do(get_facet_items(.$facet)) %>% 
    ungroup()
  
  write_csv(d, tags_csv)
  
}


update_tethys_intxns <- function(verbose=F){
  
  tethys_pfx <- "https://tethys.pnnl.gov/knowledge-base-marine-energy"
  tags_csv   <- here("data/tethys_tags.csv")
  s_r_csv    <- here("data/tethys_intxns.csv") # TODO: rm data/tethys_stressor_receptor.csv
  
  get_num_refs <- function(url){
    # url = "https://tethys.pnnl.gov/knowledge-base-marine-energy?f[0]=receptor:280&f[1]=stressor:355"
    
    #if (url == "https://tethys.pnnl.gov/knowledge-base-marine-energy?f[0]=receptor:284&f[1]=stressor:531") browser()
    if (verbose)
      message(glue("url: {url}"))
    
    tbls <- read_html(url) %>% 
      html_table() 
    
    if (length(tbls) == 0){
      num_refs <- 0 %>% as.integer()
    } else{
      num_refs <- nrow(tbls[[1]])
    }
    
    num_refs
  }
  
  d_tags <- read_csv(tags_csv)
  
  receptor <- d_tags %>% 
    filter(facet == "receptor") %>% pull(item_label) %>% unique() %>% sort()
  stressor <- d_tags %>% 
    filter(facet == "stressor") %>% pull(item_label)
  
  d_s_r <- expand_grid(receptor, stressor) %>% 
    left_join(
      d_tags %>% 
        filter(facet == "receptor") %>% 
        select(receptor = item_label, receptor_id = item_id),
      by = "receptor") %>% 
    left_join(
      d_tags %>% 
        filter(facet == "stressor") %>% 
        select(stressor = item_label, stressor_id = item_id),
      by = "stressor") %>% 
    mutate(
      url      = glue("{tethys_pfx}?f[0]=receptor:{receptor_id}&f[1]=stressor:{stressor_id}"),
      num_refs = map_int(url, get_num_refs),
      link     = glue(
        "<a href='{url}'>{receptor} x {stressor} ({num_refs})</a>"))
  
  write_csv(d_s_r, s_r_csv)
}


update_projects <- function(){
  
  librarian::shelf(
    dplyr, glue, googlesheets4, here, htmltools, markdown, purrr, readr, sf, stringr)

  # google sheet key from Google Console service account
  #   https://console.cloud.google.com/iam-admin/serviceaccounts/details/111453668228815650069/keys?authuser=2&organizationId=651265387478&project=marineenergy4gargle
  gs4_auth_json <- "/share/data/marineenergy4gargle.json" 
  
  # [data | marineenergy.app - Google Sheet](https://docs.google.com/spreadsheets/d/1MTlWQgBeV4eNbM2JXNXU3Y-_Y6QcOOfjWFyKWfdMIQM/edit#gid=662531985)
  #   + shared sheet with: shares@marineenergy4gargle.iam.gserviceaccount.com
  gsheet     <- "https://docs.google.com/spreadsheets/d/1MTlWQgBeV4eNbM2JXNXU3Y-_Y6QcOOfjWFyKWfdMIQM/edit"
  
  # local CSVs
  prj_times_csv        <- here("data/project_times.csv")
  prj_permits_csv      <- here("data/project_permits.csv")
  prj_permit_types_csv <- here("data/project_permit_types.csv")
  prj_sites_csv        <- here("data/project_sites.csv") # with popup info on permits per site
  
  tibble(
    permit_types = c(
    "Notice of Intent/Preliminary Permit Application",
    "Draft Pilot License App",
    "Final Pilot License App",
    "Pilot License Issued",
    "Draft License App",
    "Draft Re-License App",
    "Final License App",
    "Final Re-License App",
    "Environmental Assessment",
    "Settlement Agreement",
    "Permit Issued",
    "Re-License Issued")) %>% 
    write_csv(prj_permit_types_csv)
  permit_types <- read_csv(prj_permit_types_csv, col_types = cols()) %>% 
    pull(permit_types)
  
  stopifnot(file.exists(gs4_auth_json))
  gs4_auth(path = gs4_auth_json)
  
  # read gsheets
  projects <- read_sheet(gsheet, "projects") %>% 
    mutate(
      date_beg = as.Date(date_beg, format = "%Y-%m-%d"),
      date_end = as.Date(date_end, format = "%Y-%m-%d")) %>% 
    filter(!is.na(longitude), !is.na(latitude)) %>% 
    arrange(technology_type, project)
  
  project_permits <- read_sheet(gsheet, "project_permits") %>%
    mutate(
      permit_type  = factor(
        permit_type, levels = permit_types, ordered = T),
      license_date = as.Date(license_date, format = "%Y-%m-%d")) %>% 
    filter(!is.na(permit_type)) %>%
    select(project, license_date, permit_type, link) %>% 
    arrange(project, license_date, permit_type)
    
  write_csv(projects       , prj_times_csv)
  write_csv(project_permits, prj_permits_csv)
    
    
  md2html <- function(x){
    markdownToHTML(text = x, fragment.only = T, options = c())}
  
  md_permits <- project_permits %>%
    mutate(
      permit_md = ifelse(
        is.na(link),
        glue("- {permit_type}: {license_date}"),
        glue("- <a href='{link}' target='_blank'>{permit_type}</a>: {license_date}"))) %>%
    group_by(project) %>%
    summarize(
      permits_md = paste(permit_md, collapse = "\n"),
      .groups = "drop")
  
  prj_sites <- projects %>% 
    left_join(
      md_permits, by = "project") %>% 
    mutate(
      label_md = glue(
        "**{project}** (_{technology_type}_)"),
      popup_md = glue(
        "**{project}** (_{technology_type}_)<br>
      Dates: {date_beg} to {ifelse(format(date_end, '%Y-%m-%d') == format(Sys.Date(), '%Y-%m-%d'), 'ongoing', format(date_end, '%Y-%m-%d'))}<br>
      Location (lon, lat): {longitude}, {latitude}<br>
      {permits_md}"),
      label_html = map_chr(label_md, md2html),
      popup_html = map_chr(popup_md, md2html)) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% 
    arrange(project)
  
  prj_sites %>% 
    st_drop_geometry() %>% 
    write_csv(prj_sites_csv)
}

