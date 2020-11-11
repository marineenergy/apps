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
  DT, gt, htmltools, htmlwidgets, kableExtra, knitr, markdown, rmarkdown, shiny, webshot,
  # utility
  fs, glue, here, stringr, urltools)
here <- here::here

# rstudio/webshot2
# wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
# sudo dpkg -i google-chrome-stable_current_amd64.deb; sudo apt-get -fy install

# webshot
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


datasets_gsheet2db <- function(tbl = "datasets", redo = T){
  # tbl = "datasets" OR "datasets_mc"
  
  # datasets_marinecadastre.gov.csv - Google Sheet
  #   edit online: https://docs.google.com/spreadsheets/d/1MMVqPr39R5gAyZdY2iJIkkIdYqgEBJYQeGqDk1z-RKQ/edit#gid=0
  gid <- "1MMVqPr39R5gAyZdY2iJIkkIdYqgEBJYQeGqDk1z-RKQ"
  csv <- glue("https://docs.google.com/spreadsheets/d/{gid}/gviz/tq?tqx=out:csv&sheet={tbl}")
  
  d <- read_csv(csv) %>% 
    select(-starts_with("X"))
  
  if (!tbl %in% dbListTables(con) | redo)
    dbWriteTable(con, tbl, d, overwrite=T)
}
# datasets_gsheet2db()


tabulate_dataset_shp_within_aoi <- function(dataset_code, aoi_wkt){
  # summarize shapefile dataset from area of interest
  
  # dataset_code = "cetacean-bia"; aoi_wkt = params$aoi_wkt
  # dataset_code = "efh"; aoi_wkt = "POLYGON ((-67.06819 44.99416, -67.1857 44.94707, -67.21651 44.88058, -67.15834 44.78871, -67.04385 44.81789, -66.91015 44.86279, -67.06819 44.99416))"
  
  ds <- tbl(con, "datasets") %>% 
    filter(code == !!dataset_code) %>% 
    replace_na(list(buffer_nm = 0)) %>% 
    collect()
  
  if (!is.na(ds$summarize_sql)){
    sql_intersection <- glue("
      {ds$select_sql} AS ds
      WHERE ST_DWithin(Geography(ds.geometry), 'SRID=4326;{aoi_wkt}', {ds$buffer_nm} * 1852);")
    dbExecute(con, glue("CREATE TEMPORARY TABLE tmp_aoi AS {sql_intersection};"))
    sql_summarize <- "SELECT sitename_l AS Species, string_agg(lifestage, ', ') AS Lifestage FROM tmp_aoi GROUP BY sitename_l"
    x_df <- dbGetQuery(con, sql_summarize)
  } else {
    x_sql <- glue("
      {ds$select_sql} AS ds
      WHERE ST_DWithin(Geography(ds.geometry), 'SRID=4326;{aoi_wkt}', {ds$buffer_nm} * 1852);")
    x_sf <- st_read(con, query = x_sql)
    x_df <- st_drop_geometry(x_sf)
    
    if (!is.na(ds$summarize_r))
      eval(parse(text=ds$summarize_r))
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
  
  caption_md <- glue("Literature from [Tethys Knowledge Base](https://tethys.pnnl.gov/knowledge-base-all)}.")
  
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