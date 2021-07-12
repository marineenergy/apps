get_gsheet_data <- function(sheet = "tags"){
  librarian::shelf(googlesheets4)
  
  # google sheet key from Google Console service account
  #   https://console.cloud.google.com/iam-admin/serviceaccounts/details/111453668228815650069/keys?authuser=2&organizationId=651265387478&project=marineenergy4gargle
  gs4_auth_json <- "/share/data/marineenergy4gargle.json" 
  # tags tab in [data | marineenergy.app - Google Sheet](https://docs.google.com/spreadsheets/d/1MTlWQgBeV4eNbM2JXNXU3Y-_Y6QcOOfjWFyKWfdMIQM/edit#gid=662531985)
  #   + shared sheet with: shares@marineenergy4gargle.iam.gserviceaccount.com
  # sheet_id  <- "1MTlWQgBeV4eNbM2JXNXU3Y-_Y6QcOOfjWFyKWfdMIQM"
  sheet_id  <- "https://docs.google.com/spreadsheets/d/1MTlWQgBeV4eNbM2JXNXU3Y-_Y6QcOOfjWFyKWfdMIQM/edit"
  
  # googledrive
  stopifnot(file.exists(gs4_auth_json))
  gs4_auth(path = gs4_auth_json)
  
  # rename original tags
  # DBI::dbSendQuery(con, "ALTER TABLE tags RENAME TO tags_0;")
  # dbListTables(con) %>% sort()
  
  
  # read tags from gsheet
  read_sheet(sheet_id, sheet)
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

update_tags <- function(){
  
  # rename original tags
  # DBI::dbSendQuery(con, "ALTER TABLE tags RENAME TO tags_0;")
  # dbListTables(con) %>% sort()
  
  
  # read tags from gsheet
  tags  <- get_gsheet_data("tags") %>% 
    select(category, tag_sql, tag)
  
  tag_lookup <- get_gsheet_data("tag_lookup") %>% 
    select(content, tag_category, tag_content, tag_sql)
  
  # check tag_sql valid for ltree; 
  ck_valid_tag_sql <- function(d){
    d %>% 
      mutate(
        # ltree allows only certain characters
        tag_sql_fix = str_replace_all(tag_sql, "[^A-Za-z0-9_.]", ""),
        tag_sql_fix = ifelse(tag_sql_fix == tag_sql, NA, tag_sql_fix)) %>% 
      filter(!is.na(tag_sql_fix))
  }
  
  tags_tofix <- ck_tag_sql(tags)
  stopifnot(nrow(tags_tofix) == 0)
  
  tag_lookup_tofix <- ck_tag_sql(tags)
  stopifnot(nrow(tag_lookup_tofix) == 0)
  
  # check all tag_lookup.tag_sql in tags.tag_sql
  tag_lookup_notin_tags <- anti_join(tag_lookup, tags, by="tag_sql")
  stopifnot(tag_lookup_notin_tags == 0)
  
  # write tags to db
  dbWriteTable(con, "tags", tags, overwrite=T)
  dbWriteTable(con, "tag_lookup", tag_lookup, overwrite=T)
  #  dbListTables(con) %>% str_subset("^tag")
  
  dbExecute(con, "CREATE EXTENSION IF NOT EXISTS ltree;")
  dbExecute(con, "ALTER TABLE tags ALTER COLUMN tag_sql TYPE ltree USING text2ltree(tag_sql);")
  dbExecute(con, "ALTER TABLE tag_lookup ALTER COLUMN tag_sql TYPE ltree USING text2ltree(tag_sql);")
  dbExecute(con, "CREATE INDEX idx_tags_tag_sql ON tags USING GIST (tag_sql);")
  dbExecute(con, "CREATE INDEX idx_tag_lookup_tag_sql ON tag_lookup USING GIST (tag_sql);")
}

update_ferc_docs <- function(){
  
  # rename original tags
  # DBI::dbSendQuery(con, "ALTER TABLE tags RENAME TO tags_0;")
  # dbListTables(con) %>% sort()
  # source(here::here("scripts/common.R"))
  # source(file.path(dir_scripts, "db.R"))
  # source(file.path(dir_scripts, "update.R"))
  
  # read tags from gsheet
  docs  <- get_gsheet_data("documents")
  
  
  
  stressors <- docs %>% 
    select(stressor)
  
  
  tag_lookup <- get_gsheet_data("tag_lookup") %>% 
    select(content, tag_category, tag_content, tag_sql)
  
  # check tag_sql valid for ltree; 
  ck_valid_tag_sql <- function(d){
    d %>% 
      mutate(
        # ltree allows only certain characters
        tag_sql_fix = str_replace_all(tag_sql, "[^A-Za-z0-9_.]", ""),
        tag_sql_fix = ifelse(tag_sql_fix == tag_sql, NA, tag_sql_fix)) %>% 
      filter(!is.na(tag_sql_fix))
  }
  
  tags_tofix <- ck_tag_sql(tags)
  stopifnot(nrow(tags_tofix) == 0)
  
  tag_lookup_tofix <- ck_tag_sql(tags)
  stopifnot(nrow(tag_lookup_tofix) == 0)
  
  # check all tag_lookup.tag_sql in tags.tag_sql
  tag_lookup_notin_tags <- anti_join(tag_lookup, tags, by="tag_sql")
  stopifnot(tag_lookup_notin_tags == 0)
  
  # write tags to db
  dbWriteTable(con, "tags", tags, overwrite=T)
  dbWriteTable(con, "tag_lookup", tag_lookup, overwrite=T)
  #  dbListTables(con) %>% str_subset("^tag")
  
  dbExecute(con, "CREATE EXTENSION IF NOT EXISTS ltree;")
  dbExecute(con, "ALTER TABLE tags ALTER COLUMN tag_sql TYPE ltree USING text2ltree(tag_sql);")
  dbExecute(con, "ALTER TABLE tag_lookup ALTER COLUMN tag_sql TYPE ltree USING text2ltree(tag_sql);")
  dbExecute(con, "CREATE INDEX idx_tags_tag_sql ON tags USING GIST (tag_sql);")
  dbExecute(con, "CREATE INDEX idx_tag_lookup_tag_sql ON tag_lookup USING GIST (tag_sql);")
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

update_tethys_mgt <- function(){
  
  mgt_url      <- "https://tethys.pnnl.gov/management-measures"
  mgt_csv      <- here("data/tethys_mgt.csv")
  mgt_tags_csv <- here("data/tethys_mgt_tags.csv")
  
  # read web
  mgt <- read_html(mgt_url) %>% 
    html_table() %>% 
    .[[1]] %>% 
    tibble() %>% 
    rowid_to_column("rowid")
  
  # write mgt_csv
  write_csv(mgt, mgt_csv)
  
  # match tags
  tag_lookup <- tbl(con, "tag_lookup") %>% 
    filter(content == "tethys_mgt") %>% 
    collect()
  
  mgt <- read_csv(mgt_csv, col_types = cols())
  
  mgt_tags_no_tag_sql <- mgt %>% 
    select(
      rowid,
      Technology, 
      Stressor, 
      Management = `Management Measure Category`,
      Phase      = `Phase of Project`) %>% 
    pivot_longer(
      everything() & -one_of("rowid"), 
      names_to="tag_category", 
      values_to="content_tag") %>% 
    bind_rows(
      mgt %>% 
        select(
          rowid,
          content_tag       = Receptor, 
          content_tag_extra = `Specific Receptor`) %>% 
        mutate(
          tag_category = "Receptor") %>% 
        select(rowid, tag_category, content_tag, content_tag_extra)) %>% 
    select(rowid, tag_category, content_tag, content_tag_extra) %>% 
    arrange(rowid, tag_category, content_tag, content_tag_extra)
  
  mgt_tag_lookup <- mgt_tags_no_tag_sql %>% 
    group_by(tag_category, content_tag, content_tag_extra) %>% 
    summarise(.groups="drop") %>% 
    mutate(
      content = "tethys_mgt") %>% 
    relocate(content)
  # mgt_tag_lookup
  # TODO: compare  missing to get_gsheet_data(), tag_lookup tab
  
  mgt_tag_lookup <- get_gsheet_data("tag_lookup") %>% 
    filter(content == "tethys_mgt")
  
  mgt_tags <- mgt_tags_no_tag_sql %>% 
    left_join(
      mgt_tag_lookup,
      by = c("tag_category", "content_tag", "content_tag_extra")) %>% 
    arrange(rowid, tag_category, content_tag, content_tag_extra) # mgt_tags
  mgt_tags_missing_tag_sql <- mgt_tags %>% filter(is.na(tag_sql))
  stopifnot(nrow(mgt_tags_missing_tag_sql) == 0)
  
  write_csv(mgt_tags, mgt_tags_csv)
  
  tbl_mgt_tags <- mgt_tags %>% 
    select(rowid, tag_sql)
  
  # View(mgt_tags)
  
  
  # TODO: ALTER TABLE tethys_mgt_tags ALTER COLUMN rowid PRIMARY KEY.
  # TODO: ADD FOREIGN KEYS for tethys_mgt
  dbWriteTable(con, "tethys_mgt"     , mgt         , overwrite = T)
  dbWriteTable(con, "tethys_mgt_tags", tbl_mgt_tags, overwrite = T)
  dbExecute(con, "ALTER TABLE tethys_mgt_tags ALTER COLUMN tag_sql TYPE ltree USING text2ltree(tag_sql);")
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

