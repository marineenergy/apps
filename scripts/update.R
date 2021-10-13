get_gsheet_data <- function(sheet = "tags", sheet_id  = "https://docs.google.com/spreadsheets/d/1MTlWQgBeV4eNbM2JXNXU3Y-_Y6QcOOfjWFyKWfdMIQM/edit"){
  librarian::shelf(googlesheets4)
  
  # google sheet key from Google Console service account
  #   https://console.cloud.google.com/iam-admin/serviceaccounts/details/111453668228815650069/keys?authuser=2&organizationId=651265387478&project=marineenergy4gargle
  gs4_auth_json <- "/share/data/marineenergy4gargle.json" 
  # tags tab in [data | marineenergy.app - Google Sheet](https://docs.google.com/spreadsheets/d/1MTlWQgBeV4eNbM2JXNXU3Y-_Y6QcOOfjWFyKWfdMIQM/edit#gid=662531985)
  #   + shared sheet with: shares@marineenergy4gargle.iam.gserviceaccount.com
  # sheet_id  <- "1MTlWQgBeV4eNbM2JXNXU3Y-_Y6QcOOfjWFyKWfdMIQM"
  
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
  
  dbWriteTable(con, "projects", projects, overwrite = T)
  dbWriteTable(con, "project_permits", project_permits, overwrite = T)
  #dbListTables(con) %>% sort() %>% str_extract("proj.*") %>% na.omit()
  
  # store project names for matching with ferc_docs
  project_names <- projects %>% 
    select(prj = project) %>% 
    # tibble() %>% collect() %>% 
    mutate(
      prj_alt = case_when(
        prj == "Igiugig"       ~ "Iguigig",
        prj == "OPT Reedsport" ~ "REEDSPORT OPT",
        prj == "PacWave-N"     ~ "Pacwave North",
        prj == "PacWave-S"     ~ "Pacwave South",
        prj == "RITE"          ~ "Roosevelt Island Tidal Energy"))
  dbWriteTable(con, "project_names", project_names, overwrite = T)
  
  
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
  
  # project_sites used by Projects - Map for popup listing permits by site
  dbWriteTable(con, "project_sites", prj_sites, overwrite = T)
}

update_tags <- function(){
  
  # rename original tags
  # DBI::dbSendQuery(con, "ALTER TABLE tags RENAME TO tags_0;")
  # dbListTables(con) %>% sort()
  
  # read tags from gsheet
  tags  <- get_gsheet_data("tags") %>% 
    select(category, tag_sql, tag)
  
  tag_lookup <- get_gsheet_data("tag_lookup") %>% 
    select(content, tag_category, content_tag, tag_sql)
  
  # check tag_sql valid for ltree; 
  ck_valid_tag_sql <- function(d){
    d %>% 
      mutate(
        # ltree allows only certain characters
        tag_sql_fix = stringr::str_replace_all(tag_sql, "[^A-Za-z0-9_.]", ""),
        tag_sql_fix = ifelse(tag_sql_fix == tag_sql, NA, tag_sql_fix)) %>% 
      filter(!is.na(tag_sql_fix))
  }
  
  tags_tofix <- ck_valid_tag_sql(tags)
  stopifnot(nrow(tags_tofix) == 0)
  
  tag_lookup_tofix <- ck_valid_tag_sql(tags)
  stopifnot(nrow(tag_lookup_tofix) == 0)
  
  # check all tag_lookup.tag_sql in tags.tag_sql
  tag_lookup_notin_tags <- anti_join(tag_lookup, tags, by="tag_sql")
  stopifnot(tag_lookup_notin_tags == 0)
  
  # provide plural form if tagged at highest level of category
  categories_all <- c(
    Technology  = "All Technologies",
    Receptor    = "All Receptors",
    Stressor    = "All Stressors",
    Phase       = "All Phases",
    Management  = "All Management Categories",
    Consequence = "All Consequences")
  stopifnot(all(tags %>% distinct(category) %>% pull(category) %in% names(categories_all)))
  
  # add columns for fast, pretty printing to shiny and reports
  tags <- tags %>% 
    mutate(
      cat       = tolower(category),
      tag_nocat = purrr::map2_chr(tag, category, function(tag, category){
        stringr::str_replace(tag, glue("{category}/"), "")}),
      tag_nocat = ifelse(
        tag_nocat == tag,
        # provide plural form if tagged at highest level of category
        categories_all[tag_nocat],
        tag_nocat))
  
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
  
  # original docs repeats the row of detail for multiple tag interactions (nrow=687)
  docs <- get_gsheet_data("documents_import2db") %>% 
    rename(detail = key_interaction_detail) %>% 
    mutate(
      detail = map_chr(detail, iconv, from = "latin1", to = "ASCII", sub = ""))
                       
  # names(docs)[sapply(docs, class) == "logical"] %>% paste(collapse = ',\n') %>% cat()
  # across(docs, where(is.logical), )
  
  prjs <- dbReadTable(con, "project_names") %>% collect() %>% tibble()
  
  match_prj <- function(
    prj_doc, prj_names = prjs$prj, alt_prj_names = prjs$prj_alt) {
    prj_index <- prj_doc %>% 
      str_detect(
        coll(
          prj_names %>%
            str_replace_all("-", " "),
          ignore_case = T)) %>% 
      unlist(recursive = T)
    if (!(TRUE %in% prj_index)) {
      prj_index <- list()
      prj_index <- prj_doc %>% 
        str_detect(
          coll(
            alt_prj_names %>%
              str_replace_all("-", " "),
            ignore_case = T)) %>% 
        unlist(recursive = T)
    }
    # get prj name that matches prj
    if (!(TRUE %in% prj_index)) {
      prj_index <- NA
    } else {
      prj_index <- which(prj_index == TRUE)
    }
    if (!(is.na(prj_index))) {
      prj_name <- prj_names[prj_index]
    } else {
      prj_name <- NA
    }
    prj_name
  }
  
  # summarize the docs by the interaction detail (nrow=222)
  docs_ixns <- docs %>% 
    group_by(detail) %>% 
    summarize(
      project            = first(project), 
      prj_document       = first(`doc NAME`), 
      prj_doc_attachment = first(`ATTACHMENT NAME`), 
      prj_doc_attach_url = first(url), 
      ck_ixn             = first(presented_as_potential_interaction),
      ck_obs             = first(decribed_from_observations_at_the_project_site),
      ck_mp              = first(`monitoring_plan_(mp)`),
      ck_amp             = first(`adaptive_management_plan_(amp)`),
      ck_pme             = first(protection_mitigation_and_enhancement),
      ck_bmps            = first(bmps_applied),
      .groups = "drop") %>% 
    mutate(project = map_chr(prj_document, match_prj)) %>% 
    tibble::rownames_to_column("rowid") %>% 
    mutate(
      rowid = as.integer(rowid))
  
  # assign rowid based on the summarized docs_ixns
  docs <- docs %>% 
    left_join(
      docs_ixns %>% 
        select(rowid, detail), 
      by = "detail") %>% 
    relocate(rowid)
  
  # get all tags by category from wide to long (nrow=3,435)
  # update_tags()
  docs_tags <- bind_rows(
    docs %>% 
      mutate(
        content      = "ferc_docs",
        tag_category = "Technology") %>% 
      select(content, rowid, tag_category, content_tag = technology),
    docs %>% 
      mutate(
        content      = "ferc_docs",
        tag_category = "Stressor") %>% 
      select(content, rowid, tag_category, content_tag = stressor),
    docs %>% 
      mutate(
        content      = "ferc_docs",
        tag_category = "Receptor") %>% 
      select(content, rowid, tag_category, content_tag = receptor),
    docs %>% 
      mutate(
        content      = "ferc_docs",
        tag_category = "Phase") %>% 
      select(content, rowid, tag_category, content_tag = phase),
    # docs %>% 
    #   mutate(
    #     content      = "ferc_docs",
    #     tag_category = "Effect") %>% 
    docs %>% 
      mutate(
        content      = "ferc_docs",
        tag_category = "Consequence") %>% 
      select(content, rowid, tag_category, content_tag = consequence)) %>% 
    mutate(content_tag = na_if(content_tag, "NA")) %>% 
    filter(!is.na(content_tag))
  
  # get authoritative tags
  tags <- tbl(con, "tags") %>% 
    collect() %>% 
    mutate(
      tag_sql = as.character(tag_sql))
  
  # create tags_lookup for manually matching...
  # docs_tags_lookup <- docs_tags %>% 
  #   group_by(content, tag_category, content_tag) %>% 
  #   filter(!is.na(content_tag)) %>% 
  #   summarize(.groups = "drop") %>% 
  #   #View()
  #   mutate(
  #     content_tag_strip = stringr::str_replace_all(content_tag, "[^A-Za-z0-9_.]", ""),
  #     content_tag_sql   = as.character(glue("{tag_category}.{content_tag_strip}"))) %>% 
  #   left_join(
  #     tags,
  #     by = c("content_tag_sql" = "tag_sql")) %>% 
  #   arrange(is.na(tag), tag_category, content_tag)
  # 
  # docs_tags_lookup %>%
  #   filter(is.na(tag)) %>%
  #   write_csv(here("data/docs_tags_lookup_na.csv"))
  # table(!is.na(docs_tags_lookup$tag))
  # View(docs_tags_lookup)
  # 
  # readr::write_csv(docs_tags_lookup, here("data/docs_tags_lookup.csv"))
  
  # copy/pasted docs_tags_lookup.csv into gsheet
  # TODO: finish Effect.* lookups
  
  tag_lookup <- get_gsheet_data("tag_lookup") %>% 
    filter(content == "ferc_docs") %>% 
    select(-content, -content_tag_extra)
  # TODO: match Effect.* content_tag with tag_sql
  
  docs_tags <- docs_tags %>% 
    left_join(
      tag_lookup,
      by = c("tag_category", "content_tag"))
  
  # check that all tags matched in tag_lookup
  docs_tags_nolookup <- docs_tags %>% 
    filter(is.na(tag_sql), !is.na(content_tag)) %>% 
    group_by(tag_category, content_tag) %>% 
    summarize(.groups = "drop")
  stopifnot(nrow(docs_tags_nolookup)==0)
  
  tags <- tbl(con, "tags") %>% 
    collect() %>% 
    mutate(
      tag_sql = as.character(tag_sql))
  
  # write tables to db
  dbWriteTable(con, "ferc_docs", docs_ixns, overwrite=T)
  # ferc_docs:      rowid | key_interaction_detail
  #  1 to many with :
  #   ferc_docs_tags: rowid | tag_sql
  dbWriteTable(con, "ferc_doc_tags", docs_tags, overwrite=T)
  dbExecute(con, "ALTER TABLE ferc_doc_tags ALTER COLUMN tag_sql TYPE ltree USING text2ltree(tag_sql);")
  dbExecute(con, "CREATE INDEX idx_ferc_doc_tags_tag_sql ON ferc_doc_tags USING GIST (tag_sql);")
  # TODO: add db relationship: ferc_doc_tags.tag_sql <-> tags.tag_sql
  
  # check table creation
  # DBI::dbListTables(con) %>% sort() %>% stringr::str_subset("^shp_", negate=T)
}

update_tethys_pubs <- function(){
  # update db tables: tethys_pubs, tethys_pub_tags; plus data/tethys_docs.[json|csv]
  
  # source(here("scripts/common.R")); source(here("scripts/db.R"))
  shelf(jsonlite, purrr, readr)
  
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
  #  [1] "URI"              "type"             "landingPage"      "sourceURL"        "title"           
  #  [6] "description"      "author"           "organization"     "originationDate"  "spatial"         
  # [11] "technologyType"   "tags"             "modifiedDate"     "signatureProject"

  d <- tibble(
    uri   = map_chr(tethys_content, "URI"),
    rowid = str_replace(uri, "https://tethys.pnnl.gov/node/", "") %>% as.integer(),
    title = map_chr(tethys_content, "title"),
    data  = map_chr(tethys_content, toJSON) %>% 
      str_replace_all("'","''")) %>% 
    relocate(rowid) %>% 
    filter(!duplicated(uri))
  write_csv(d, tethys_docs_csv)
  
  # TODO: rename table tethys_pubs -> tethys_docs and read fxns in Shiny report app
  # dbRemoveTable(con, "tethys_pubs")
  sql <- glue("
    CREATE TABLE IF NOT EXISTS tethys_pubs (
  	  rowid INTEGER NOT NULL PRIMARY KEY,
  	  uri TEXT NOT NULL,
  	  title TEXT,
  	  data JSON NOT NULL);")
  dbExecute(con, sql)
  dbExecute(con, "DELETE FROM tethys_pubs;")
  #tbl(con, "tethys_pubs")
  
  # run once in Terminal to install software and test connection to database:
  #   sudo apt-get update; sudo apt-get install postgresql-client
  # pgpassword set at top:
  pass <- readLines("/share/.password_mhk-env.us")
  Sys.setenv(PGPASSWORD=pass) # for psql command line
  cmd <- glue('cat {tethys_docs_csv} | psql -h postgis -p 5432 -U admin -c "COPY tethys_pubs (rowid, uri, title, data) FROM STDIN WITH (FORMAT CSV, HEADER TRUE);" gis')
  system(cmd)
  
  # dbExecute(con, "ALTER TABLE tethys_pubs ADD COLUMN title TEXT")
  # # tbl(con, "tethys_pubs")
  # dbExecute(con, "
  #   UPDATE tethys_pubs 
  #   SET
  #     title = data -> 'title' ->> 0")
  
  # update tables for easier querying
  docs <- dbGetQuery(
    con, 
    "SELECT 
     rowid, uri, title,
     data -> 'tags'                 AS tags, 
     data -> 'technologyType'       AS technologyType
   FROM tethys_pubs") %>% 
    arrange(rowid) %>% 
    tibble()
  # docs # 6,484 rows
  # docs %>% head(10) %>% View()
  
  # TODO: evaluate counts of tags, esp. "Environment"
  # TODO: use rowid instead of uri, but affects old site
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
    mutate(
      rowid = str_replace(
        uri, "https://tethys.pnnl.gov/node/", "") %>% 
        as.integer()) %>% 
    tibble()
  
  tag_lookup <- get_gsheet_data("tag_lookup") %>% 
    filter(content == "tethys_pubs") %>% 
    select(-content, -content_tag_extra)
  # TODO: match Effect.* content_tag with tag_sql
  
  doc_tags <- doc_tags %>% 
    left_join(
      tag_lookup,
      by = c("tag" = "content_tag"))
  
  # TODO: rename table tethys_pub_tags -> tethys_doc_tags and read fxns in Shiny report app
  dbWriteTable(con, "tethys_pub_tags", doc_tags, overwrite=T)
  dbExecute(con, "ALTER TABLE tethys_pub_tags ALTER COLUMN tag_sql TYPE ltree USING text2ltree(tag_sql);")
  dbExecute(con, "CREATE INDEX idx_tethys_pub_tags_tag_sql ON tethys_pub_tags USING GIST (tag_sql);")
  dbExecute(con, "CREATE INDEX idx_tethys_pub_rowid ON tethys_pub_tags USING BTREE (rowid);")
  dbExecute(con, "CREATE INDEX idx_tethys_pubs_rowid ON tethys_pubs USING BTREE (rowid);")
  # doc_tags # 14,505 rows
  # doc_tags # 16,034 rows after UNION
  
  doc_tags %>% 
    select(content_tag = tag) %>% 
    group_by(content_tag) %>% 
    summarize(
      content_tag_extra = n(),
      .groups = "drop") %>% 
    mutate(
      tag_category = "",
      content      = "tethys_pubs", 
      tag_sql      = "") %>% 
    select(
      content, tag_category, content_tag, content_tag_extra, tag_sql) %>% 
    write_csv(here("data/tethys_pub_tag_lookup.csv"))
  
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

update_spatial <- function(){
  # update db tables: spatial, spatial_tags; data/mc_spatial.csv
  
  # [spatial | marineenergy.app - Google Sheets](https://docs.google.com/spreadsheets/d/1MMVqPr39R5gAyZdY2iJIkkIdYqgEBJYQeGqDk1z-RKQ/edit#gid=936111013)
  d_sp <- get_gsheet_data("datasets", "1MMVqPr39R5gAyZdY2iJIkkIdYqgEBJYQeGqDk1z-RKQ") %>%
      mutate(
        ready = as.logical(ready)) %>% 
    filter(ready) %>% 
    replace_na(list(buffer_km = 0)) %>% 
    select(-notes, -issues)
  
  tag_lookup <- get_gsheet_data("tag_lookup") %>% 
    filter(content == "tethys_pubs") %>% 
    select(-content, -content_tag_extra)
  
  d_sp_tag <- d_sp %>%   
    separate_rows(tags, sep = ";") %>% 
    rename(tag = tags)
  
  # check that we're not missing any spatial tags
  stopifnot(length(setdiff(d_sp_tag$tag, tag_lookup$content_tag)) == 0)
  
  d_sp_tags <- d_sp_tag %>% 
    left_join(
      tag_lookup,
      by = c("tag" = "content_tag")) %>% 
    select(rowid, tag_sql)
  
  # write tables to filesys and database
  write_csv(d_sp, here("data/mc_spatial.csv"))
  dbWriteTable(con, "mc_spatial", d_sp, overwrite=T)
  dbWriteTable(con, "mc_spatial_tags", d_sp_tags, overwrite=T)
  dbExecute(con, "ALTER TABLE mc_spatial_tags ALTER COLUMN tag_sql TYPE ltree USING text2ltree(tag_sql);")
  dbExecute(con, "CREATE INDEX idx_mc_spatial_tags_tag_sql ON mc_spatial_tags USING GIST (tag_sql);")
  dbExecute(con, "CREATE INDEX idx_mc_spatial_tags_rowid ON mc_spatial_tags USING BTREE (rowid);")
  dbExecute(con, "CREATE INDEX idx_mc_spatial_rowid ON mc_spatial USING BTREE (rowid);")
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
    mutate(across(where(is.character), ~na_if(., "n/a"))) %>% 
    tibble()
  
  #paste(names(mgt), collapse = ", ")
  # Technology, Management Measure Category, Phase of Project, Stressor, Receptor, 
  #   Specific Receptor, Interaction, Specific Management Measures, Implications of Measure
  mgt <- mgt %>% 
    group_by(Interaction, `Specific Management Measures`, `Implications of Measure`) %>% 
    nest() %>% 
    ungroup() %>% 
    rowid_to_column("rowid")
  
  # write mgt_csv
  mgt %>% 
    select(-data) %>% 
    write_csv(mgt_csv)
  
  # OLD: match tags from tag_lookup in db
  # tag_lookup <- tbl(con, "tag_lookup") %>% 
  #   filter(content == "tethys_mgt")
  tag_lookup <- get_gsheet_data("tag_lookup") %>% 
    filter(content == "tethys_mgt") %>% 
    select(-content)
  #table(tag_lookup$tag_category)
  # Management      Phase   Receptor   Stressor Technology 
  #          5          3         39         22          2
  # TODO: update db tag_lookup, optional?
  
  get_tag_content <- function(d_r){
    # d_r <- mgt$data[[1]]
    d_r %>% 
      select(
        Technology, 
        Stressor, 
        Management = `Management Measure Category`,
        Phase      = `Phase of Project`) %>% 
      pivot_longer(
        everything(), 
        names_to  = "tag_category", 
        values_to = "content_tag") %>% 
      bind_rows(
        d_r %>% 
          select(
            content_tag       = Receptor, 
            content_tag_extra = `Specific Receptor`) %>% 
          mutate(
            tag_category = "Receptor") %>% 
          select(tag_category, content_tag, content_tag_extra)) %>% 
      distinct(tag_category, content_tag, content_tag_extra) %>% 
      arrange(desc(tag_category), content_tag, content_tag_extra)
  }
  
  mgt_tags <- mgt %>%
    select(rowid, data) %>% 
    mutate(
      tags = map(data, get_tag_content)) %>% 
    select(-data) %>% 
    unnest(tags) %>% 
    left_join(
      tag_lookup,
      by = c("tag_category", "content_tag", "content_tag_extra")) %>% 
    arrange(rowid, tag_sql, tag_category, content_tag, content_tag_extra)
  
  mgt_tags_missing_tag_sql <- mgt_tags %>% filter(is.na(tag_sql))
  stopifnot(nrow(mgt_tags_missing_tag_sql) == 0)
  
  mgt_tags %>% 
    select(rowid, tag_sql) %>% 
    write_csv(mgt_tags_csv)
  
  mgt      <- read_csv(mgt_csv, col_types = cols())
  mgt_tags <- read_csv(mgt_tags_csv, col_types = cols())
  
  # TODO: ALTER TABLE tethys_mgt_tags ALTER COLUMN rowid PRIMARY KEY.
  # TODO: ADD FOREIGN KEYS for tethys_mgt
  dbWriteTable(con, "tethys_mgt"     , mgt     , overwrite = T)
  dbWriteTable(con, "tethys_mgt_tags", mgt_tags, overwrite = T)
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

