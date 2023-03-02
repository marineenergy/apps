
# DB CONNECTION & SCRIPTS ----
dir_scripts <<- here::here("scripts")
source(file.path(dir_scripts, "common_2.R"))
source(file.path(dir_scripts, "db.R"))
source(file.path(dir_scripts, "edit_interface.R"))
source(file.path(dir_scripts, "shiny_report.R"))
source(file.path(dir_scripts, "update.R"))

# LIBRARIES ----
# devtools::install_github("DavidPatShuiFong/DTedit@f1617e253d564bce9b2aa67e0662d4cf04d7931f")
shelf(
  bbest/DTedit, # DavidPatShuiFong/DTedit, # [@bbest pr](https://github.com/DavidPatShuiFong/DTedit/pull/35)
  DBI, DT, 
  glue, purrr, readr, tidyr,
  shiny, shinycssloaders)
#devtools::load_all("/share/github/DTedit") 
#devtools::install_local("/share/github/DTedit") # value <- ifelse(is.na(value), FALSE, value) # FIXES ERROR if NA: missing value where TRUE/FALSE needed
options(readr.show_col_types = FALSE)

# launch with reactlog
#   library(reactlog); reactlog_enable(); app <- runApp(here::here("edit"))
# once app has closed, display reactlog from shiny
#   shiny::reactlogShow()

#options(error = recover)

# initialize tables if missing ----

tbl(con, "ferc_doc_tags")
tbl(con, "ba_doc_tags") %>% 
  

# FXNS REFERENCED IN CALLBACKS ----

# convert data from dtedit to ba_docs format  
get_new_docs <- function(d, flds = ba_doc_names) {
  d %>%
    select(
      -document, -project, -prj_doc_sec, -prj_doc_sec_values, 
      -prj_document, -prj_doc_attachment, -prj_doc_attach_url) %>% 
    left_join(prj_doc_sec_lookup, by = "prj_doc_sec_display") %>%
    rename(
      project = prj, prj_document = doc, prj_doc_attachment = sec,
      prj_doc_attach_url = url) %>% 
    # separate(
    #   prj_doc_sec_values,
    #   into = c('project', 'prj_document', 'prj_doc_attachment'),
    #   sep  = ";;") %>% 
    select(flds) %>% 
    relocate(flds) %>% 
    arrange(rowid)
} 

# convert data from dtedit to ba_doc_tags format
get_new_tags <- function(d, flds = ba_tag_names) {
  # tbl(con, "ba_doc_tags") %>% collect() %>% names() %>% paste(collapse = ', ')

  d %>% 
    select(rowid, tag_named) %>% 
    unnest(tag_named) %>% 
    select(rowid, tag_sql = tag_named) %>% 
    arrange(rowid, tag_sql)
}

# DATA SETUP ----
# TODO: find home for creation of these in db, borrowing from prj_subpages_test.Rmd

# dbListTables(con) %>% str_subset("ba")
# [1] "ba"                 "ba_prj"             "ba_projects"        "ba_docs"           
# [5] "ba_project_doc_sec" "ba_doc_tags" 
# dbReadTable(con, "ba_project_doc_sec")

# prj_sites_lookup <- read_csv(here("data/project_sites.csv")) %>%
#   arrange(project)
prj_sites_lookup <- tbl(con, "ba_sites") |> 
  arrange(ba_project_code) |> 
  collect()

# prj_doc_sec_lookup <- dbReadTable(con, "ferc_project_doc_sec") %>%
#   tibble() %>% collect() %>%
#   filter(!is.na(prj))
prj_doc_sec_lookup <- tbl(con, "ba_project_doc_sec") |> 
  filter(!is.na(ba_project_code)) |> 
  collect()
  
# prj_doc_lookup <- prj_doc_sec_lookup %>% 
#   group_by(prj, doc) %>% summarize() %>% ungroup()
prj_doc_lookup <- prj_doc_sec_lookup |> 
  group_by(ba_project_code, ba_doc) |> 
  summarize(.groups = "drop")

# CALLBACK FXNS ----

# INSERT
ba.insert.callback <- function(data, row) {
  # browser()
  d <- data %>% slice(row) %>% 
    na_if("NA") %>% na_if("") %>% 
    mutate(rowid = max(get_ba()$rowid) + 1)
  d_docs <- get_new_docs(d) # %>% tibble() # data to INSERT into ba_docs
  d_tags <- get_new_tags(d) # %>% tibble()  # data to INSERT into ba_doc_tags
  
  conn <- poolCheckout(con)
  sql_insert_docs <- glue_data_sql(
    d_docs,
    "INSERT INTO ba_docs VALUES
      ({rowid}, {detail}, {project},
      {prj_document}, {prj_doc_attachment}, {prj_doc_attach_url},
      {ck_ixn}, {ck_obs}, {ck_mp}, {ck_amp}, {ck_pme}, {ck_bmps})",
    .con = conn)
  poolReturn(conn)
  res <- try(dbExecute(con, sql_insert_docs))
  if ("try-error" %in% class(res)) stop(res)
  dbAppendTable(con, "ba_doc_tags", d_tags)
  # DBI::dbAppendTable(conn, "ba_doc_tags", d_tags)
  
  get_ba()
}

# UPDATE
ba.update.callback <- function(data, olddata, row) {
  # browser()
  d <- data %>% slice(row) %>% 
    tibble() %>% 
    mutate(across(starts_with("ck_"), as.logical)) %>% 
    na_if("NA") %>% 
    na_if("") 
  d_docs <- get_new_docs(d)  # data to UPDATE ba_docs
  d_tags <- get_new_tags(d)  # data to be APPENDED to ba_doc_tags
  
  conn <- poolCheckout(con)
  sql_update_docs <- glue_data_sql(
    d_docs,
    "UPDATE ba_docs 
      SET
        rowid              = {rowid}, 
        detail             = {detail}, 
        project            = {project},
        prj_document       = {prj_document}, 
        prj_doc_attachment = {prj_doc_attachment}, 
        prj_doc_attach_url = {prj_doc_attach_url},
        ck_ixn             = {ck_ixn}, 
        ck_obs             = {ck_obs}, 
        ck_mp              = {ck_mp}, 
        ck_amp             = {ck_amp}, 
        ck_pme             = {ck_pme}, 
        ck_bmps            = {ck_bmps}
      WHERE rowid = {rowid}",
    .con = conn)
  poolReturn(conn)
  sql_delete_tags <- glue("
    DELETE FROM ba_doc_tags WHERE rowid = {d$rowid};")
  
  res <- try(dbExecute(con, sql_update_docs))
  if ("try-error" %in% class(res)) stop(res)
  
  res <- try(dbExecute(con, sql_delete_tags))
  if ("try-error" %in% class(res)) stop(res)
  DBI::dbAppendTable(con, "ba_doc_tags", d_tags)
  # DBI::dbAppendTable(conn, "ba_doc_tags", d_tags)
  
  get_ba()
}

# DELETE
ba.delete.callback <- function(data, row) {
  # browser()
  d <- data %>% slice(row) %>% na_if("NA") %>% na_if("")
  sql_delete_docs <- glue("DELETE FROM ba_docs WHERE rowid = {d$rowid};")
  sql_delete_tags <- glue("DELETE FROM ba_doc_tags WHERE rowid = {d$rowid}")
  
  res <- try(dbExecute(con, sql_delete_docs))
  if ("try-error" %in% class(res)) stop(res)
  
  res <- try(dbExecute(con, sql_delete_tags))
  if ("try-error" %in% class(res)) stop(res)
  
  get_ba()
}

#* get additional data ----
ba <- get_ba() 
tags <- get_tags() 
ba_doc_names <- dbReadTable(con, "ba_docs") %>% names()
ba_tag_names <- dbReadTable(con, "ba_doc_tags") %>% names()

# * get input choices ----
tag_choices <- list()
for (category in unique(tags$category[tags$category != "Management"])){ # category = tags$category[1]
  tag_choices <- append(
    tag_choices,
    setNames(
      list(
        tags %>% 
          filter(category != "Management") %>% # mgmt under tethys so exclude
          filter(category == !!category) %>% 
          pull(tag_named) %>% 
          unlist()),
      category)) 
}

# prj_doc_sec_choices <- list()
# for (project in unique(get_ba()$project)){ 
#   prj_doc_sec_choices <- append(
#     prj_doc_sec_choices,
#     setNames(
#       list(
#         ba %>% 
#           filter(project == !!project) %>% 
#           pull(prj_doc_sec) %>% 
#           unlist()),
#       project)) 
# }


# * get labels (dtedit fld names)
#* get labels for dtedit ----
# construct first column: cat(paste(str_pad(glue('"{names(ba)}"'), max(nchar(names(ba))), "right"), collapse = '\n'))
labels <- tribble(
  ~fld                 ,  ~view_label,  ~edit_label,                                                 ~delete_label,
  # -------------------|-------------|------------------------------------------------------------|----------------
  "rowid"              ,   "ID"      ,   NA                                                       ,  "ID",
  "project"            ,   "Project" ,   NA                                                       ,  "Project",
  "document"           ,   "Document",   NA                                                       ,  NA,
  "prj_document"       ,    NA       ,   NA                                                       ,  "Document",
  "prj_doc_attachment" ,    "Section",   NA                                                       ,  "Document section",
  "prj_doc_sec"        ,    NA       ,   NA                                                       ,  NA,
  "prj_doc_sec_display",    NA       ,   "Project, document, and document section (if applicable)",  NA,
  "prj_doc_sec_values" ,    NA       ,   NA                                                       ,  NA,
  "detail"             ,    "Detail" ,   "Key interaction detail"                                 ,  "Key interaction detail",
  "tag_sql"            ,    NA       ,   NA                                                       ,  NA,
  "tag_named"          ,    NA       ,   "Tags"                                                   ,  "Tags",
  "tag_html"           ,    "Tags"   ,   NA                                                       ,  NA,
  "prj_doc_attach_url" ,    NA       ,   NA                                                       ,  NA,
  "ck_ixn"             ,    "Ixn"    ,   "Presented as potential interaction?"                    ,  "Presented as potential interaction?",
  "ck_obs"             ,    "Obs"    ,   "Described from observations at the project site?"       ,  "Described from observations at the project site?",   
  "ck_mp"              ,    "MP?"    ,   "Monitoring plan (MP)?"                                  ,  "Monitoring plan (MP)?",
  "ck_amp"             ,    "AMP?"   ,   "Adaptive management plan (AMP)?"                        ,  "Adaptive management plan (AMP)?",
  "ck_pme"             ,    "PME?"   ,   "Protection mitigation and enhancement (PME)?"           ,  "Protection mitigation and enhancement (PME)?",
  "ck_bmps"            ,    "BMPs?"  ,   "Best management practices (BMPs) applied?"              ,  "Best management practices (BMPs) applied?" 
)


# UPDATE DTEDIT PAGE on click
# update_dtedit_page <- function() {
#   prj_doc_sec_lookup <- dbReadTable(con, "ba_project_doc_sec") %>% 
#     tibble() %>% collect()
#   d_prj_doc <- prj_doc_sec_lookup %>% 
#     group_by(prj, doc) %>% summarize() %>% ungroup()
#   prj_doc_sec_choices(
#     prj_doc_sec_lookup %>% pull(prj_doc_sec_display) %>% sort() %>% unique())
#   showModal(
#     modalDialog(
#       "Input choices have been refreshed from those added on project docs page.",
#       footer    = modalButton("Continue"),
#       easyClose = TRUE,
#       size      = "s",
#       fade      = TRUE,
#       style     = "font-weight: bold;"))
# }
