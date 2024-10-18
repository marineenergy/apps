
# DB CONNECTION & SCRIPTS ----
dir_scripts <<- here::here("scripts")
source(file.path(dir_scripts, "common_2.R"))
source(file.path(dir_scripts, "db.R"))
source(file.path(dir_scripts, "edit_interface.R"))
source(file.path(dir_scripts, "shiny_report.R"))
source(file.path(dir_scripts, "update.R"))

# https://stackoverflow.com/questions/64239320/dbpool-object-expiring
ck_con <- function(){
  if (!con$valid)
    source(file.path(dir_scripts, "db.R"))
}
ck_con()

# LIBRARIES ----
# devtools::install_github("DavidPatShuiFong/DTedit@f1617e253d564bce9b2aa67e0662d4cf04d7931f")
# devtools::load_all("/share/github/DTedit")
shelf(
  bbest/DTedit, # [@bbest pr](https://github.com/DavidPatShuiFong/DTedit/pull/35)
  DBI, DT, 
  glue, purrr, readr, tidyr,
  shiny, shinycssloaders, shinyFeedback)
#devtools::load_all("/share/github/DTedit") 
#devtools::install_local("/share/github/DTedit") # value <- ifelse(is.na(value), FALSE, value) # FIXES ERROR if NA: missing value where TRUE/FALSE needed
options(readr.show_col_types = FALSE)

# fs::file_touch(here::here("edit-ba/restart.txt"))

# GPT params
py           <- "/opt/venv/bin/python"   # default python venv from docker container
gpt_py       <- "/share/github/ba/tag_excerpt_gpt.py"
gpt_versions <- c("4","3.5")
#gpt_version <- "3.5"
gpt_version_init <- "4"

# launch with reactlog
#   library(reactlog); reactlog_enable(); app <- runApp(here::here("edit"))
# once app has closed, display reactlog from shiny
#   shiny::reactlogShow()

#options(error = recover)

# initialize tables if missing ----
db_tbls <- dbListTables(con)

# tbl(con, "ba_docs")
if (!"ba_docs" %in% db_tbls){
  # NOTE: this table gets written by update_ba() in update.R
  d_ba_docs <- tbl(con, "ferc_docs") |> 
    collect() |> 
    slice(0) |> 
    rename(
      ba_project  = project,
      ba_doc_file = prj_document,
      ba_doc_url  = prj_doc_attach_url) |> 
    select(-ck_pme)
  # setdiff(d_ba_docs |> colnames(), tbl(con, "ba_docs") |> colnames()) |> paste(collapse = ", ")
  # detail, 
  # prj_doc_attachment,                     # what level is this?
  # ck_ixn, ck_obs, ck_mp, ck_amp, ck_bmps
  # TODO: fix similar fields above in ferc_docs, since these fields should be in separate subtable to ferc_docs, ie ferc_doc_excerpts
}

if (!"ba_doc_excerpts" %in% db_tbls){
  d_ba_doc_excerpts <- tbl(con, "ferc_docs") |> 
    collect() |> 
    slice(0) |> 
    rename(
      ba_doc_file = prj_doc_attach_url,
      excerpt     = detail) |> 
    select(
      ba_doc_file, excerpt, rowid, ck_ixn, ck_obs, ck_mp, ck_amp, ck_bmps)

  dbWriteTable(con, "ba_doc_excerpts", d_ba_doc_excerpts, overwrite = T)

  # TODO: indexes
}

# drop ba_doc_excerpts.ck_* fields if still there
flds_ck <- tbl(con, "ba_doc_excerpts") |> 
  colnames() |> 
  str_subset("ck_")
if (length(flds_ck) > 0){
  sql_drops <- glue("DROP COLUMN {flds_ck}")
  dbExecute(con, glue("ALTER TABLE ba_doc_excerpts {paste(sql_drops, collapse=', ')}"))
}

# tbl(con, "ba_doc_excerpt_tags")
if (!"ba_doc_excerpt_tags" %in% db_tbls){
  d_ba_doc_excerpt_tags <- tbl(con, "ferc_doc_tags") |> 
    collect() |> 
    slice(0) |> 
    select(
      rowid, tag_sql)
  dbWriteTable(con, "ba_doc_excerpt_tags", d_ba_doc_excerpt_tags)
  dbExecute(con, "ALTER TABLE ba_doc_excerpt_tags ALTER COLUMN tag_sql TYPE ltree USING text2ltree(tag_sql);")
  # TODO: indexes
}

# FXNS REFERENCED IN CALLBACKS ----

# convert data from dtedit to ba_docs format  
get_new_excerpts <- function(d) {
  
  flds_excerpts <- colnames(tbl(con, "ba_doc_excerpts"))
  d |> 
    select(all_of(flds_excerpts))
} 

# convert data from dtedit to ba_doc_tags format
get_new_tags <- function(d) {

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
ba_projects <- tbl(con, "ba_projects") |> 
  arrange(ba_project) |> 
  pull(ba_project)

d_ba_docs <- tbl(con, "ba_docs") |> 
  collect()

# skipping doc sections ----
# prj_doc_sec_lookup <- dbReadTable(con, "ferc_project_doc_sec") %>%
#   tibble() %>% collect() %>%
#   filter(!is.na(prj))
# prj_doc_lookup <- prj_doc_sec_lookup %>% 
#   group_by(prj, doc) %>% summarize() %>% ungroup()

# CALLBACK FXNS ----

# INSERT
ba.insert.callback <- function(data, row) {
  ck_con()
  
  # not_list <- function(x){ !is.list(x) }
  d <- data |>
    slice(row) |> 
    # na_if("NA") |> 
    # na_if("") |> 
    # mutate(across(where(not_list), ~na_if(., "NA"))) |> 
    mutate(
      rowid = ifelse(nrow(get_ba_doc_excerpts(gpt_version_init)) == 0, 1, max(get_ba_doc_excerpts(gpt_version_init)$rowid) + 1))
  
  if (d$ck_gpt){
    tmp_txt <- tempfile(fileext=".txt")
    writeLines(d$excerpt, tmp_txt)
    cmd <- glue('{py} "{gpt_py}" "{tmp_txt}" "{d$gpt_version}"')
    message(cmd)
    res <- system(cmd, intern = T)
    unlink(tmp_txt)
    if (!str_detect(res, "Error|Sorry|sorry"))
      d$tag_named[[1]] <- union(str_split(res, ", ")[[1]], d$tag_named[[1]])
  }
  d <- d |> 
    select(-ck_gpt, -gpt_version, -excerpt_html)
  
  # TODO: other elements to add, like ba_docs? handling of NAs?
  
  d_excerpts <- get_new_excerpts(d) # data to INSERT into ba_docs
  d_tags     <- get_new_tags(d)     # data to INSERT into ba_doc_extract_tags
  
  #con <- poolCheckout(con)
  dbAppendTable(con, "ba_doc_excerpts", d_excerpts)
  dbAppendTable(con, "ba_doc_excerpt_tags", d_tags)
  #poolReturn(conn)
  
  get_ba_doc_excerpts(gpt_version_init)
}

# UPDATE
ba.update.callback <- function(data, olddata, row) {
  ck_con()
  
  # browser()
  d <- data |>
    slice(row) |>
    tibble()
  stopifnot(nrow(d) == 1)
  
  if (d$ck_gpt){
    tmp_txt <- tempfile(fileext=".txt")
    writeLines(d$excerpt, tmp_txt)
    cmd <- glue('python "{gpt_py}" "{tmp_txt}" "{d$gpt_version}"')
    message(cmd)
    res <- system(cmd, intern = T)
    unlink(tmp_txt)
    if (!str_detect(res, "Error|Sorry|sorry"))
      d$tag_named[[1]] <- union(str_split(res, ", ")[[1]], d$tag_named[[1]])
  }
  d <- d |> 
    select(-ck_gpt, -gpt_version, -excerpt_html)
  
  d_excerpts <- get_new_excerpts(d) # data to INSERT into ba_docs
  d_tags     <- get_new_tags(d)     # data to INSERT into ba_doc_extract_tags

  #conn <- poolCheckout(con)
  dbExecute(con, glue("DELETE FROM ba_doc_excerpts WHERE rowid = {d$rowid}"))
  dbAppendTable(con, "ba_doc_excerpts", d_excerpts)
  dbExecute(con, glue("DELETE FROM ba_doc_excerpt_tags WHERE rowid = {d$rowid}"))
  dbAppendTable(con, "ba_doc_excerpt_tags", d_tags)
  #poolReturn(conn)
  
  get_ba_doc_excerpts(gpt_version_init)
}

# DELETE
ba.delete.callback <- function(data, row) {
  ck_con()
  
  d <- data |> 
    slice(row) # |> na_if("NA") |> na_if("")
  
  #conn <- poolCheckout(con)
  dbExecute(con, glue("DELETE FROM ba_doc_excerpts WHERE rowid = {d$rowid}"))
  dbExecute(con, glue("DELETE FROM ba_doc_excerpt_tags WHERE rowid = {d$rowid}"))
  #poolReturn(conn)
  
  get_ba_doc_excerpts(gpt_version_init)
}

#* get additional data ----
ba <- get_ba_doc_excerpts(gpt_version_init) # get_ba() # TODO: rename ba -> d_ba_doc_excerpts
tags <- get_tags() 
# ferc_doc_names <- dbReadTable(con, "ferc_docs") %>% names()
# ferc_tag_names <- dbReadTable(con, "ferc_doc_tags") %>% names()
# ba_doc_flds         <- tbl(con, "ba_docs")         %>% colnames()
# ba_doc_excerpt_flds <- tbl(con, "ba_doc_excerpts") %>% colnames()

# * get input choices ----
get_tag_choices <- function(tags){
  tag_choices <- list()
  for (category in unique(tags$category)){ # category = unique(tags$category)[1]
    tag_choices <- append(
      tag_choices,
      setNames(
        list(
          tags %>% 
            filter(category == !!category) %>% 
            pull(tag_named) %>% 
            unlist()),
        category)) 
  }
  tag_choices
}
tag_choices <- get_tag_choices(tags)

# * get labels for dtedit ----
# construct first column: cat(paste(str_pad(glue('"{names(ba)}"'), max(nchar(names(ba))), "right"), collapse = '\n'))
labels <- tribble(
  ~fld                 ,  ~view_label   ,  ~edit_label                  ,  ~delete_label,
  # -------------------|----------------|-------------------------------|----------------
  "ba_doc_file"        , "BA Document"  ,   "BA Document"               ,  "BA Document",
  "rowid"              , "ID"           ,   NA                          ,  NA,
  "excerpt"            , NA             ,   "Excerpt"                   ,  "Excerpt",
  "excerpt_html"       , "Excerpt"      ,   NA                          ,  NA,
  "tag_sql"            , NA             ,   NA                          ,  NA,
  "tag_named"          , NA             ,   "Tags"                      ,  "Tags",
  "tag_html"           , "Tags"         ,   NA                          ,  NA,
  "gpt_version"        , NA             ,   "GPT version"               ,  NA,
  "ck_gpt"             , NA             ,   "Auto Tag (with OpenAI GPT)",  NA)
