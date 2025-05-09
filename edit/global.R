
# DB CONNECTION & SCRIPTS ----
dir_scripts <<- here::here("scripts")
source(file.path(dir_scripts, "common_2.R"))
source(file.path(dir_scripts, "db.R"))
source(file.path(dir_scripts, "edit_interface.R"))
source(file.path(dir_scripts, "shiny_report.R"))
source(file.path(dir_scripts, "update.R"))

# LIBRARIES ----
# devtools::install_github("DavidPatShuiFong/DTedit@f1617e253d564bce9b2aa67e0662d4cf04d7931f")
# DEBUG
devtools::load_all("/share/github/DTedit")

#https://github.com/rstudio/DT/releases/tag/v0.19
# remotes::install_github("rstudio/DT@v0.19") # vs 0.33

shelf(
  bbest/DTedit, # DavidPatShuiFong/DTedit, # [@bbest pr](https://github.com/DavidPatShuiFong/DTedit/pull/35)
  # DavidPatShuiFong/DTedit, # 4 commits behind jbryer/DTedit
  # jbryer/DTedit,
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

# FXNS REFERENCED IN CALLBACKS ----

# convert data from dtedit to ferc_docs format  
get_new_docs <- function(d, flds = ferc_doc_names) {
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

# convert data from dtedit to ferc_doc_tags format
get_new_tags <- function(d, flds = ferc_tag_names) {
  # tbl(con, "ferc_doc_tags") %>% collect() %>% names() %>% paste(collapse = ', ')

  d %>% 
    select(rowid, tag_named) %>% 
    unnest(tag_named) %>% 
    select(rowid, tag_sql = tag_named) %>% 
    arrange(rowid, tag_sql)
}

# DATA SETUP ----
# TODO: find home for creation of these in db, borrowing from prj_subpages_test.Rmd

# dbListTables(con) %>% str_subset("ferc")
# [1] "ferc"                 "ferc_prj"             "ferc_projects"        "ferc_docs"           
# [5] "ferc_project_doc_sec" "ferc_doc_tags" 
# dbReadTable(con, "ferc_project_doc_sec")

# by prj
prj_sites_lookup <- read_csv(here("data/project_sites.csv")) %>% 
  arrange(project)
# by prj_doc_sec: all prj doc sec data
prj_doc_sec_lookup <- dbReadTable(con, "ferc_project_doc_sec") %>% 
  tibble() %>% collect() %>% 
  filter(!is.na(prj))
# by prj_doc
prj_doc_lookup <- prj_doc_sec_lookup %>% 
  group_by(prj, doc) %>% summarize() %>% ungroup()

# CALLBACK FXNS ----

# INSERT
ferc.insert.callback <- function(data, row) {
  d <- data |> slice(row) |> tibble() |> 
    mutate(across(where(is.character), na_if, "")) |> 
    mutate(across(where(is.character), na_if, "NA")) |> 
    mutate(rowid = max(get_ferc()$rowid) + 1)
  d_docs <- get_new_docs(d) # %>% tibble() # data to INSERT into ferc_docs
  d_tags <- get_new_tags(d) # %>% tibble()  # data to INSERT into ferc_doc_tags
  
  conn <- poolCheckout(con)
  sql_insert_docs <- glue_data_sql(
    d_docs,
    "INSERT INTO ferc_docs VALUES
      ({rowid}, {detail}, {project},
      {prj_document}, {prj_doc_attachment}, {prj_doc_attach_url},
      {ck_ixn}, {ck_obs}, {ck_mp}, {ck_amp}, {ck_pme}, {ck_bmps})",
    .con = conn)
  poolReturn(conn)
  res <- try(dbExecute(con, sql_insert_docs))
  if ("try-error" %in% class(res)) stop(res)
  dbAppendTable(con, "ferc_doc_tags", d_tags)
  # DBI::dbAppendTable(conn, "ferc_doc_tags", d_tags)
  
  get_ferc()
}

# UPDATE
ferc.update.callback <- function(data, olddata, row) {
  d <- data |> slice(row) |>
    tibble() |> 
    mutate(across(where(is.character), na_if, "")) |> 
    mutate(across(where(is.character), na_if, "NA"))
  d_docs <- get_new_docs(d)  # data to UPDATE ferc_docs
  d_tags <- get_new_tags(d)  # data to be APPENDED to ferc_doc_tags
  
  conn <- poolCheckout(con)
  sql_update_docs <- glue_data_sql(
    d_docs,
    "UPDATE ferc_docs 
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
    DELETE FROM ferc_doc_tags WHERE rowid = {d$rowid};")
  
  res <- try(dbExecute(con, sql_update_docs))
  if ("try-error" %in% class(res)) stop(res)
  
  res <- try(dbExecute(con, sql_delete_tags))
  if ("try-error" %in% class(res)) stop(res)
  DBI::dbAppendTable(con, "ferc_doc_tags", d_tags)
  # DBI::dbAppendTable(conn, "ferc_doc_tags", d_tags)
  
  get_ferc()
}

# DELETE
ferc.delete.callback <- function(data, row) {
  d <- data |> 
    slice(row) |> 
    mutate(across(where(is.character), na_if, "")) |> 
    mutate(across(where(is.character), na_if, "NA"))
  
  sql_delete_docs <- glue("DELETE FROM ferc_docs WHERE rowid = {d$rowid};")
  sql_delete_tags <- glue("DELETE FROM ferc_doc_tags WHERE rowid = {d$rowid}")
  
  res <- try(dbExecute(con, sql_delete_docs))
  if ("try-error" %in% class(res)) stop(res)
  
  res <- try(dbExecute(con, sql_delete_tags))
  if ("try-error" %in% class(res)) stop(res)
  
  get_ferc()
}

#* get additional data ----
ferc <- get_ferc() 
tags <- get_tags() 
ferc_doc_names <- dbReadTable(con, "ferc_docs") %>% names()
ferc_tag_names <- dbReadTable(con, "ferc_doc_tags") %>% names()

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
# for (project in unique(get_ferc()$project)){ 
#   prj_doc_sec_choices <- append(
#     prj_doc_sec_choices,
#     setNames(
#       list(
#         ferc %>% 
#           filter(project == !!project) %>% 
#           pull(prj_doc_sec) %>% 
#           unlist()),
#       project)) 
# }


# * get labels (dtedit fld names)
#* get labels for dtedit ----
# construct first column: cat(paste(str_pad(glue('"{names(ferc)}"'), max(nchar(names(ferc))), "right"), collapse = '\n'))
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
#   prj_doc_sec_lookup <- dbReadTable(con, "ferc_project_doc_sec") %>% 
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
