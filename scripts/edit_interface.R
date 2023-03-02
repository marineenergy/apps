shelf(DBI, DT, glue, tidyr)

# get_ba() helper functions ----
merge_tags <- function(tag_list_col) {
  tag_list_col %>% 
    unlist() %>% 
    unique() 
}
merge_tags_html <- function(tag_list_col) {
  tag_list_col %>% 
    unlist() %>% unique() %>% 
    paste(., collapse = "<br>")
}
merge_tags_named <- function(tag_list_col) {
  tag_list_col %>% 
    unlist(recursive = F) %>% 
    unlist(recursive = F, use.names = T) %>% 
    unique() 
}

# read in & merge ba_docs & ba_doc_tags from db ----

get_ferc <- function() {
  # read in ferc_docs and merge w/ table created by inner join b/w
  # ferc_doc_tags and tags lookup
  d_ferc <- tbl(con, "ferc_docs") %>% 
    collect() %>% 
    left_join(
      tbl(con, "ferc_doc_tags") %>% 
        collect() %>% 
        mutate(tag_sql = as.character(tag_sql)) %>%
        inner_join(
          get_tags_nocat() %>%
            select(tag_sql, tag_named, tag_html),
          by = "tag_sql"),
      by = "rowid") %>% 
    arrange(rowid, tag_sql) %>% 
    group_by(rowid) %>% 
    tidyr::nest(
      tag_sql   = tag_sql,         # for UPDATING / storage
      tag_named = tag_named,       # for EDIT INTERFACE
      tag_html  = tag_html) %>%    # for VIEW dtedit table
    mutate(
      tag_sql   = map(tag_sql,   merge_tags),
      tag_named = map(tag_named, merge_tags_named),
      tag_html  = map(tag_html,  merge_tags_html),
      document  = ifelse(
        is.na(prj_doc_attach_url),
        prj_document,
        as.character(glue(
          "<a href='{prj_doc_attach_url}' target='_blank'>{prj_document}</a>"))),
      prj_doc_sec_display = as.character(glue(
        "<h5><b>{project}</b></h5> {prj_document} {ifelse(!is.na(prj_doc_attachment), glue('<br><i>{prj_doc_attachment}</i>'), '')}")),
      prj_doc_sec_values = as.character(glue(
        "{project};;{prj_document};;{prj_doc_attachment}")),
      prj_doc_sec = map2(prj_doc_sec_values, prj_doc_sec_display, setNames)) %>%
    relocate(
      rowid, project, document, prj_doc_attachment,
      prj_doc_sec, prj_doc_sec_display, prj_doc_sec_values, 
      detail, tag_sql, tag_named, tag_html) %>% 
    arrange(project, document, prj_doc_attachment) %>%
    data.frame()
  
  d_ferc 
}


# OLD: get_ba(); NEW: get_ba_docs(), get_ba_doc_excerpts()
get_ba <- function() {
  # read in ba_docs and merge w/ table created by inner join b/w
  # ba_doc_tags and tags lookup
  d_ba <- tbl(con, "ba_doc_excerpt_tags") |> 
    collect() |> 
    mutate(tag_sql = as.character(tag_sql)) |> 
    inner_join(
      get_tags_nocat() |> 
        select(tag_sql, tag_named, tag_html),
      by = "tag_sql") |> 
    left_join(
      tbl(con, "ba_docs") |> 
        left_join(
          tbl(con, "ba_doc_excerpts"),
          by = "ba_doc_file") |> 
        collect(),
      by = "rowid") |> 
    arrange(ba_project, ba_doc_file, rowid, tag_sql) |> 
    group_by(rowid) |> 
    tidyr::nest(
      tag_sql   = tag_sql,         # for UPDATING / storage
      tag_named = tag_named,       # for EDIT INTERFACE
      tag_html  = tag_html) |>     # for VIEW dtedit table
    mutate(
      tag_sql       = map(tag_sql,   merge_tags),
      tag_named     = map(tag_named, merge_tags_named),
      tag_html      = map(tag_html,  merge_tags_html),
      # document -> document_html
      document_html = ifelse(
        is.na(ba_doc_url),
        ba_doc_file,
        glue(
          "<a href='{ba_doc_url}' target='_blank'>{ba_doc_file}</a>") |> 
          as.character()) ) |> 
    relocate(
      ba_project, ba_doc_file, rowid, excerpt, tag_sql, tag_named, tag_html) %>% 
    arrange(ba_project, ba_doc_file) %>%
    data.frame()

  d_ba
}

get_ba_docs <- function() {
  
  d_ba_docs <- tbl(con, "ba_docs") |>
    select(
      ba_project, ba_doc_file, ba_doc_url, prepared_by, institution, date_report, date_test_beg, date_test_end) |> 
    collect() |> 
    mutate(
      ba_doc_html = ifelse(
        is.na(ba_doc_url),
        ba_doc_file,
        glue(
          "<a href='{ba_doc_url}' target='_blank'>{ba_doc_file}</a>") |> 
          as.character())) |> 
    arrange(ba_project, ba_doc_file) |> 
    relocate(
      ba_project, ba_doc_file, ba_doc_url, ba_doc_html) |> 
    data.frame()
  
  d_ba_docs
}

get_ba_doc_excerpts <- function() {
  
  d_ba_doc_excerpts <- tbl(con, "ba_doc_excerpts") |> 
    left_join(
      tbl(con, "ba_docs"),
      by = "ba_doc_file") |> 
    collect() |> 
    left_join(
      tbl(con, "ba_doc_excerpt_tags") |> 
        collect() |> 
        mutate(tag_sql = as.character(tag_sql)) |> 
        inner_join(
          get_tags_nocat() |> 
            select(tag_sql, tag_named, tag_html),
          by = "tag_sql"),
      by = "rowid",
      multiple = "all") |> 
    arrange(ba_project, ba_doc_file, rowid, tag_sql) |> 
    group_by(rowid) |> 
    tidyr::nest(
      tag_sql   = tag_sql,         # for UPDATING / storage
      tag_named = tag_named,       # for EDIT INTERFACE
      tag_html  = tag_html) |>     # for VIEW dtedit table
    mutate(
      tag_sql       = map(tag_sql,   merge_tags),
      tag_named     = map(tag_named, merge_tags_named),
      tag_html      = map(tag_html,  merge_tags_html),
      # document -> document_html
      ba_doc_html = ifelse(
        is.na(ba_doc_url),
        ba_doc_file,
        glue(
          "<a href='{ba_doc_url}' target='_blank'>{ba_doc_file}</a>") |> 
          as.character()) ) |> 
    relocate(
      ba_project, ba_doc_file, ba_doc_url, ba_doc_html, rowid, excerpt, tag_sql, tag_named, tag_html) |> 
    # select(-ba_doc_file, -ba_doc_url) |> 
    arrange(ba_project, ba_doc_file) |> 
    data.frame()
  
  d_ba_doc_excerpts
}


# write project data to db ---- for update.R

update_ferc_prjs <- function(){
  d_prj_doc_sec <- dbReadTable(con, "projects") %>% tibble() %>% collect() %>% 
    select(prj = project) %>% 
    left_join(
      get_ferc() %>% 
        select(
          prj = project, 
          doc = prj_document, 
          sec = prj_doc_attachment, 
          url = prj_doc_attach_url,
          prj_doc_sec_display = prj_doc_sec_display, 
          prj_doc_sec_values = prj_doc_sec_values) %>% 
        group_by(
          prj, doc, sec, url,
          prj_doc_sec_display, prj_doc_sec_values) %>% 
        summarize() %>% 
        ungroup(),
      by = "prj")
  
  
  
  # d_prj_doc_sec <- get_ferc() %>%
  #   select(
  #     prj = project, 
  #     doc = prj_document, 
  #     sec = prj_doc_attachment, 
  #     url = prj_doc_attach_url,
  #     prj_doc_sec_display = prj_doc_sec_display, 
  #     prj_doc_sec_values = prj_doc_sec_values) %>% 
  #     # prj_doc_sec = prj_doc_sec) %>% 
  #   group_by(
  #     prj, doc, sec, url,
  #     prj_doc_sec_display, prj_doc_sec_values) %>% 
  #   # group_by(
  #   #   prj, doc, sec, url,
  #   #   prj_doc_sec_display, prj_doc_sec_values, prj_doc_sec) %>% 
  #   summarize() %>% 
  #   # rowid_to_column("id_prj_doc_sec") %>% 
  #   ungroup() 
  # based on existing prj_doc_sec in ferc_docs
  dbWriteTable(con, "ferc_project_doc_sec", d_prj_doc_sec, overwrite = T)
}

# use conn to preview SQL, but con for st_read() to get spatial geometries
# conn <<- connections::connection_open(
#   RPostgres::Postgres(),
#   dbname   = db_params$dbname,
#   host     = db_params$host,
#   port     = 5432,
#   user     = db_params$user,
#   password = readLines(db_params$pwd_txt))










