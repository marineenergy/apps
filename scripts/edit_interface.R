shelf(DT, glue, tidyr)

# get_ferc() helper functions ----
merge_tags <- function(tag_list_col) {
  tag_list_col %>% 
    unlist() %>% 
    unique() 
}
merge_tags_html <- function(tag_list_col) {
  tag_list_col %>% 
    unlist() %>% unique() %>% 
    paste(., collapse = "\n")
}
merge_tags_named <- function(tag_list_col) {
  tag_list_col %>% 
    unlist(recursive = F) %>% 
    unlist(recursive = F, use.names = T) %>% 
    unique() 
}

# read in & merge ferc_docs & ferc_doc_tags from db ----
get_ferc <- function() {
  # read in ferc_docs and merge w/ table created by inner join b/w
  # ferc_doc_tags and tags lookup
  dbReadTable(con, "ferc_docs") %>% 
    tibble() %>% collect() %>% 
    na_if("NA") %>%
    merge(
      # read in ferc_doc_tags
      dbReadTable(con, "ferc_doc_tags") %>% 
        tibble() %>% collect() %>% 
        mutate(tag_sql_chr = ifelse(
          content_tag == "ALL",
          glue("{tag_category}.{content_tag}") %>% as.character(),
          tag_sql %>% as.character())) %>% 
        filter(tag_sql != "NA") %>% 
        select(-content, -tag_category, -content_tag) %>% 
        # inner_join() with tags lookup to get tag_html & tag_named
        inner_join(
          get_tags() %>% 
            mutate(
              tag_html_nocat = glue(
                "<span class='me-tag me-{cat}'>{tag_nocat}</span>")) %>% 
            select(tag_sql, tag_named, tag_html_nocat) %>% 
            rename(tag_html = tag_html_nocat),
          by = c("tag_sql_chr" = "tag_sql")) %>% 
        select(-tag_sql_chr) %>%
        group_by(rowid) %>% 
        tidyr::nest(
          tag_sql   = tag_sql,          # for UPDATING / storage
          tag_named = tag_named,        # for EDIT INTERFACE
          tag_html  = tag_html),        # for VIEW dtedit table
      # merge params
      by.x = "rowid", by.y = "rowid", all.x = T, incomparables = NA) %>% 
    mutate(
      tag_sql   = map(tag_sql,   merge_tags),
      tag_named = map(tag_named, merge_tags_named),
      tag_html  = map(tag_html,  merge_tags_html),
      document  = ifelse(
        is.na(prj_doc_attachment),
        prj_document,
        glue("{prj_document} - {prj_doc_attachment}")),
      document = ifelse(
        is.na(prj_doc_attach_url),
        document,
        glue('<a href="{prj_doc_attach_url}">{document}</a>')),
      document = as.character(document),
      # prj_doc_sec = glue("<h5><b>{project}</b></h5> {prj_document} {ifelse(!is.na(prj_doc_attachment), glue('| <i>{prj_doc_attachment}</i>'), '')}"),
      prj_doc_sec_display = as.character(glue("<h5><b>{project}</b></h5> {prj_document} {ifelse(!is.na(prj_doc_attachment), glue('<br><i>{prj_doc_attachment}</i>'), '')}")),
      prj_doc_sec_values = as.character(glue("{project};;{prj_document};;{prj_doc_attachment}"))) %>% 
    mutate(prj_doc_sec = map2(prj_doc_sec_values, prj_doc_sec_display, setNames)) %>%
    relocate(
      rowid, document, project, 
      prj_doc_sec, prj_doc_sec_display, prj_doc_sec_values, 
      detail, tag_sql, tag_named, tag_html) %>% 
    arrange(rowid) %>%
    data.frame()
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
conn <<- connections::connection_open(
  RPostgres::Postgres(),
  dbname   = db_params$dbname,
  host     = db_params$host,
  port     = 5432,
  user     = db_params$user,
  password = readLines(db_params$pwd_txt))










