# reports-v2 functions

get_tbl_ixn <- function(db_tbl, ixn){
  rowids <- get_rowids_with_ixn(glue("{db_tbl}_tags"), ixn)
  
  tbl(con, db_tbl) %>%
    filter(rowid %in% !!rowids) %>% 
    select(-rowid) %>% 
    collect()
}