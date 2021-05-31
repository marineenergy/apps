get_rowids_with_ixn <- function(db_tbl, ixn){
  # db_tbl = "tethys_mgt_tags"; ixn = c("Receptor.Fish", "Stressor.PhysicalInteraction.Collision")
  
  sql <- glue("SELECT rowid FROM {db_tbl} WHERE tag_sql ~ '{ixn}.*'") %>% 
    paste(collapse = "\nINTERSECT\n")
  DBI::dbGetQuery(con, sql) %>% 
    pull(rowid)
}
