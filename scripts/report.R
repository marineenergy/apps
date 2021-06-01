# reports-v2 functions

library(dplyr)

get_tbl_ixn <- function(db_tbl, ixn){
  rowids <- get_rowids_with_ixn(glue("{db_tbl}_tags"), ixn)
  
  tbl(con, db_tbl) %>%
    filter(rowid %in% !!rowids) %>% 
    select(-rowid) %>% 
    collect()
}

rpt_content <- function(cntnt, ixns, rmd){
  # cntnt = contents[1]; ixns = params$interactions
  
  cntnt_tbl <- c(
    # projects   = "projects", 
    management = "tethys_mgt")
  
  if (cntnt %in% names(cntnt_tbl)){
    db_tbl <- cntnt_tbl[[cntnt]]
    
    rmd_ixns <- lapply(
      1:length(ixns), 
      function(i_ixn){ 
        knitr::knit_expand('_interaction.Rmd') })
  } else {
    rmd_ixns <- list("Coming soon...")
  }
  
  list(
    glue("\n# {stringr::str_to_title(cntnt)}\n", .trim = F)) %>% 
    append(rmd_ixns) %>% 
    # knit_child(text = unlist(.), quiet = T) %>% 
    # paste(sep = '\n\n')
    unlist() %>% 
    write(rmd, append = T)
  T
}

# yaml to params for Rmd
yaml2params <- function(yml, frontmatter=F){
  p <- yaml::read_yaml(yml)
  if (frontmatter){
    # directly writing into frontmatter of Rmd requires extra `value` for list objects
    p$contents     <- list(value = p$contents)
    p$interactions <- list(value = p$interactions)
  }
  p
}
