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
  
  cntnts_info <- list(
    projects   = list(
      method     = "child_rmd",
      # TODO: migrate from csv to db
      # db_tbl     = "projects",  
      child_rmd  = "_projects.Rmd"),
    management = list(
      method     = "tbl_per_ixn",
      db_tbl     = "tethys_mgt",
      caption_md = "Literature from [Tethys Knowledge Base](https://tethys.pnnl.gov/knowledge-base-all)."))
  
  info <- cntnts_info[[cntnt]]
  
  if (info$method == "tbl_per_ixn"){
    db_tbl     <- info$db_tbl
    caption_md <- info$db_tbl
    
    # cntnt <- "management"
    rmd_ixns <- with(
      info,
      lapply(
        1:length(ixns), 
        function(i_ixn){ 
          knitr::knit_expand('_interaction.Rmd') }))
    
    list(
      glue("\n# {stringr::str_to_title(cntnt)}\n", .trim = F)) %>% 
      append(rmd_ixns) %>% 
      # knit_child(text = unlist(.), quiet = T) %>% 
      # paste(sep = '\n\n')
      unlist() %>% 
      write(rmd, append = T)
  }
  
  if (info$method == "child_rmd"){
    readLines(info$child_rmd) %>% 
      write(rmd, append = T)
  }
  
  T
}

rpt_tbl <- function(d, cntnt, caption_md=""){
  
  is_df   <- T
  is_html <- knitr::is_html_output()
  
  if (cntnt == "literature" & is_html){
    # tethys_lit
    d <- d %>%
      mutate(
        Title = map2_chr(
          title, uri,
          function(x, y)
            glue("<a href={y} target='_blank'>{x}</a>"))) %>%
      select(Title) %>%
      arrange(Title)
  }
  
  if (cntnt == "literature" & !is_html){
    is_df <- F
    
    d <- d %>%
      mutate(
        li_title = glue("1. [{title}]({uri})")) %>%
      pull(li_title) %>% 
      paste(collapse = "\n")
  }
  
  if (cntnt == "management" & !is_html){
    #tbl(con, "tethys_mgt") %>% collect() %>% names() %>% paste(collapse = '`,\n = `') %>% cat()
    
    # d <- tbl(con, "tethys_mgt") %>% 
    #   collect() %>% 
    #   slice(1:3)
    d <- d %>% 
      mutate(
        Parameters = glue(
          "
          Technology: {Technology}; Category: {`Management Measure Category`}; 
          Phase: {`Phase of Project`}; 
          Stressor: {Stressor}; 
          Receptor: {Receptor} -- {`Specific Receptor`}")) %>% 
      select(
        Parameters, 
        Interaction, 
        Measure = `Specific Management Measures`, 
        Implications = `Implications of Measure`)
    # d %>% 
    #   knitr::kable(format="html", caption=caption_md)
  }
  
  if (is_html){
    caption_html <- htmltools::HTML(markdown::markdownToHTML(
      text = caption_md,
      fragment.only = T))
    
    d %>% 
      DT::datatable(
        caption = caption_html,
        escape = F)
  } else {
    if (is_df){
      #d, caption = caption_md, format = "simple")

      # TODO: get nice table output in docx
      #   https://stackoverflow.com/questions/47704329/how-to-format-kable-table-when-knit-from-rmd-to-word-with-bookdown
      # d %>% 
      #   kableExtra::kbl(booktabs = T, caption = caption_md) %>% 
      #   kableExtra::kable_styling(full_width = T, latex_options = "striped") %>% 
      #   kableExtra::column_spec(1:4, width = rep("1.5in", 4))
      # library(huxtable)
      
      glue("{caption_md}:\n\n", .trim=F) %>% cat()
      
      # install.packages(c("huxtable", "flextable"))
      h <- d %>% 
        huxtable::as_hux() %>%
        # huxtable::set_caption(caption_md) %>% 
        huxtable::theme_basic() %>% 
        huxtable::map_background_color(
          huxtable::by_rows("grey95", "grey80")) %>%
        # set_tb_padding(2)
        # set_width(0.8) %>% 
        # set_font_size(8) %>% 
        # set_lr_padding(2) %>% 
        huxtable::set_col_width(rep(1/ncol(d), ncol(d))) # %>% 
        # set_position("left")
      huxtable::width(h) <- 1
      huxtable::wrap(h) <- TRUE
      h
    } else {
      glue("{caption_md}:\n\n", .trim=F) %>% cat()
      cat(d)
    }
  }
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
