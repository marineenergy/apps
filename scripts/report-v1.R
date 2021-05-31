get_aoi_sql <- function(aoi_wkt){
  
  if (is.null(aoi_wkt))
    return(null)
  
  if (length(aoi_wkt) > 1){
    aoi_wkts <- glue("'SRID=4326;{aoi_wkt}'::geometry")
    aoi_sql  <- glue("ST_COLLECT(\n{paste(aoi_wkts, collapse=',\n')})") # Is this recreating the ST_COLLECT statement
    # for every item in <aoi_wkt> array?
  } else {
    # aoi_sql <- glue("'SRID=4326;{aoi_wkt}'")
    aoi_sql <- glue("'SRID=4326;{aoi_wkt}'::geometry")
  }
  aoi_sql
}

html_add <- function(content){
  html_tags <<- tagList(html_tags, content)
}

html_init <- function(){
  html_tags <<- tagList()
}

html_out <- function(){
  html_tags
}

knit_tethys_literature_from_tags <- function(tags){
  
  
  lapply(tags, function(tag) {
    knit_expand('_docs-tethys.Rmd') }) %>% 
    knit_child(text = unlist(.), quiet = T) %>% 
    cat(sep = '\n\n')
}

knit_tethys_literature_from_tags_gen <- function(tags){
  
  lapply(tags, function(tag) {
    knit_expand('_docs-tethys_gen.Rmd') }) %>% 
    knit_child(text = unlist(.), quiet = T) %>% 
    cat(sep = '\n\n')
}

tabulate_dataset_shp_within_aoi <- function(dataset_code, aoi_wkt, output = "kable", debug = T){
  # summarize shapefile dataset from area of interest
  
  # dataset_code = "cetacean-bia"; aoi_wkt = params$aoi_wkt; output = "kable"
  # dataset_code = "cetacean-pacific-summer"; aoi_wkt = params$aoi_wkt; output = "kable"
  
  # dataset_code = "pipelines"; aoi_wkt='POLYGON ((-124.316 35.95711, -119.627 31.65049, -116.5146 35.0183, -120.8413 37.45895, -124.316 35.95711))'
  
  if (debug)
    message(glue("tab..._shp_within_aoi(dataset_code='{dataset_code}', aoi_wkt='{paste(aoi_wkt, collapse=';')}')"))
  
  if (is.null(aoi_wkt))
    return("Please draw a Location to get a summary of the intersecting features for this dataset.")
  
  ds <- tbl(con, "datasets") %>% 
    filter(code == !!dataset_code) %>% 
    replace_na(list(buffer_nm = 0)) %>% 
    collect()
  
  aoi_sql <- get_aoi_sql(aoi_wkt)
  
  # Different set of queries required for data sets that do or
  #   do not need area weighted statistics 
  if (ds$st_intersection){
    # Area weighted statistics ARE required
    ixn_sql <- str_replace({ds$select_sql}, 'geometry', 'geometry, st_intersection(ds.geometry, buf_aoi.geom) as ixn ')
    
    if (!is.na(ds$summarize_sql)){
      x_df <- dbGetQuery(
        con,
        glue("
          with
            buf_aoi as (
              select ST_BUFFER(({aoi_sql})::geography, {ds$buffer_nm} * 1852) as geom),
            tmp_aoi as (
              {ixn_sql} as ds, buf_aoi
              where st_intersects(ds.geometry, buf_aoi.geom))
            {ds$summarize_sql}
          "))
    } else {
      x_sf <- st_read(
        con, 
        glue("
          with
            buf_aoi as (
              select ST_BUFFER(({aoi_sql})::geography, {ds$buffer_nm} * 1852) as geom)
            {ixn_sql} as ds, buf_aoi
            where st_intersects(ds.geometry, buf_aoi.geom)
          "))
      x_df <- st_drop_geometry(x_sf)
      
      if (!is.na(ds$summarize_r))
        eval(parse(text=ds$summarize_r))
    }
    
  } else {
    # Area weighted statistics NOT required
    if (!is.na(ds$summarize_sql)){
      x_df <- dbGetQuery(
        con, glue("
          with 
            buf_aoi as (
              select ST_BUFFER(({aoi_sql})::geography, {ds$buffer_nm} * 1852) as geom ),
            tmp_aoi as (
              {ds$select_sql} as ds
              inner join buf_aoi on st_intersects(ds.geometry, buf_aoi.geom) )
           {ds$summarize_sql}
           "))
    } else {
      x_sf <- st_read(
        con, query = glue("
          with 
            buf_aoi as (
              select ST_BUFFER(({aoi_sql})::geography, {ds$buffer_nm} * 1852) as geom)
            {ds$select_sql} as ds
            inner join buf_aoi on st_intersects(ds.geometry, buf_aoi.geom )
            "))
      x_df <- st_drop_geometry(x_sf)
      
      if (!is.na(ds$summarize_r))
        eval(parse(text=ds$summarize_r))
    }
  }
  if (output == "tibble"){
    return(x_df)
  }
  
  x_spatial <- ifelse(
    ds$buffer_nm == 0,
    glue("\n\nSpatial: within site", .trim = F),
    glue("\n\nSpatial: within {ds$buffer_nm} nautical miles of site", .trim = F))
  
  if (knitr::is_html_output()){
    x_caption <- HTML(markdownToHTML(
      text = glue("Source: [{ds$src_name}]({ds$src_url}){x_spatial}"),
      fragment.only = T))
    
    tbl <- x_df %>% 
      kbl(caption = x_caption) %>%
      kable_styling(
        # full_width = F, position = "left", # position = "float_right"
        bootstrap_options = c("striped", "hover", "condensed", "responsive"))
    
  } else {
    x_caption <- glue("Source: [{ds$src_name}]({ds$src_url}){x_spatial}")
    
    tbl <- x_df %>% 
      kable(caption = x_caption, format = "pipe")
  }
  
  tbl
}

tabulate_dataset_shp_within_aoi_old <- function(dataset_code, aoi_wkt, output = "kable"){
  # summarize shapefile dataset from area of interest, with temporary in-memory query (Common Table Expressions; vs on disk temp tables)
  
  # TODO: pull latest datasets: https://docs.google.com/spreadsheets/d/1MMVqPr39R5gAyZdY2iJIkkIdYqgEBJYQeGqDk1z-RKQ/edit#gid=936111013
  # datasets_gsheet2db(redo = T)
  
  # dataset_code = "cetacean-bia"; aoi_wkt = params$aoi_wkt
  # dataset_code = "efh"; aoi_wkt = "POLYGON ((-67.06819 44.99416, -67.1857 44.94707, -67.21651 44.88058, -67.15834 44.78871, -67.04385 44.81789, -66.91015 44.86279, -67.06819 44.99416))"
  # dataset_code = "cetacean-bia";
  # dataset_code = "cetacean-pacific-summer"; aoi_wkt = "POLYGON ((-67.06819 44.99416, -67.1857 44.94707, -67.21651 44.88058, -67.15834 44.78871, -67.04385 44.81789, -66.91015 44.86279, -67.06819 44.99416))"
  # params <- yaml::yaml.load("
  # title: Testing
  # aoi_wkt:
  # - POLYGON ((-115.9245 32.26236, -115.9245 32.26565, -115.9206 32.26565, -115.9206
  #               32.26236, -115.9245 32.26236))
  # - POLYGON ((-121.9503 33.01519, -121.9503 35.51658, -115.8711 35.51658, -115.8711
  #             33.01519, -121.9503 33.01519))")
  # aoi_wkt <- params$aoi_wkt
  # dataset_code = "oil-gas-wells"
  # aoi_wkt      = "POLYGON ((-157.4273 55.22198, -157.4273 61.76097, -143.1428 61.76097, -143.1428 55.22198, -157.4273 55.22198))"
  # dataset_code='fed-sand-gravel-lease'
  # aoi_wkt='POLYGON ((-175.4932 15.34568, -175.4932 27.93566, -151.813 27.93566, -151.813 15.34568, -175.4932 15.34568))'
  
  # dataset_code='monuments'
  # aoi_wkt='POLYGON ((-180.0668 16.98081, -180.0668 29.87807, -153.4797 29.87807, -153.4797 16.98081, -180.0668 16.98081))'
  
  
  message(glue("tab..._shp_within_aoi(dataset_code='{dataset_code}', aoi_wkt='{paste(aoi_wkt, collapse=';')}')"))
  
  if (is.null(aoi_wkt))
    return("Please draw a Location to get a summary of the intersecting features for this dataset.")
  
  ds <- tbl(con, "datasets") %>% 
    filter(code == !!dataset_code) %>% 
    replace_na(list(buffer_nm = 0)) %>% 
    collect()
  
  if (length(aoi_wkt) > 1){
    aoi_wkts <- glue("'SRID=4326;{aoi_wkt}'::geometry")
    aoi_sql  <- glue("ST_COLLECT(\n{paste(aoi_wkts, collapse=',\n')})") # Is this recreating the ST_COLLECT statement
    # for every item in <aoi_wkt> array?
  } else {
    # aoi_sql <- glue("'SRID=4326;{aoi_wkt}'")
    aoi_sql <- glue("'SRID=4326;{aoi_wkt}'::geometry")
  }
  
  # Use CTE instead of temporary tables
  # TODO
  #    Add conditional to check if ds$summarize_r
  #    Drop geometry column in x_df?
  
  # DEBUG, TODO: turn off this manual override for https://github.com/mhk-env/mhk-env_api/issues/4
  if (dataset_code == "cetacean-pacific-summer")
    ds$st_intersection = F
  sql_intersection <- ifelse(ds$st_intersection,'ST_INTERSECTION','ST_INTERSECTS')
  
  if (!is.na(ds$summarize_sql)){
    x_df <- dbGetQuery(
      con,
      glue("
        with 
          tmp_selarea as (
            select ST_BUFFER({aoi_sql}, {ds$buffer_nm}) as geom ),
          tmp_aoi as (
            {ds$select_sql} as ds
            inner join tmp_selarea on {sql_intersection}(ds.geometry, tmp_selarea.geom) )
         {ds$summarize_sql}
         "))
  } else {
    x_sf <- st_read(
      con, query = glue("
        with 
          tmp_selarea as (
            select ST_BUFFER({aoi_sql}, {ds$buffer_nm} * 1852) as geom)
          {ds$select_sql} as ds
          inner join tmp_selarea on {sql_intersection}(ds.geometry, tmp_selarea.geom )
          "))
    x_df <- st_drop_geometry(x_sf)
    
    if (!is.na(ds$summarize_r))
      eval(parse(text=ds$summarize_r))
  }
  if (output == "tibble"){
    return(x_df)
  }
  
  x_spatial <- ifelse(
    ds$buffer_nm == 0,
    glue("\n\nSpatial: within site", .trim = F),
    glue("\n\nSpatial: within {ds$buffer_nm} nautical miles of site", .trim = F))
  
  if (knitr::is_html_output()){
    x_caption <- HTML(markdownToHTML(
      text = glue("Source: [{ds$src_name}]({ds$src_url}){x_spatial}"),
      fragment.only = T))
    
    tbl <- x_df %>% 
      kbl(caption = x_caption) %>%
      kable_styling(
        # full_width = F, position = "left", # position = "float_right"
        bootstrap_options = c("striped", "hover", "condensed", "responsive"))
    
  } else {
    x_caption <- glue("Source: [{ds$src_name}]({ds$src_url}){x_spatial}")
    
    tbl <- x_df %>% 
      kable(caption = x_caption, format = "pipe")
  }
  
  tbl
}

tabulate_tethys_literature_from_tags <- function(tags){
  
  # tags <- params$stressors[1]
  # tags <- "EMF"
  #
  # tags = c("Marine Mammals AND Noise", "Fish AND EMF")
  # tags <- str_split(tags, " AND ") %>% unlist()
  # tags <- str_split("Birds AND Collision AND Marine Energy (General)", " AND ") %>% unlist()
  
  res <- dbGetQuery(
    con, 
    glue("
    SELECT uri, title, COUNT(tag_text) AS tag_cnt from (
        SELECT 
          uri, 
          data ->'title' ->> 0 AS title,
          json_array_elements(data->'tags') ->> 0 as tag_text
        FROM tethys_pubs) q 
     WHERE tag_text IN ('{paste(tags, collapse = \"','\")}')
     GROUP BY uri, title")) %>% 
    filter(tag_cnt == length(tags))
  
  caption_md <- glue("Literature from [Tethys Knowledge Base](https://tethys.pnnl.gov/knowledge-base-all).")
  
  if (knitr::is_html_output()){
    caption_html <- HTML(markdownToHTML(
      text = caption_md,
      fragment.only = T))
    
    res %>%
      mutate(
        Title = map2_chr(
          title, uri,
          function(x, y)
            glue("<a href={y} target='_blank'>{x}</a>"))) %>%
      select(Title) %>%
      arrange(Title) %>%
      datatable(
        caption = caption_html,
        escape = F)
  } else {
    
    glue("{caption_md}:\n\n", .trim=F) %>% cat()
    
    res %>%
      mutate(
        li_title = glue("1. [{title}]({uri})")) %>%
      pull(li_title) %>% 
      paste(collapse = "\n") %>% 
      cat()
    
    # res %>%
    #   tibble() %>% 
    #   mutate(
    #     Title = glue("[{title}]({uri})")) %>%
    #   select(Title) %>% 
    #   gt() %>%
    #   fmt_markdown(columns = vars(Title)) %>% 
    #   cols_align(align = "left", columns = vars(Title)) %>% 
    #   tab_options(
    #     row.striping.include_table_body = T,
    #     table.width = pct(100)) %>% 
    #   tab_header(
    #     title    = md(caption_md), 
    #     subtitle = md("&nbsp;")) # need blank subtitle for now: https://github.com/rstudio/gt/issues/197)
    # tab_source_note(md(caption_md))
    
    # tbl %>% 
    #   mutate(test = paste0("\\href{http://", link, "}{", test, "}")) %>%
    #   kable("latex", escape = F, booktabs = T) %>%
    #   kable_styling(bootstrap_options = c("hover", "condensed")) 
    # 
    # 
    # res %>%
    #   mutate(
    #     Title = cell_spec(title, "html", link = uri)) %>% 
    #   select(Title) %>% 
    #   kbl(caption = caption_md, escape = F) %>%
    #   kable_styling(
    #     # full_width = F, position = "left", # position = "float_right"
    #     bootstrap_options = c("striped", "hover", "condensed", "responsive"))
  }
}

tabulate_tethys_literature_from_tags_gen <- function(tags){
  # drop Marine Energy since includes all
  tags <- setdiff(tags, "Marine Energy")
  
  s_tags <- glue("'{paste(tags, collapse = \"', '\")}'")
  
  q_tags <- paste(
    glue("
        SELECT uri
        FROM tethys_pub_tags
        WHERE tag IN ({s_tags})"), collapse = "\n UNION \n")
  # cat(q_tags)
  q_pubs <- glue(
    "
      SELECT DISTINCT q.uri, p.title FROM (\n{q_tags}\n) q 
      INNER JOIN (
        SELECT 
          uri,
          data -> 'title' ->> 0  AS title 
        FROM tethys_pubs) p ON q.uri = p.uri
      ORDER BY p.title")
  
  res <- dbGetQuery(con, q_pubs)  %>% 
    tibble() %>% 
    mutate(
      title = str_trim(title))
  
  caption_md <- glue("Literature from [Tethys Knowledge Base](https://tethys.pnnl.gov/knowledge-base-all).")
  
  if (knitr::is_html_output()){
    caption_html <- HTML(markdownToHTML(
      text = caption_md,
      fragment.only = T))
    
    res %>%
      mutate(
        Title = map2_chr(
          title, uri,
          function(x, y)
            glue("<a href={y} target='_blank'>{x}</a>"))) %>%
      select(Title) %>%
      arrange(Title) %>%
      datatable(
        caption = caption_html,
        escape = F)
  } else {
    
    glue("{caption_md}:\n\n", .trim=F) %>% cat()
    
    res %>%
      mutate(
        li_title = glue("1. [{title}]({uri})")) %>%
      pull(li_title) %>% 
      paste(collapse = "\n") %>% 
      cat()
  }
}

tabulate_tethys_literature_from_tags_gen_oldjson <- function(tags){
  
  # tags <- params$stressors[1]
  # tags <- "EMF"
  #
  # tags = c("Marine Mammals AND Noise", "Fish AND EMF")
  # tags <- str_split(tags, " AND ") %>% unlist()
  # tags <- str_split("Birds AND Collision AND Marine Energy", " AND ") %>% unlist()
  # tags <- str_split("Birds AND Collision AND Current", " AND ") %>% unlist()
  # tags <- str_split("Environment AND Human Dimensions AND Current", " AND ") %>% unlist()
  
  # Tech Ideal:
  # tech <- read_csv(here("data/tags.csv")) %>% 
  #   filter(
  #     facet == "technology",
  #     item_label %in% tags) %>% 
  #   pull(item_label)
  
  # Tech Actual:
  # https://tethys.pnnl.gov/knowledge-base-marine-energy
  # Technology ---
  # * Marine Energy (2433)
  #   - Tidal (878)
  #   - Wave (674)
  #   - Riverine (87)
  #   - OTEC (57)
  #   - Ocean Current (32)
  #   - Salinity Gradient (9)
  # * Wind Energy (493)
  #   - Offshore Wind (418)
  #   - Land-Based Wind (9)
  #
  # dbGetQuery(
  #   con,
  #   glue("
  #       SELECT json_array_elements(data->'technologyType') ->> 0 as tech_text
  #       FROM tethys_pubs")) %>%
  #   group_by(tech_text) %>%
  #   summarize(n = n())
  # 
  # 4 Wave                591
  # 1 Current             873 # Tidal
  # 2 OTEC                 57
  # 3 Salinity Gradient     8
  
  message(glue("tab..._tags_gen(): {paste(tags, collapse='|')}"))
  
  # tags = c("Birds", "Marine Energy")
  
  tech_avail <- c("Marine Energy", "OTEC", "Salinity Gradient", "Current", "Wave")
  
  tech_tags <- intersect(tags, tech_avail)
  s_r_tags  <- setdiff(tags, tech_tags)
  
  # drop Marine Energy since includes all, but use above to exclude from s_r_tags
  tech_tags <- setdiff(tech_tags, "Marine Energy")
  
  s_r_res <- dbGetQuery(
    con, 
    glue("
        SELECT uri, title, COUNT(tag_text) AS tag_cnt FROM (
            SELECT 
              uri, 
              data ->'title'                    ->> 0 AS title,
              json_array_elements(data->'tags') ->> 0 as tag_text
            FROM tethys_pubs) q 
         WHERE 
          tag_text IN ('{paste(s_r_tags, collapse = \"','\")}')
         GROUP BY uri, title")) %>% 
    filter(tag_cnt == length(s_r_tags)) %>% 
    select(uri, title) %>% 
    tibble()
  
  if (length(tech_tags) > 0){
    tech_res <- dbGetQuery(
      con, 
      glue("
        SELECT uri, title, COUNT(tag_text) AS tag_cnt FROM (
            SELECT 
              uri, 
              data ->'title'                              ->> 0 AS title,
              json_array_elements(data->'technologyType') ->> 0 as tag_text
            FROM tethys_pubs) q 
         WHERE 
          tag_text IN ('{paste(tech_tags, collapse = \"','\")}')
         GROUP BY uri, title")) %>% 
      filter(tag_cnt == length(tech_tags)) %>% 
      select(uri, title) %>% 
      tibble()
    
    res <- inner_join(
      s_r_res,
      tech_res,
      by = c("uri", "title"))
    
  } else {
    res <- s_r_res
  }
  
  caption_md <- glue("Literature from [Tethys Knowledge Base](https://tethys.pnnl.gov/knowledge-base-all).")
  
  if (knitr::is_html_output()){
    caption_html <- HTML(markdownToHTML(
      text = caption_md,
      fragment.only = T))
    
    res %>%
      mutate(
        Title = map2_chr(
          title, uri,
          function(x, y)
            glue("<a href={y} target='_blank'>{x}</a>"))) %>%
      select(Title) %>%
      arrange(Title) %>%
      datatable(
        caption = caption_html,
        escape = F)
  } else {
    
    glue("{caption_md}:\n\n", .trim=F) %>% cat()
    
    res %>%
      mutate(
        li_title = glue("1. [{title}]({uri})")) %>%
      pull(li_title) %>% 
      paste(collapse = "\n") %>% 
      cat()
    
    # res %>%
    #   tibble() %>% 
    #   mutate(
    #     Title = glue("[{title}]({uri})")) %>%
    #   select(Title) %>% 
    #   gt() %>%
    #   fmt_markdown(columns = vars(Title)) %>% 
    #   cols_align(align = "left", columns = vars(Title)) %>% 
    #   tab_options(
    #     row.striping.include_table_body = T,
    #     table.width = pct(100)) %>% 
    #   tab_header(
    #     title    = md(caption_md), 
    #     subtitle = md("&nbsp;")) # need blank subtitle for now: https://github.com/rstudio/gt/issues/197)
    # tab_source_note(md(caption_md))
    
    # tbl %>% 
    #   mutate(test = paste0("\\href{http://", link, "}{", test, "}")) %>%
    #   kable("latex", escape = F, booktabs = T) %>%
    #   kable_styling(bootstrap_options = c("hover", "condensed")) 
    # 
    # 
    # res %>%
    #   mutate(
    #     Title = cell_spec(title, "html", link = uri)) %>% 
    #   select(Title) %>% 
    #   kbl(caption = caption_md, escape = F) %>%
    #   kable_styling(
    #     # full_width = F, position = "left", # position = "float_right"
    #     bootstrap_options = c("striped", "hover", "condensed", "responsive"))
  }
}
