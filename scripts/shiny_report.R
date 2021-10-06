shelf(DT, glue, tidyr)
# 
# # edit interface ----
# # get_ferc() helper functions
# merge_tags <- function(tag_list_col) {
#   tag_list_col %>% 
#     unlist() %>% 
#     unique() 
# }
# merge_tags_html <- function(tag_list_col) {
#   tag_list_col %>% 
#     unlist() %>% unique() %>% 
#     paste(., collapse = "\n")
# }
# merge_tags_named <- function(tag_list_col) {
#   tag_list_col %>% 
#     unlist(recursive = F) %>% 
#     unlist(recursive = F, use.names = T) %>% 
#     unique() 
# }
# match_prj <- function(
#   prj_doc, prj_names = prjs$prj, alt_prj_names = prjs$prj_alt) {
#   prj_index <- prj_doc %>% 
#     str_detect(
#       coll(
#         prj_names %>%
#           str_replace_all("-", " "),
#         ignore_case = T)) %>% 
#     unlist(recursive = T)
#   if (!(TRUE %in% prj_index)) {
#     prj_index <- list()
#     prj_index <- prj_doc %>% 
#       str_detect(
#         coll(
#           alt_prj_names %>%
#             str_replace_all("-", " "),
#           ignore_case = T)) %>% 
#       unlist(recursive = T)
#   }
#   # get prj name that matches prj
#   if (!(TRUE %in% prj_index)) {
#     prj_index <- NA
#   } else {
#     prj_index <- which(prj_index == TRUE)
#   }
#   if (!(is.na(prj_index))) {
#     prj_name <- prj_names[prj_index]
#   } else {
#     prj_name <- NA
#   }
#   prj_name
# }
# 
# # read in & merge ferc_docs & ferc_doc_tags from db
# get_ferc <- function() {
#   # read in ferc_docs and merge w/ table created by inner join b/w
#   # ferc_doc_tags and tags lookup
#   dbReadTable(con, "ferc_docs") %>% 
#     tibble() %>% collect() %>% 
#     na_if("NA") %>%
#     merge(
#       # read in ferc_doc_tags
#       dbReadTable(con, "ferc_doc_tags") %>% 
#         tibble() %>% collect() %>% 
#         mutate(tag_sql_chr = ifelse(
#           content_tag == "ALL",
#           glue("{tag_category}.{content_tag}") %>% as.character(),
#           tag_sql %>% as.character())) %>% 
#         filter(tag_sql != "NA") %>% 
#         select(-content, -tag_category, -content_tag) %>% 
#         # inner_join() with tags lookup to get tag_html & tag_named
#         inner_join(
#           get_tags() %>% 
#             mutate(
#               tag_html_nocat = glue(
#                 "<span class='me-tag me-{cat}'>{tag_nocat}</span>")) %>% 
#             select(tag_sql, tag_named, tag_html_nocat) %>% 
#             rename(tag_html = tag_html_nocat),
#           by = c("tag_sql_chr" = "tag_sql")) %>% 
#         select(-tag_sql_chr) %>%
#         group_by(rowid) %>% 
#         tidyr::nest(
#           tag_sql   = tag_sql,          # for UPDATING / storage
#           tag_named = tag_named,        # for EDIT INTERFACE
#           tag_html  = tag_html),        # for VIEW dtedit table
#       # merge params
#       by.x = "rowid", by.y = "rowid", all.x = T, incomparables = NA) %>% 
#     mutate(
#       tag_sql   = map(tag_sql,   merge_tags),
#       tag_named = map(tag_named, merge_tags_named),
#       tag_html  = map(tag_html,  merge_tags_html),
#       document  = ifelse(
#         is.na(prj_doc_attachment),
#         prj_document,
#         glue("{prj_document} - {prj_doc_attachment}")),
#       document = ifelse(
#         is.na(prj_doc_attach_url),
#         document,
#         glue('<a href="{prj_doc_attach_url}">{document}</a>')),
#       document = as.character(document),
#       # prj_doc_sec = glue("<h5><b>{project}</b></h5> {prj_document} {ifelse(!is.na(prj_doc_attachment), glue('| <i>{prj_doc_attachment}</i>'), '')}"),
#       prj_doc_sec_display = as.character(glue("<h5><b>{project}</b></h5> {prj_document} {ifelse(!is.na(prj_doc_attachment), glue('<br><i>{prj_doc_attachment}</i>'), '')}")),
#       prj_doc_sec_values = as.character(glue("{project};;{prj_document};;{prj_doc_attachment}"))) %>% 
#     mutate(prj_doc_sec = map2(prj_doc_sec_values, prj_doc_sec_display, setNames)) %>%
#     # select(-prj_doc_sec_values) %>% 
#     # select(-project) %>% 
#     # mutate(project = map_chr(prj_document, match_prj)) %>% 
#     relocate(
#       rowid, document, project, 
#       prj_doc_sec, prj_doc_sec_display, prj_doc_sec_values, 
#       detail, tag_sql, tag_named, tag_html) %>% 
#     arrange(rowid) %>%
#     data.frame()
# }


map_projects <- function(prj_sites){
  leaflet::leaflet(
    data    = prj_sites, width = "100%",
    options = leaflet::leafletOptions(
      zoomControl = F)) %>% 
    leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% 
    leaflet::addMarkers(
      label = ~label_html, 
      popup = ~popup_html) %>%
    htmlwidgets::onRender("function(el, x) {
          L.control.zoom({ position: 'topright' }).addTo(this)
    }")
}

get_tags <- function(){
  tbl(con, "tags") %>% 
    collect() %>% 
    filter(tag != category) %>% 
    mutate(
      tag_sql = as.character(tag_sql),
      tag_named = purrr::map2(tag_sql, tag_nocat, setNames),
      tag_html  = glue("<span class='me-tag me-{cat}'>{tag}</span>")) %>% 
    arrange(desc(category), tag)
}

get_tags_html <- function(rid, tbl_tags = "ferc_doc_tags"){
  # rid = 1; tbl_tags = "ferc_doc_tags"
  tbl(con, tbl_tags) %>% 
    filter(rowid == !!rid, !is.na(tag_sql)) %>% 
    # select(rowid, tag_sql) %>% 
    distinct(rowid, tag_sql) %>% 
    collect() %>% 
    mutate(
      tag_sql = as.character(tag_sql)) %>%
    left_join(
      d_tags %>% 
        select(tag_sql, category, tag, tag_html),
      by = "tag_sql") %>% 
    filter(!is.na(tag)) %>% 
    arrange(desc(category), tag) %>% 
    pull(tag_html) %>% 
    paste(collapse = " ")
}

get_rowids_with_ixn <- function(db_tbl, ixn){
  # db_tbl = "tethys_mgt_tags"; ixn = c("Receptor.Fish", "Stressor.PhysicalInteraction.Collision")
  
  sql <- glue("SELECT rowid FROM {db_tbl} WHERE tag_sql ~ '{ixn}.*'") %>% 
    paste(collapse = "\nINTERSECT\n")
  DBI::dbGetQuery(con, sql) %>% 
    pull(rowid)
}

ixns_to_colorhtml_df <- function(ixns, df_tags){
  # from tag_sql character vector produce data.frame of Interaction with colored HTML tags
  # shiny: tbl_ixns
  # _report: 
  
  if (length(ixns) == 0)
    return(tibble(Interaction = character(0)))
  
  if (class(ixns) == "character"){
    # individual interactions, like headers in report
    d <- tibble(
      tag_sql = ixns) %>% 
      tidyr::unnest(tag_sql) %>% 
      left_join(
        df_tags, by = "tag_sql") %>% 
      # arrange by category inverse alphabetical:
      #   Technology, Stressor, Receptor, Phase, Management
      arrange(desc(category), tag) %>% 
      summarize(
        Interaction = paste(tag_html, collapse = " "), .groups = "drop")
    return(d)
  }
  # multiple interactions, like tbl_ixns in app
  tibble(
    rowid   = 1:length(ixns),
    tag_sql = ixns) %>% 
    tidyr::unnest(tag_sql) %>% 
    left_join(
      df_tags, by = "tag_sql") %>% 
    # arrange by category inverse alphabetical:
    #   Technology, Stressor, Receptor, Phase, Management
    arrange(rowid, desc(category), tag) %>% 
    group_by(rowid) %>% 
    summarize(
      Interaction = paste(tag_html, collapse = " "), .groups = "drop") %>% 
    select(-rowid)
}

ixn_to_colorhtml <- function(ixns, df_tags, is_html = NULL){
  if (is.null(is_html))
    is_html <- knitr::is_html_output()
  if (is_html){
    ixns_to_colorhtml_df(ixns, df_tags) %>% 
      pull(Interaction) %>% 
      paste(collapse = ', ')
  } else {
    ixns
  }
}

load_projects <- function(ixns=NULL){
  # p_csvs <- list.files("/share/github/apps/data", "project_.*")
  # file.copy(file.path("/share/github/apps/data", p_csvs), file.path("/share/github/apps_dev/data", p_csvs), overwrite = T)
  prj_sites_csv        <<- file.path(dir_data, "project_sites.csv")
  prj_times_csv        <<- file.path(dir_data, "project_times.csv")
  prj_permits_csv      <<- file.path(dir_data, "project_permits.csv")
  prj_permit_types_csv <<- file.path(dir_data, "project_permit_types.csv")
  
  # TODO: load prj_*  into db, read from db
  prj_sites <<- readr::read_csv(prj_sites_csv, col_types = readr::cols()) %>% 
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F)
  
  d_times <<- readr::read_csv(prj_times_csv, col_types = readr::cols())  %>% 
    arrange(technology_type, project) %>% 
    mutate(technology_type = factor(technology_type))
  
  d_times <<- d_times %>% 
    mutate(
      # order projects by technology_type, then project
      project = factor(project, levels = d_times$project))
  # levels(d_times$project) # Igiugig,...,Yakutat
  
  d_permits <<- readr::read_csv(prj_permits_csv, col_types = readr::cols()) %>% 
    left_join(
      d_times %>% 
        select(project, technology_type), 
      by = "project") %>% 
    arrange(technology_type, project)
  d_permits <<- d_permits %>% 
    mutate(
      # order projects by technology_type, then project
      project = factor(project, levels = distinct(d_permits, project) %>% pull(project)))
  # levels(d_permits$project) # Igiugig,...,Yakutat
  
  # order permit_types
  permit_types <<- readr::read_csv(prj_permit_types_csv, col_types = readr::cols()) %>% 
    pull(permit_types)
  permit_types <<- permit_types %>% 
    intersect(d_permits$permit_type)
  d_permits <<- d_permits %>% 
    mutate(
      # order permit types by permit_types
      permit_type = permit_type %>% factor(levels = permit_types))

  # # extract technology from interaction tags
  # tags2tech <- c(
  #   "Technology.Riverine" = "Riverine Energy",
  #   "Technology.Tidal"    = "Tidal Energy",
  #   "Technology.Wave"     = "Wave Energy")
  # if (!is.null(ixns)){
  #   # if ixns exist
  #   # tech contains only the tags2tech names that are also in values$ixns
  #   tech <- tags2tech[intersect(names(tags2tech), values$ixns %>% unlist())]
  #   # tech <- tags2tech[1:2]
  # } else {
  #   # else tech is all of tags2tech
  #   tech <- tags2tech
  # }

  # # filter by technology
  # prj_sites <<- prj_sites %>%
  #   filter(technology_type %in% tech)
  # d_times <<- d_times %>%
  #   filter(technology_type %in% tech)
  # d_permits <<- d_permits %>%
  #   filter(technology_type %in% tech)

  prj_sites$label_html <- prj_sites$label_html %>% lapply(htmltools::HTML)
  prj_sites$popup_html <- prj_sites$popup_html %>% lapply(htmltools::HTML)
  
  # colors & symbols
  #project_statuses <<- unique(d_times$project_status)
  project_statuses <<- c("Active Project", "Inactive Project")
  cols_type   <<- colorRampPalette(RColorBrewer::brewer.pal(n=11, name = 'PiYG'))(length(permit_types))
  cols_status <<- c("#30A4E1", "#999999") # Active/Inactive Projects
  cols        <<- setNames(
    c(cols_type, cols_status), 
    c(permit_types, project_statuses))
  symbls_type  <<- c(rep('triangle-up', 3), 'triangle-down', 'triangle-up', 'triangle-down', 'triangle-up', 'triangle-down', rep('triangle-up', 3))
  symbls_status <<- rep(NA, 2)
  symbls <<- setNames(
    c(symbls_type,  symbls_status), 
    c(permit_types, project_statuses))
  
  # technology_type numbers for horizontal line and label placement along y axis
  n_tech <<- d_times %>% 
    group_by(technology_type) %>% 
    summarize(n = n())
  n_riv <<- n_tech %>% filter(technology_type == "Riverine Energy") %>% pull(n)
  n_tid <<- n_tech %>% filter(technology_type == "Tidal Energy")    %>% pull(n)
  n_wav <<- n_tech %>% filter(technology_type == "Wave Energy")     %>% pull(n)
}

# filter projects by selected technology
filter_prj_by_tech <- function(tech, prj_sites, d_times, d_permits) {
  
  prj_sites <<- prj_sites %>% filter(technology_type %in% tech) %>% 
    mutate(
      project = factor(
        project,
        levels = prj_sites %>% sf::st_drop_geometry() %>% distinct(project) %>% pull(project)))
  
  d_times   <<- d_times   %>% filter(technology_type %in% tech) %>% 
    mutate(
      project = factor(
        project, 
        levels = d_times %>% distinct(project) %>% pull(project)))
  
  d_permits <<- d_permits %>% filter(technology_type %in% tech) %>% 
    mutate(
      project = factor(
        project, 
        levels = d_permits %>% distinct(project) %>% pull(project)))
        # levels = d_permits %>% 
        #   filter(technology_type %in% tech) %>% 
        #   distinct(project) %>% 
        #   pull(project)))
}

# calculate y placement of antns and tech lines 
calculate_y_tech <- function(tech) {
  if ("Riverine Energy" %in% tech) {
    n_riv_sel <- n_riv
  } else if (!("Riverine Energy" %in% tech)) {
    n_riv_sel <- as.integer(0)
  }
  if ("Wave Energy" %in% tech) {
    n_wav_sel <- n_wav
  } else if (!("Wave Energy" %in% tech)) {
    n_wav_sel <- as.integer(0)
  }
  if ("Tidal Energy" %in% tech) {
    n_tid_sel <- n_tid
  } else if (!("Tidal Energy" %in% tech)) {
    n_tid_sel <- as.integer(0)
  }
  n_projects <<- n_riv_sel + n_wav_sel + n_tid_sel
  p_riv_sel  <<- n_riv_sel/n_projects
  p_tid_sel  <<- n_tid_sel/n_projects
  p_wav_sel  <<- n_wav_sel/n_projects
  
  p_tech_tbl <<- 
    tibble(
      tech        = c("Riverine Energy", "Tidal Energy", "Wave Energy"),
      p_tech_sel  = c(p_riv_sel,          p_tid_sel,     p_wav_sel),
      n_tech      = c(n_riv,              n_tid,         n_wav)) %>% 
    mutate(
      p_tech_all  = n_tech/(sum(n_tech)),
      name        = stringr::str_replace(tech, " Energy", ""))
}


# plot_projects() helper functions ----
add_prj_sgmts <- function(fig, time_data) {
  fig %>% 
    plotly::add_segments(
      data   = time_data %>% filter(technology_type %in% tech), # %>%
       # TODO: squeez labels by wrapping lines or some such
       # mutate(project = recode(project, `Portsmouth Memorial Bridge`="Portsmouth\n Memorial\n Bridge")),
       x     = ~date_beg,
       xend  = ~date_end,
       y     = ~time_data$project[time_data$technology_type %in% tech],
       yend  = ~time_data$project[time_data$technology_type %in% tech],
       color = ~project_status,
       line  = list(width = 10)) 
}

add_prj_mkrs <- function(fig, permit_data) {
  fig %>% 
    plotly::add_markers(
      data      = permit_data %>% filter(technology_type %in% tech),
      x         = ~license_date, 
      y         = ~permit_data$project[permit_data$technology_type %in% tech],
      symbol    = ~permit_type,
      symbols   = symbls_type,
      color     = ~permit_type,
      colors    = cols_type, 
      size      = 10,
      hoverinfo = "text",
      hovertext = paste(
        '<b>License Date:</b> '    , permit_data$license_date, 
        '<br><b>Project Name:</b> ', permit_data$project, 
        '<br><b>Permit Type:</b> ' , permit_data$permit_type,
        '<br><b>Technology:</b> '  , permit_data$technology_type))
}

lgnd_x_y <- function(fig, time_data) {
  fig %>% 
    plotly::layout(
      xaxis = list(
        #title = 'Date',
        title     = '',
        showline  = FALSE,
        showgrid  = FALSE),
      yaxis = list(
        title     = '',
        autorange = "reversed",
        domain    = c(0, length(unique(time_data$project[time_data$technology_type %in% tech]))),
        range     = c(0,1),
        showline  = FALSE,
        showgrid  = TRUE,
        type = "category",
        text = c(unique(time_data$project)),
        tickfont = list(size = 8)),
      margin = list(
        r = 15,
        t = 25,
        b = 40,
        l = 25,
        pad = list(r = 20, t = 0, b = 0, l = 50)),
      legend = list(
        orientation = 'h',
        font = list(size = 10)))
}


get_antn_info <- function(tech, p_tech_tbl){
  calculate_y_tech(tech)
  ybase      <<- list()
  ybase[[1]] <<- 1.005      
  ybase[[2]] <<- ybase[[1]] - p_tech_tbl$p_tech_sel[1]
  ybase[[3]] <<- ybase[[2]] - p_tech_tbl$p_tech_sel[2]
  
  # antns <<- list()
  
  y_tech_antn <<- list()
  for (i in 1:nrow(p_tech_tbl)) {
    y_tech_antn[[i]] <<- ybase[[i]] - 0.5*(p_tech_tbl$p_tech_sel[i])
  }
    
    
    # antns[[i]] <<- list(
    #   x         = 1,
    #   y         = ybase[[i]] - 0.5*(p_tech_tbl$p_tech_sel[i]),
    #   showarrow = FALSE,
    #   text      = glue("<b>{p_tech_tbl$name[i]}</b>"),
    #   xref      = "paper",
    #   yref      = "paper",
    #   align     = "center",
    #   font      = list(size = 8),
    #   textangle = "90",
    #   yshift    = 8)
  
}

add_tech_text <- function(fig, p_tech_sel, y_tech, tech_name){
  get_antn_info(tech = tech, p_tech_tbl = p_tech_tbl)
  if (p_tech_sel == 0) {
    fig 
  } else {
    fig %>% 
      plotly::layout(
        annotations = list(
          x         = 1,
          y         = y_tech,
          showarrow = FALSE,
          text      = glue("<b>{tech_name}</b>"),
          xref      = "paper",
          yref      = "paper",
          align     = "center",
          font      = list(size = 8),
          textangle = "90",
          yshift    = 8))
  }
}
    
    
    
    
    
    

  

  
  

  
  

#   # tech lines -----
#   n_tech_types  <- p_tech_tbl %>%
#     filter(p_tech_sel != 0) %>%
#     nrow()
# 
#   tech_ln_1 <- add_line(y_tech = top - 1.1*(p_tech_tbl$p_tech_sel[1]))
#   tech_ln_2 <- add_line(y_tech = top - 1.1*(p_tech_tbl$p_tech_sel[2]))
#   # tech_ln_3 <- add_line(y_tech = 1 - p_tech_tbl$p_tech_sel[3])
# 
#   # function ----
#   if (n_tech_types %in% c(0, 1)) {
#     fig %>% plotly::layout(
#       shapes = list(rectangle))
#   } else if (n_tech_types > 1) {
#     fig %>%
#       plotly::layout(
#         shapes = list(
#           rectangle,
#           tech_ln_1,
#           tech_ln_2))
#   }
# }


add_lines <- function(fig, tech){
  
  # rectangle bdr ----
  top       <- 0.975
  bottom    <- 0.025
  left      <- 0
  right     <- 1.5
  
  rectangle <- list(
    type      = "rect", xref = "paper", yref  = "paper",
    fillcolor = "transparent", 
    line      = list(color = "black",   width = 0.8), 
    opacity   = 1,
    x0        = left,
    x1        = right,
    y0        = bottom,
    y1        = top)
  
  # tech lines ----
  tech         <- tech
  n_tech_all   <- c("Riverine Energy" = n_riv, 
                    "Tidal Energy"    = n_tid, 
                    "Wave Energy"     = n_wav)
  n_tech       <<- n_tech_all[tech]
  n_tech_types <<- length(tech)
  
  if (n_tech > 1){
    y_lns <- ((n_tech[-length(n_tech)])) %>% 
      cumsum() %>% 
      as.list()
  }
  
  if (n_tech_types > 1){
    # convert y_lns to list format
    lines <- list()
    for (i in 1:length(y_lns)) {
      # lines[[names(y_lns[i])]] <- add_line(y_tech = y_lns[[i]])
      lines[[i]] <- list(
        type      = "line", xref = "paper", yref  = "y",
        line      = list(color = "black",   width = 0.8), 
        opacity   = 1,
        x0        = 0,
        x1        = 1.5,
        y0        = y_lns[[i]] - (0.5),
        y1        = y_lns[[i]] - (0.5))
    }
    
    for (i in 1:length(lines)) {
      lines_list <- list(rectangle, lines[[1]], lines[[i]])
    }
  }
  # browser()

  # add_lines() function ----
  if (n_tech_types %in% c(0, 1)) {
    fig %>% plotly::layout(
      shapes = list(rectangle))
  } else if (n_tech_types > 1) {
    fig %>% 
        plotly::layout(
          shapes = lines_list)
  }
}

# for initial plotly projects plot
plot_projects <- function(){
  load_projects()
  
  tech <<- c("Riverine Energy", "Tidal Energy", "Wave Energy")
  filter_prj_by_tech(tech, prj_sites, d_times, d_permits)
  calculate_y_tech(tech)
  fig <- plotly::plot_ly(colors = cols, symbols = symbls, height = 700) 
  fig <- fig %>% 
    lgnd_x_y(time_data       = d_times)   %>% 
    add_prj_sgmts(time_data  = d_times)   %>% 
    add_prj_mkrs(permit_data = d_permits) %>% 
    add_lines(tech) %>% 
    add_tech_text(
      p_tech_sel = p_tech_tbl$p_tech_sel[1],
      tech_name  = p_tech_tbl$name[1],
      y_tech     = y_tech_antn[[1]]) %>% 
    add_tech_text(
      p_tech_sel = p_tech_tbl$p_tech_sel[2],
      tech_name  = p_tech_tbl$name[2],
      y_tech     = y_tech_antn[[2]]) %>% 
    add_tech_text(
      p_tech_sel = p_tech_tbl$p_tech_sel[3],
      tech_name  = p_tech_tbl$name[3],
      y_tech     = y_tech_antn[[3]])  
  fig
}

# for plot filtered by tech selection
update_projects <- function(){
  load_projects()
  tech <<- tech
  filter_prj_by_tech(tech, prj_sites, d_times, d_permits)
  calculate_y_tech(tech)
  fig <- plotly::plot_ly(colors = cols, symbols = symbls, height = 700) 
  fig <- fig %>% 
    lgnd_x_y(time_data       = d_times)   %>% 
    add_prj_sgmts(time_data  = d_times)   %>% 
    add_prj_mkrs(permit_data = d_permits) %>% 
    add_lines(tech) %>% 
    add_tech_text(
      p_tech_sel = p_tech_tbl$p_tech_sel[1],
      tech_name  = p_tech_tbl$name[1],
      y_tech     = y_tech_antn[[1]]) %>% 
    add_tech_text(
      p_tech_sel = p_tech_tbl$p_tech_sel[2],
      tech_name  = p_tech_tbl$name[2],
      y_tech     = y_tech_antn[[2]]) %>% 
    add_tech_text(
      p_tech_sel = p_tech_tbl$p_tech_sel[3],
      tech_name  = p_tech_tbl$name[3],
      y_tech     = y_tech_antn[[3]])  
  
  # update plot
  tech <<- tech
  filter_prj_by_tech(tech, prj_sites, d_times, d_permits)
  calculate_y_tech(tech)
  fig <- plotly::plot_ly(colors = cols, symbols = symbls, height = 700) 
  fig <- fig %>% 
    lgnd_x_y(time_data       = d_times)   %>% 
    add_prj_sgmts(time_data  = d_times)   %>% 
    add_prj_mkrs(permit_data = d_permits) %>% 
    add_lines(tech) %>% 
    add_tech_text(
      p_tech_sel = p_tech_tbl$p_tech_sel[1],
      tech_name  = p_tech_tbl$name[1],
      y_tech     = y_tech_antn[[1]]) %>% 
    add_tech_text(
      p_tech_sel = p_tech_tbl$p_tech_sel[2],
      tech_name  = p_tech_tbl$name[2],
      y_tech     = y_tech_antn[[2]]) %>% 
    add_tech_text(
      p_tech_sel = p_tech_tbl$p_tech_sel[3],
      tech_name  = p_tech_tbl$name[3],
      y_tech     = y_tech_antn[[3]])  
  fig
}


tags_sql_to_html <- function(ixns, df_tags){
  
}