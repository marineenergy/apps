get_tags <- function(){
  tbl(con, "tags") %>% 
    collect() %>% 
    filter(tag != category) %>% 
    mutate(
      tag_sql = as.character(tag_sql),
      tag = purrr::map2_chr(tag, category, function(tag, category){
        stringr::str_replace(tag, glue("{category}/"), "")}),
      tag_named = purrr::map2(tag_sql, tag, setNames),
      tag_html  = purrr::map2_chr(tag, category, function(tag, category){
        glue("<span class='me-tag me-{tolower(category)}'>{tag}</span>") })) %>% 
    arrange(category, tag)
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

map_projects <- function(prj_sites){
  leaflet::leaflet(
    data = prj_sites, width = "100%",
    options = leaflet::leafletOptions(
      zoomControl = F)) %>% 
    leaflet::addProviderTiles(leaflet::providers$Esri.OceanBasemap) %>% 
    leaflet::addMarkers(
      label        = ~label_html, 
      popup        = ~popup_html) %>%
    htmlwidgets::onRender("function(el, x) {
          L.control.zoom({ position: 'topright' }).addTo(this)
    }")
}


# plot_projects() helper functions ----
add_prj_sgmts <- function(fig, time_data) {
  fig %>% 
    plotly::add_segments(
      data   = time_data, # %>%
       # TODO: squeez labels by wrapping lines or some such
       # mutate(project = recode(project, `Portsmouth Memorial Bridge`="Portsmouth\n Memorial\n Bridge")),
       x     = ~date_beg,
       xend  = ~date_end,
       y     = ~project,
       yend  = ~project,
       color = ~project_status,
       line  = list(width = 10)) 
}

add_prj_mkrs <- function(fig, permit_data) {
  fig %>% 
    plotly::add_markers(
      data      = permit_data,
      x         = ~license_date, 
      y         = ~project,
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
        domain    = c(0,1),
        range     = c(0, length(unique(time_data$project))),
        showline  = FALSE,
        showgrid  = FALSE,
        type = "category",
        tickfont = list(size = 8)),
      # margin = list(
      #   r = 10, 
      #   t = 25, 
      #   b = 40, 
      #   l = 100),
      legend = list(
        # x = 1.01, 
        # y = 0.5), 
        orientation = 'h',
        font = list(size = 10)))
}

# add line for technology
add_tech_ln <- function(fig, n_y){
  fig %>%
    plotly::layout(
      shapes = list(
        list(
          line = list(color = "black", width = 0.8),
          type = "line",
          x0   = 0,
          x1   = 1,
          xref = "paper",
          y0   = -0.5 + n_y, #Defines horizontal line separating riverine projects from tidal projects
          y1   = -0.5 + n_y,
          yref = "y")))
}

add_tech_text <- function(fig, tech, y){
  #antns <- plotly::layout(fig, "annotations")
  
  # browser()
  # y_pos <- (-1 + y + (n_tech)/2)
  # message("y: {y}")
  
  fig %>% 
    plotly::layout(
      annotations = list(
        # antns,
        # list(
          x         = 1,
          y         = y,
          showarrow = FALSE,
          text      = glue("<b>{tech}</b>"),
          xref      = "paper",
          yref      = "y",
          align     = "center",
          font      = list(size = 8),
          textangle = "90"))
}

add_borders <- function(fig) {
  
  # BOTTOM (x axis)
  bottom_bdr <- list(
    line = list(color = "black",   width = 0.8), 
    type = "line", xref = "paper", yref  = "paper",
    x0   = 0, 
    x1   = 1, 
    y0   = 0.025, 
    y1   = 0.025)
  
  # TOP (x axis)
  top_bdr <- list(
    line = list(color = "black",   width = 0.8), 
    type = "line", xref = "paper", yref  = "paper",
    x0   = 0, 
    x1   = 1, 
    y0   = 0.975, 
    y1   = 0.975)
  
  # LEFT (y axis)
  left_bdr <- list(
    line = list(color = "black",  width = 0.8), 
    type = "line", xref = "paper", yref  = "paper",
    x0   = 0, 
    x1   = 0, 
    y0   = 0.975, 
    y1   = 0.025)
  
  fig %>% plotly::layout(shapes = list(bottom_bdr, top_bdr, left_bdr))
}
        
        # list(
          # line = list(color = "black", width = 0.8), 
          # type = "line", 
          # x0   = 0, 
          # x1   = 1, 
          # xref = "paper", 
          # y0   = 0.025, 
          # y1   = 0.025, 
          # yref = "paper"),
        # 
        # list(
        #   line = list(color = "black", width = 0.8), 
        #   type = "line", 
        #   x0   = 0, 
        #   x1   = 1, 
        #   xref = "paper", 
        #   y0   = 0.975, 
        #   y1   = 0.975, 
        #   yref = "paper"),
        # list(
        #   line = list(color = "black", width = 0.8), 
        #   type = "line", 
        #   x0   = 0, 
        #   x1   = 0, 
        #   xref = "paper", 
        #   y0   = 0.975, 
        #   y1   = 0.025, 
        #   yref = "paper")))

add_antns <- function(fig, n_riv, n_tid, n_wav){
  
  antn_riv <- list(
    x         = 1,
    y         = -1 + n_riv/2,
    showarrow = FALSE,
    text      = "<b>Riverine</b>",
    xref      = "paper",
    yref      = "y",
    align     = "center",
    font      = list(size = 8),
    textangle = "90",
    yshift    = 4)
  
  antn_tid <- list(
    x         = 1,
    y         = (-1 + n_riv + (n_tid)/2),
    showarrow = FALSE,
    text      = "<b>Tidal</b>",
    xref      = "paper",
    yref      = "y",
    align     = "center",
    font      = list(size = 8),
    textangle = "90")
  
  antn_wav <- list(
    x         = 1,
    y         = (-1 + n_riv + n_tid + (n_wav)/2),
    showarrow = FALSE,
    text      = "<b>Wave</b>",
    xref      = "paper",
    yref      = "y",
    align     = "center",
    font      = list(size = 8),
    textangle = "90")
  
  if (all(c("Riverine Energy", "Tidal Energy", "Wave Energy") %in% tech)){
    antns <<- list(antn_riv, antn_tid, antn_wav)
  } else if (all(c("Riverine Energy", "Tidal Energy") %in% tech)){
    antns <<- list(antn_riv, antn_tid)
  } else if (all(c("Riverine Energy", "Wave Energy")  %in% tech)){
    antns <<- list(antn_riv, antn_wav)
  } else if (all(c("Tidal Energy", "Wave Energy")     %in% tech)){
    antns <<- list(antn_tid, antn_wav)
  } else if (tech == "Riverine Energy"){
    antns <<- list(antn_riv)
  } else if (tech == "Tidal Energy"){
    antns <<- list(antn_tid)
  } else if (tech == "Wave Energy"){
    antns <<- list(antn_wav)
  }
  
  fig %>% 
    plotly::layout(annotations = antns)
  
}

plot_projects <- function(){
  fig <- plotly::plot_ly(colors = cols, symbols = symbls, height = 700) 
  
  #browser()
  # table(d_times$technology_type)
  # table(d_permits$technology_type)
  
  fig <- fig %>% 
    add_prj_sgmts(time_data  = d_times)   %>% 
    add_prj_mkrs(permit_data = d_permits) %>% 
    lgnd_x_y(time_data       = d_times)   %>% 
    add_borders() %>% 
    add_tech_text("Tidal", n_tid/2 + 1)
    
    # add_tech_ln(n_riv) %>%
    # add_tech_ln(n_riv + n_tid) %>% 
    # add_antns(n_riv, n_tid, n_wav)
  fig
}



 
# add_antns <- function(n_riv, n_tid, n_wav) {
#   plotly::layout(annotations = antns) 
# }
  #       # riverine
  #       list(
  #         x = 1,
  #         y = (-1 + n_riv)/2,
  #         showarrow = FALSE,
  #         text = "<b>Riverine</b>",
  #         xref = "paper",
  #         yref = "y",
  #         align = "center",
  #         font = list(size = 8),
  #         textangle = "90",
  #         yshift = 4),
  #       # tidal
  #       list(
  #         x = 1,
  #         y = (-1 + n_riv + (n_tid)/2),
  #         showarrow = FALSE,
  #         text = "<b>Tidal</b>",
  #         xref = "paper",
  #         yref = "y",
  #         align = "center",
  #         font = list(size = 8),
  #         textangle = "90"),
  #       # wave
  #       list(
  #         x = 1,
  #         y = (-1 + n_riv + n_tid + (n_wav)/2),
  #         showarrow = FALSE,
  #         text = "<b>Wave</b>",
  #         xref = "paper",
  #         yref = "y",
  #         align = "center",
  #         font = list(size = 8),
  #         textangle = "90")))
  # } else if (tech == c("Riverine Energy", "Wave Energy")) {
  #   plotly::layout(
  #     annotations = list(
  #       list(
  #         x = 1,
  #         y = (-1 + n_riv)/2,
  #         showarrow = FALSE,
  #         text = "<b>Riverine</b>",
  #         xref = "paper",
  #         yref = "y",
  #         align = "center",
  #         font = list(size = 8),
  #         textangle = "90",
  #         yshift = 4),
  #   list(
  #     x = 1,
  #     y = (-1 + n_riv + n_tid + (n_wav)/2),
  #     showarrow = FALSE,
  #     text = "<b>Wave</b>",
  #     xref = "paper",
  #     yref = "y",
  #     align = "center",
  #     font = list(size = 8),
  #     textangle = "90")))
  # } else if (tech == "Riverine Energy") {
  #   plotly::layout(
  #     annotations = list(
  #       list(
  #         x = 1,
  #         y = (-1 + n_riv)/2,
  #         showarrow = FALSE,
  #         text = "<b>Riverine</b>",
  #         xref = "paper",
  #         yref = "y",
  #         align = "center",
  #         font = list(size = 8),
  #         textangle = "90",
  #         yshift = 4)))

  # else if c("Riverine Energy", "Wave Energy") %in% tech) {

# plotly::layout(
#   shapes = list(
#     list(
#       line = list(
#         color = "black", 
#         width = 0.8), 
#       type = "line", 
#       x0 = 0, 
#       x1 = 1, 
#       xref = "paper", 
#       y0 = 0.025, 
#       y1 = 0.025, 
#       yref = "paper"),
#     list(
#       line = list(
#         color = "black", 
#         width = 0.8), 
#       type = "line", 
#       x0 = 0, 
#       x1 = 1, 
#       xref = "paper", 
#       y0 = 0.975, 
#       y1 = 0.975, 
#       yref = "paper"),
#     list(
#       line = list(
#         color = "black", 
#         width = 0.8), 
#       type = "line", 
#       x0 = 0, 
#       x1 = 0, 
#       xref = "paper", 
#       y0 = 0.975, 
#       y1 = 0.025, 
#       yref = "paper")),
  # annotations = list(
  #   list(
  #     x = 1,
  #     y = (-1 + n_riv)/2,
  #     showarrow = FALSE,
  #     text = "<b>Riverine</b>",
  #     xref = "paper",
  #     yref = "y",
  #     align = "center",
  #     font = list(size = 8),
  #     textangle = "90",
  #     yshift = 4),
  #   list(
  #     x = 1,
  #     y = (-1 + n_riv + (n_tid)/2),
  #     showarrow = FALSE,
  #     text = "<b>Tidal</b>",
  #     xref = "paper",
  #     yref = "y",
  #     align = "center",
  #     font = list(size = 8),
  #     textangle = "90"),
  #   list(
  #     x = 1,
  #     y = (-1 + n_riv + n_tid + (n_wav)/2),
  #     showarrow = FALSE,
  #     text = "<b>Wave</b>",
  #     xref = "paper",
  #     yref = "y",
  #     align = "center",
  #     font = list(size = 8),
  #     textangle = "90")))


  # fig <- plotly::plot_ly(colors = cols, symbols = symbls, height = 700) 
  # 
  # fig <- fig %>% 
  #   add_prj_sgmts(time_data  = d_times) %>% 
  #   add_prj_mkrs(permit_data = d_permits) %>% 
  #   lgnd_x_y(time_data       = d_times) 
  # 
  # fig <- fig %>%
  #   add_tech_ln(n_riv) %>%
  #   add_tech_ln(n_riv + n_tid)
  # 
  # fig <- fig %>% 
  #   add_shapes() 
  # # %>% 
  # #   add_antns(n_riv, n_tid, n_wav)
  # # %>% 
  #   # add_antns(n_riv, n_tid, n_wav)
  #   
# fig

  
  # fig <- fig %>% 
  #   add_prj_sgmts(d_times)
  #   plotly::add_segments(
  #     data  = d_times, # %>%
  #       # TODO: squeez labels by wrapping lines or some such
  #       # mutate(project = recode(project, `Portsmouth Memorial Bridge`="Portsmouth\n Memorial\n Bridge")),
  #     x     = ~date_beg,
  #     xend  = ~date_end,
  #     y     = ~project,
  #     yend  = ~project,
  #     color = ~project_status,
  #     line  = list(width = 10))
  # #fig
  #plotly_json(p = fig)
  
  # fig <- fig %>% 
    # plotly::add_markers(
    #   data = d_permits,
    #   x = ~license_date, 
    #   y = ~project,
    #   symbol = ~permit_type,
    #   symbols = symbls_type,
    #   color = ~permit_type,
    #   colors = cols_type, 
    #   size = 10,
    #   hoverinfo = "text",
    #   hovertext = paste(
    #     'License Date: '    , d_permits$license_date, 
    #     '<br>Project Name: ', d_permits$project, 
    #     '<br>Permit Type: ' , d_permits$permit_type))
    # 
  #fig
  #plotly_json(p = fig)
  
  # fig <- fig %>% 
    # plotly::layout(
    #   xaxis = list(
    #     #title = 'Date',
    #     title = '',
    #     showline = FALSE,
    #     showgrid = FALSE),
    #   yaxis = list(
    #     title = '',
    #     autorange = "reversed",
    #     domain = c(0,1),
    #     range = c(0, length(unique(d_times$project))),
    #     showline = FALSE,
    #     showgrid = FALSE,
    #     type = "category",
    #     tickfont = list(size = 8)),
    #   # margin = list(
    #   #   r = 10, 
    #   #   t = 25, 
    #   #   b = 40, 
    #   #   l = 100),
    #   legend = list(
    #     # x = 1.01, 
    #     # y = 0.5), 
    #     orientation = 'h',
    #     font = list(size = 10)))
  
 

  # fig <- fig %>% 
  #   add_tech_ln(n_riv) %>% 
  #   add_tech_ln(n_riv + n_tid)
  
  # fig <- fig %>% 
    # plotly::layout(
    #   shapes = list(
    #     list(
    #       line = list(
    #         color = "black", 
    #         width = 0.8), 
    #       type = "line", 
    #       x0 = 0, 
    #       x1 = 1, 
    #       xref = "paper", 
    #       y0 = 0.025, 
    #       y1 = 0.025, 
    #       yref = "paper"),
    #     list(
    #       line = list(
    #         color = "black", 
    #         width = 0.8), 
    #       type = "line", 
    #       x0 = 0, 
    #       x1 = 1, 
    #       xref = "paper", 
    #       y0 = 0.975, 
    #       y1 = 0.975, 
    #       yref = "paper"),
    #     list(
    #       line = list(
    #         color = "black", 
    #         width = 0.8), 
    #       type = "line", 
    #       x0 = 0, 
    #       x1 = 0, 
    #       xref = "paper", 
    #       y0 = 0.975, 
    #       y1 = 0.025, 
    #       yref = "paper")),
    #   annotations = list(
    #     list(
    #       x = 1,
    #       y = (-1 + n_riv)/2,
    #       showarrow = FALSE,
    #       text = "<b>Riverine</b>",
    #       xref = "paper",
    #       yref = "y",
    #       align = "center",
    #       font = list(size = 8),
    #       textangle = "90",
    #       yshift = 4),
    #     list(
    #       x = 1,
    #       y = (-1 + n_riv + (n_tid)/2),
    #       showarrow = FALSE,
    #       text = "<b>Tidal</b>",
    #       xref = "paper",
    #       yref = "y",
    #       align = "center",
    #       font = list(size = 8),
    #       textangle = "90"),
    #     list(
    #       x = 1,
    #       y = (-1 + n_riv + n_tid + (n_wav)/2),
    #       showarrow = FALSE,
    #       text = "<b>Wave</b>",
    #       xref = "paper",
    #       yref = "y",
    #       align = "center",
    #       font = list(size = 8),
    #       textangle = "90")))
#   
#   fig
#     
# }


tags_sql_to_html <- function(ixns, df_tags){
  
}