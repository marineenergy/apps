get_rowids_with_ixn <- function(db_tbl, ixn){
  # db_tbl = "tethys_mgt_tags"; ixn = c("Receptor.Fish", "Stressor.PhysicalInteraction.Collision")
  
  sql <- glue("SELECT rowid FROM {db_tbl} WHERE tag_sql ~ '{ixn}.*'") %>% 
    paste(collapse = "\nINTERSECT\n")
  DBI::dbGetQuery(con, sql) %>% 
    pull(rowid)
}

load_projects <- function(){
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
    mutate(
      technology_type = factor(technology_type))
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
      # order by permit_types
      permit_type = factor(permit_type, levels = permit_types))
  
  prj_sites$label_html <<- prj_sites$label_html %>% lapply(htmltools::HTML)
  prj_sites$popup_html <<- prj_sites$popup_html %>% lapply(htmltools::HTML)
  
  # colors & symbols
  project_statuses <<- unique(d_times$project_status)
  cols_type  <<- colorRampPalette(RColorBrewer::brewer.pal(n=11, name = 'PiYG'))(length(permit_types))
  cols_status <<- c("#30A4E1", "#999999") # Active/Inactive Projects
  cols <<- setNames(
    c(cols_type, cols_status), 
    c(permit_types, project_statuses))
  symbls_type  <<- c(rep('triangle-up', 3), 'triangle-down', 'triangle-up', 'triangle-down', 'triangle-up', 'triangle-down', rep('triangle-up', 3))
  symbls_status <<- rep(NA, 2)
  symbls <<- setNames(
    c(symbls_type, symbls_status), 
    c(permit_types, project_statuses))
  
  # technology_type numbers for horizontal line and label placement along y axis
  n_tech <<- d_times %>% 
    group_by(technology_type) %>% 
    summarize(
      n = n())
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

plot_projects <- function(){
  fig <- plotly::plot_ly(colors = cols, symbols = symbls, height = 700)
  
  fig <- fig %>% 
    plotly::add_segments(
      data  = d_times, # %>% 
        # TODO: squeez labels by wrapping lines or some such
        # mutate(project = recode(project, `Portsmouth Memorial Bridge`="Portsmouth\n Memorial\n Bridge")),
      x     = ~date_beg,
      xend  = ~date_end,
      y     = ~project,
      yend  = ~project,
      color = ~project_status,
      line  = list(width = 10))
  #fig
  #plotly_json(p = fig)
  
  fig <- fig %>% 
    plotly::add_markers(
      data = d_permits,
      x = ~license_date, 
      y = ~project,
      symbol = ~permit_type,
      symbols = symbls_type,
      color = ~permit_type,
      colors = cols_type, 
      size = 10,
      hoverinfo = "text",
      hovertext = paste(
        'License Date: '    , d_permits$license_date, 
        '<br>Project Name: ', d_permits$project, 
        '<br>Permit Type: ' , d_permits$permit_type))
  
  #fig
  #plotly_json(p = fig)
  
  fig <- fig %>% 
    plotly::layout(
      xaxis = list(
        #title = 'Date',
        title = '',
        showline = FALSE,
        showgrid = FALSE),
      yaxis = list(
        title = '',
        autorange = "reversed",
        domain = c(0,1),
        range = c(0, length(unique(d_times$project))),
        showline = FALSE,
        showgrid = FALSE,
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
        font = list(size = 10)),
      shapes = list(
        list(
          line = list(
            color = "black", 
            width = 0.8), 
          type = "line", 
          x0 = 0, 
          x1 = 1, 
          xref = "paper", 
          y0 = -0.5 + n_riv, #Defines horizontal line separating riverine projects from tidal projects
          y1 = -0.5 + n_riv, 
          yref = "y"), 
        list(
          line = list(
            color = "black", 
            width = 0.8), 
          type = "line", 
          x0 = 0, 
          x1 = 1, 
          xref = "paper", 
          y0 = -0.5 + n_riv + n_tid, #Defines horizontal line separating tidal projects from wave projects
          y1 = -0.5 + n_riv + n_tid, 
          yref = "y"), 
        list(
          line = list(
            color = "black", 
            width = 0.8), 
          type = "line", 
          x0 = 0, 
          x1 = 1, 
          xref = "paper", 
          y0 = 0.025, 
          y1 = 0.025, 
          yref = "paper"),
        list(
          line = list(
            color = "black", 
            width = 0.8), 
          type = "line", 
          x0 = 0, 
          x1 = 1, 
          xref = "paper", 
          y0 = 0.975, 
          y1 = 0.975, 
          yref = "paper"),
        list(
          line = list(
            color = "black", 
            width = 0.8
          ), 
          type = "line", 
          x0 = 0, 
          x1 = 0, 
          xref = "paper", 
          y0 = 0.975, 
          y1 = 0.025, 
          yref = "paper")),
      annotations = list(
        list(
          x = 1,
          y = (-1 + n_riv)/2,
          showarrow = FALSE,
          text = "<b>Riverine</b>",
          xref = "paper",
          yref = "y",
          align = "center",
          font = list(size = 8),
          textangle = "90",
          yshift = 4),
        list(
          x = 1,
          y = (-1 + n_riv + (n_tid)/2),
          showarrow = FALSE,
          text = "<b>Tidal</b>",
          xref = "paper",
          yref = "y",
          align = "center",
          font = list(size = 8),
          textangle = "90"),
        list(
          x = 1,
          y = (-1 + n_riv + n_tid + (n_wav)/2),
          showarrow = FALSE,
          text = "<b>Wave</b>",
          xref = "paper",
          yref = "y",
          align = "center",
          font = list(size = 8),
          textangle = "90")))
  
  fig
    
}