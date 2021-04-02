# libraries & functions ----

source(here::here("functions.R"))
shelf(r-spatial/mapview) # https://github.com/r-spatial/mapview/issues/324
mapviewOptions(
  basemaps = c("Stamen.TonerLite", "Esri.OceanBasemap"),
  platform = "leaflet")

dataset_titles <- dbGetQuery(con, "SELECT DISTINCT title FROM dataset_shps ORDER BY title") %>% 
  pull(title)
datasets <- dbGetQuery(
  con, 
  "SELECT title, COUNT(*) AS n_shps FROM dataset_shps 
     GROUP BY title ORDER BY title") %>% 
  mutate(
    title_nshps = glue("{title} [{n_shps}]"))
dataset_choices = with(datasets, setNames(title, title_nshps))

shp_tbls <- dbGetQuery(con, glue(
  "SELECT title, shp_tbl FROM dataset_shps 
     WHERE title = '{dataset_titles[1]}' ORDER BY shp_tbl")) %>% 
  pull(shp_tbl)

# ui.R ----
ui <- fluidPage(
  titlePanel("Dataset Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        "sel_dataset",
        "Dataset [# shapefiles]:",
        dataset_choices),
      selectInput(
        "sel_shp_tbls",
        "Shapefile:",
        shp_tbls),
      textAreaInput(
        "txt_select_sql",
        "SQL to SELECT"),
      textAreaInput(
        "txt_summarize_r",
        "R code to summarize (optional R method)"),
      textAreaInput(
        "txt_summarize_sql",
        "SQL code to customize (optional SQL method)")),
    
    mainPanel(
      leafletOutput("map"),
      dataTableOutput("tbl"))))

# server.R ----
server <- function(input, output, session) {
  
  # reactive: get_shp() ----
  get_shp <- reactive({
    req(input$sel_shp_tbls)
    
    message(glue("input$sel_shp_tbls: {input$sel_shp_tbls}"))
    #input <- list(sel_shp_tbls = "shp_USMaritimeLimitsNBoundaries")
    
    shp <- st_read(con, query = glue('SELECT * FROM "{input$sel_shp_tbls}"'))
  })
  
  # output: map ----
  output$map <- renderLeaflet({
    shp <- get_shp()
    
    m <- mapview(shp)
    m@map
  })
  
  # output: tbl ----
  output$tbl <- renderDataTable({
    shp <- get_shp()
    st_drop_geometry(get_shp())
  })
  
  # observe: sel_dataset ----
  observe({
    dataset_title <- input$sel_dataset
    
    shp_tbls <- dbGetQuery(
      con,
      glue(
        "SELECT title, shp_tbl FROM dataset_shps
                WHERE title = '{dataset_title}' ORDER BY shp_tbl")) %>%
      pull(shp_tbl)
    
    updateSelectInput(
      session,
      "sel_shp_tbls",
      choices = shp_tbls)
  })
  
}

# run app ----
shinyApp(ui = ui, server = server)

# To get working at http://shiny.marinenergy.app/datasets,
#   in rstudio.marinenergy.app Terminal:
#     sudo ln -s /share/github/apps/datasets /srv/shiny-server/datasets
