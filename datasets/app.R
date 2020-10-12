# libraries & functions ----

source(here::here("functions.R"))
shelf(mapview)

dataset_titles <- dbGetQuery(con, "SELECT DISTINCT title FROM dataset_shps ORDER BY title") %>% 
    pull(title)
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
                "Dataset:",
                dataset_titles),
            selectInput(
                "sel_shp_tbls",
                "Shapefile:",
                shp_tbls),
            textAreaInput(
                "txt_customR",
                "R code to customize")),
        
        mainPanel(
            leafletOutput("map"),
            dataTableOutput("tbl"))))

# server.R ----
server <- function(input, output, session) {
    
    # reactive: get_shp() ----
    get_shp <- reactive({
        req(input$sel_shp_tbls)
        
        shp <- st_read(con, query = glue('SELECT * FROM "{input$sel_shp_tbls}"'))
    })

    # output: map ----
    output$map <- renderLeaflet({
        shp <- get_shp()
        
        mapviewOptions(
            basemaps = c("Stamen.TonerLite"), "Esri.OceanBasemap")
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

# To get working at http://shiny.mhk-env.us/datasets,
#   in rstudio.mhk-env.us Terminal:
#     sudo ln -s /share/github/mhk-env_shiny-apps/datasets /srv/shiny-server/datasets
