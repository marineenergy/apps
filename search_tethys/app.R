library(shiny)

# setwd("/share/github/mhk-env_shiny-apps/search_tethys")

source("../functions.R") # connection object

ui <- fluidPage(
    titlePanel("Old Faithful Geyser Data"),
    
    sidebarLayout(
        sidebarPanel(
            textInput(
                "tag",
                "Tag"),
            selectInput(
                "state", "Choose a state:",
                list(
                    `East Coast` = list("NY", "NJ", "CT"),
                    `West Coast` = list("WA", "OR", "CA"),
                    `Midwest` = list("MN", "WI", "IA")),
                multiple = T)),
        
        mainPanel(
            DTOutput("tbl"))))

server <- function(input, output) {

    output$tbl <- renderDT({
        tag <- input$tag
        
        res <- dbGetQuery(
            con, 
            glue("
                SELECT * from (
                    SELECT 
                      uri, 
                      data ->'title' ->> 0 AS title,
                      json_array_elements(data->'tags') ->> 0 as tag_text
                    FROM tethys_pubs) q
                  WHERE q.tag_text = '{tag}';"))
        
        datatable(res) })
}

shinyApp(ui = ui, server = server)
