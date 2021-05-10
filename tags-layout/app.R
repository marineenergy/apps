## app.R ##
library(DT)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
        selectInput(
            "tags", 
            "Tags",
            c("Stressor/Noise","Stressor/EMF",
              "Receptor/Marine Mammals", "Receptor/Fish",
              "Technology/Wave", "Technology/Tidal",
              "Phase/1.Siting", "Phase/2.Permitting"),
            multiple = T),
        submitButton("+Interaction"),
        DTOutput('tbl_tags'),
        HTML("TODO: [Map]<br>(open to modal<br> for zoom)")
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            uiOutput("frame")),
        fluidRow(
            box(plotOutput("plot1", height = 250)),
            
            box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
            )
        )
    )
)

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$frame <- renderUI({
        tags$iframe(
            src = "https://api.marineenergy.app/highchart?spec", 
            width = "100%", height="500", style="border:none;")
    })
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
    
    output$tbl_tags = renderDT({
        req(input$tags)
        #browser()
        tibble(
            Tags = paste(input$tags, collapse="; "))
    })
}

shinyApp(ui, server)