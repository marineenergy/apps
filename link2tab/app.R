library(shiny)
library(DT)


# Define UI for application that draws a histogram
ui <- navbarPage(
  title = "MarineEnergy.app", id = 'nav',
                 
                 # Application title
                 tabPanel("Plot",
                          plotOutput("distPlot")
                 ),
                 tabPanel("Plot 2",
                          selectInput("species", "Select Species", choices = c("setosa", "virginica"),
                                      multiple = T, selected = NULL),
                          dataTableOutput("tbl1")
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observe({
        query <- parseQueryString(session$clientData$url_search)
        if(!is.null(query$url)) {
            url <- strsplit(query$url,"/")[[1]]
            updateTabsetPanel(session, 'nav', url)
        }
    })
    
    output$distPlot <- renderPlot({
        hist(rnorm(100), col = 'darkgray', border = 'white')
    })
    
    output$tbl1 <- renderDataTable({
        tmp <- iris
        if(!is.null(input$species))
            tmp <- iris[which(iris$Species %in% input$species), ]
        
        datatable(tmp)
    })
}

# Run the application
shinyApp(ui = ui, server = server)