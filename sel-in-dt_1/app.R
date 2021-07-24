library(data.table)
library(htmltools)
library(shiny)
library(shinydashboard)
library(DT)

dbHeader <- dashboardHeader(title = "")

ui <- fluidPage(
  
  dashboardPage(
    title = "Interface",
    dbHeader,
    dashboardSidebar(
      fluidRow(column(12,dateInput("whichDay", label = h4("Date"), language = "fr", value = NULL))),
      fluidRow(column(12,actionButton("submit","Submit")))
    ),
    dashboardBody(
      dataTableOutput('myTableOutput')
    )
  )
)

server <- function(session, input, output) {
  
  repTable <<- data.table(Blocs=1:3, Véhicules=1:3 )
  
  output$myTableOutput <- DT::renderDataTable(
    {repTable},escape=FALSE,options = list(
      pageLength = 100, info = FALSE, dom="t",
      preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')))
  
  observe({
    vehicles <- vector(mode = "character", length = 0)
    for(i in 1:3){
      vehicles[i] <- as.character(selectInput(inputId=paste0("row_select_", i), label=NULL, choices=c("","a","b")))
    }
    
    ## Add to table
    repTable <<- data.table(Blocs=1:3, Véhicules = vehicles )
    proxy <- dataTableProxy("myTableOutput")
    replaceData(proxy,repTable)
  }
  )
  
  observeEvent(input$submit,{
    ## ???? How to retrieve the values from Véhicules?
    for(i in 1:3) {
      print(input[[paste0("row_select_", i)]])
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)