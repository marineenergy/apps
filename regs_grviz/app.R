library(DiagrammeR)
library(shiny)

ui <- fluidPage(
  grVizOutput("dg"),
  verbatimTextOutput("print")
)

server <- function(input, output, session) {
  output$dg <- renderGrViz({
    grViz("
digraph a_nice_graph {

# node definitions with substituted label text
node [fontname = Helvetica]
a [label = 'Is the project grid connected?']
b1 [label = 'FERC Process']
b2 [label = 'Is the project in state waters?']
c1 [label = 'BOEM']
c2 [label = 'Who is leading the project?']
d1 [label = 'State proces']
d2 [label = 'USACE process']


# edge definitions with the node IDs
a -> {b1 b2}
b2 -> {c1 c2}
c2 -> {d1 d2}
}

[1]: 'top'
[2]: 10:20
")
  })
  txt <- reactive({
    req(input$dg_click)
    nodeval <- input$dg_click$nodeValues[[1]]
    return(paste(nodeval, " is clicked"))
  })
  output$print <- renderPrint({
    txt()
  })
}

shinyApp(ui, server)