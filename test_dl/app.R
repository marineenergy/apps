ui <- fluidPage(
    downloadButton("downloadData", "Download")
)

server <- function(input, output) {
    # Our dataset
    data <- mtcars
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            # write.csv(mtcars, "/tmp/test.csv")
            file.copy("/tmp/test.csv", file)
        }
    )
    
    #outputOptions(output, "downloadData", suspendWhenHidden=FALSE)
}

shinyApp(ui, server)