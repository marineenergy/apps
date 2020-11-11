# https://stackoverflow.com/questions/51746041/create-a-grid-with-checkboxes-in-r-shiny
library(shiny)
library(DT)

# create data.frame of row/column names of futre datable
my_df <- data.frame(nam = c("Favorite1", "Favorite2"))

ui = fluidRow(
  sidebarLayout(
    h2("Checkboxes Datatable"),
    DT::dataTableOutput("mytable", width = "1%")),
    mainPanel(
        h2("Selected"),
        tableOutput("checked")))


server = function(input, output) {
            
  # helper function for making checkbox
  shinyInput <- function(FUN, len, id, ...) { 
    inputs <- character(len) 
    for (i in seq_len(len)) { 
      inputs[i] <- as.character(FUN(paste0(id, i), label = NULL, ...)) 
    } 
    inputs 
  } 
  
  # datatable with checkbox
  output$mytable <- DT::renderDataTable( 
    expr = {
      # shinyInput(checkboxInput, nrow(my_df), "cbox1") %>% cat()
        
      df <- data.frame(
        my_df,
        Favorite1 = shinyInput(checkboxInput, nrow(my_df), "cbox1"), 
        Favorite2 = shinyInput(checkboxInput, nrow(my_df), "cbox2"))
      names(df)[1] <- " "
      df}, 
      rownames = FALSE,
      server = FALSE, 
      escape = FALSE, 
      options = list(
        ordering = FALSE,
        searching = FALSE,
        paging = FALSE,
        info = FALSE,
        preDrawCallback = JS("function() { 
          Shiny.unbindAll(this.api().table().node()); }"), 
        drawCallback = JS("function() { 
          Shiny.bindAll(this.api().table().node()); } ")))
  
  # helper function for reading checkbox
  shinyValue <- function(id, len) { 
      unlist(
          x = lapply(
              X = seq_len(len), 
              FUN = function(i) { 
                  value = input[[paste0(id, i)]] 
                  if (is.null(value)) {
                      NA
                  } else {
                      value
                  }  
              }
          )
      ) 
  } 
  # output read checkboxes
  output$checked <- renderTable({
      data.frame(
          Favorite1 = shinyValue("cbox1", nrow(my_df)),
          Favorite2 = shinyValue("cbox2", nrow(my_df))
      )})
}
runApp(list(ui = ui, server = server))
