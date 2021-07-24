library(shiny)
library(DT)

js <- c(
  "function(settings){",
  "  $('[id^=Slider]').ionRangeSlider({",
  "    type: 'double',",
  "    grid: true,",
  "    grid_num: 10,",
  "    min: 0,",
  "    max: 20,",
  "    from: 5,",
  "    to: 15",
  "  });",
  "}"
)

ui <- fluidPage(
  h3("This is how I want the widgets to look in the DT table."),
  fluidRow(column(3, textInput(inputId = "text",
                               label = "TEXT")),
           column(3, selectInput(inputId = "single_select",
                                 label = "SINGLE SELECT",
                                 choices = c("", "A", "B", "C"))),
           column(3, sliderInput(inputId = "slider",
                                 label = "SLIDER",
                                 min = 0,
                                 max = 10,
                                 value = c(0, 10))),
           column(3, selectizeInput(inputId = "multiple_select",
                                    label = "MULTIPLE SELECT",
                                    choices = c("", "A", "B", "C"),
                                    multiple = TRUE))),
  h3("This is how they actually appear in a DT table."),
  fluidRow(DTOutput(outputId = "table"))
)

server <- function(input, output, session) {
  
  output$table <- renderDT({
    data <- data.frame(ROW = 1:5,
                       TEXT = '<input id="text" type="text" class="form-control" value=""/>',
                       SINGLE_SELECT = '<select id="single_select" style="width: 100%;">
                       <option value="" selected></option>
                       <option value="A">A</option>
                       <option value="B">B</option>
                       <option value="C">C</option>
                       </select>',
                       SLIDER = sapply(1:5, function(i) {
                         sprintf('<input type="text" id="Slider%d" name="slider" value="" />', i)
                       }),
                       MULTIPLE_SELECT = '<select id="multiple_select" class="form-control" multiple="multiple">
                       <option value=""></option>
                       <option value="A">A</option>
                       <option value="B">B</option>
                       <option value="C">C</option>
                       </select>',
                       stringsAsFactors = FALSE)
    
    datatable(data = data,
              selection = "none",
              escape = FALSE,
              rownames = FALSE, 
              options = 
                list(
                  initComplete = JS(js)
                ))
  })
  
}

shinyApp(ui = ui, server = server)