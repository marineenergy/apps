library(shiny)
library(DT)
shinyApp(
  ui = fluidPage(
    DT::dataTableOutput('x1'),
    verbatimTextOutput('x2')
  ),
  
  server = function(input, output, session) {
    # create a character vector of shiny inputs
    shinyInput = function(FUN, len, id, value, ...) {
      if (length(value) == 1) value <- rep(value, len)
      inputs = character(len)
      for (i in seq_len(len)) {
        inputs[i] = as.character(FUN(paste0(id, i), label = NULL, value = value[i]))
      }
      inputs
    }
    
    # obtain the values of inputs
    shinyValue = function(id, len) {
      unlist(lapply(seq_len(len), function(i) {
        value = input[[paste0(id, i)]]
        if (is.null(value)) TRUE else value
      }))
    }
    
    n = 6
    df = data.frame(
      cb = shinyInput(checkboxInput, n, 'cb_', value = TRUE, width='1px'),
      month = month.abb[1:n],
      YN = rep(TRUE, n),
      ID = seq_len(n),
      stringsAsFactors = FALSE)
    
    loopData = reactive({
      df$cb <<- shinyInput(checkboxInput, n, 'cb_', value = shinyValue('cb_', n), width='1px')
      df$YN <<- shinyValue('cb_', n)
      df
    })
    
    output$x1 = DT::renderDataTable(
      isolate(loopData()),
      escape = FALSE, selection = 'none',
      options = list(
        dom = 't', paging = FALSE, ordering = FALSE,
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      ))
    
    proxy = dataTableProxy('x1')
    
    observe({
      replaceData(proxy, loopData(), resetPaging = FALSE)
    })
    
    output$x2 = renderPrint({
      data.frame(Like = shinyValue('cb_', n))
    })
  }
)