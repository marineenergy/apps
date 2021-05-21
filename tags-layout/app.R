## app.R ##
library(librarian)
shelf(
  DT, here, shiny, shinydashboard)

source(here("functions.R"))

df_tags <- tbl(con, "tags") %>% 
  collect() %>% 
  filter(tag != category) %>% 
  mutate(
    tag = map2_chr(tag, category, function(tag, category){
      str_replace(tag, glue("{category}/"), "")}),
    tag_named = map2(tag_sql, tag, setNames))

tag_choices = list()
for (cat in unique(df_tags$category)){ # (cat = df_tags$category[1])
  tag_choices <- append(
    tag_choices,
    setNames(
      list(
        df_tags %>% 
          filter(category == cat) %>% 
          pull(tag_named) %>% 
          unlist()),
      cat))
}

ui <- dashboardPage(
  dashboardHeader(
    title = "MarineEnergy.app",
    titleWidth = 310),
  dashboardSidebar(
    width = 310,
    wellPanel(
      h4("Interactions"),
      selectInput(
        "sel_ixn_tags", "Tags", tag_choices, multiple = T),
      uiOutput("ixn_btns")),
    wellPanel(
      h4("Location"),
      leafletOutput("map_side", height=200, width="100%"),
      actionButton(
        "btn_mod_map", "Modify", icon=icon("gear"), width="50%"))),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
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

server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)
  
  values <- reactiveValues(
    ixns   = list())
  
  # TODO: use API to display htmlwidget results
  # output$frame <- renderUI({
  #     tags$iframe(
  #         src = "https://api.marineenergy.app/highchart?spec", 
  #         width = "100%", height="500", style="border:none;")
  # })
  
  output$ixn_btns <- renderUI({
    btn_add <- div(
      style="display:inline-block",
      actionButton(
        "btn_add_ixn", "Add", icon=icon("plus")), width=6)
    btn_mod <- div(
      style="display:inline-block;float:right",
      actionButton(
        "btn_mod_ixns", "Modify (n=0)", icon=icon("gear")), width=6)    
    if (length(values$ixns) == 0)
      return(btn_add)
    tagList(
      btn_add,
      btn_mod)
  })
  
  observeEvent(input$btn_add_ixn, {
    req(input$sel_ixn_tags)

    values$ixns <- append(values$ixns, list(input$sel_ixn_tags))
    
    updateSelectInput(
      session, 
      "sel_ixn_tags",
      selected = "")
  })
  
  output$map_side <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        attributionControl = F,
        zoomControl = F)) %>% 
      addProviderTiles(providers$Esri.OceanBasemap)
  })
  
  observeEvent(input$btn_mod_ixns, {
    showModal(modalDialog(
      title = "Modify Interactions",
      tagList(
        dataTableOutput("tbl_ixns"),
        actionButton("btn_del_ixns", "Delete selected interaction(s)")),
      easyClose = T))
  })
  
  output$tbl_ixns <- renderDataTable({
    req(values$ixns)
    
    tibble(
      ixns = values$ixns) %>% 
      mutate(
        ixns = map_chr(ixns, paste, collapse = "; "))
  })
  
  observeEvent(input$btn_del_ixns, {
    req(values$ixns, input$tbl_ixns_rows_selected)
    
    values$ixns <- values$ixns[-input$tbl_ixns_rows_selected]
  })
  
  observe({
    n_ixns <- length(values$ixns)
    
    updateActionButton(
      session, 
      "btn_mod_ixns", 
      label = glue("Modify (n={ n_ixns })"))
  })
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
}

shinyApp(ui, server)