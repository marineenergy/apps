# libraries ----
library(librarian)
# shelf(r-lib/ellipsis, RinteRface/shinydashboardPlus, r-lib/vctrs)
shelf(
  DT, googleAuthR, here, mapedit, shiny, shinydashboard, shinyjs, shinyWidgets, sf)
library(shinydashboardPlus) # overwrites shinydashboard

source(here("functions.R"))

# tag_choices ----
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


# googleAuthR ----

# * [Client ID for Web application – APIs & Services – iea-uploader – Google API Console](https://console.developers.google.com/apis/credentials/oauthclient/596429062120-lsgv9f8pbas11o7ub9b9cm0earf1dpu5.apps.googleusercontent.com?authuser=3&project=iea-uploader)
options(googleAuthR.webapp.client_id = "596429062120-lsgv9f8pbas11o7ub9b9cm0earf1dpu5.apps.googleusercontent.com")

googleSignInUI_btn_signin <- function(id, logout_name = "Sign Out", logout_class = "btn-danger"){
  ns <- shiny::NS(id)
  
  shiny::tagList(
    div(id=ns("signin"), class="g-signin2", "data-onsuccess"="onSignIn"))
}

googleSignInUI_btn_signout <- function(id, logout_name = "Sign Out", logout_class = "btn-danger"){
  ns <- shiny::NS(id)
  
  shiny::tagList(
    tags$button(id = ns("signout"), logout_name, onclick = "signOut();", class = logout_class))
}

googleSignInUI_head <- function(id, logout_name = "Sign Out", logout_class = "btn-danger"){
  
  ns <- shiny::NS(id)
  
  tagList(
    tags$head(
      tags$meta(name="google-signin-scope", content="profile email"),
      tags$meta(name="google-signin-client_id", content=getOption("googleAuthR.webapp.client_id")),
      HTML('<script src="https://apis.google.com/js/platform.js?onload=init"></script>')),
    googleAuthR:::load_js_template(
      "js/signin-top.js", 
      ns("signin"), ns("signout") ,ns("g_id"), ns("g_name"), ns("g_image"), ns("g_email")),
    googleAuthR:::load_js_template(
      "js/signin-bottom.js",
      ns("g_id"), ns("g_name"), ns("g_image"), ns("g_email"), ns("signed_in")))
}

## map_edit ----

map_edit <- leaflet(
  options = leafletOptions(
    zoomControl = T,
    attributionControl = F)) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  setView(-93.4, 37.4, 4)

# ui ----
ui <- dashboardPage(
  dashboardHeader(
    title = "MarineEnergy.app",
    titleWidth = 310,
    tags$li(
      class = "dropdown",
      tags$li(
        googleSignInUI_btn_signin("login"), class = "dropdown"),
      userOutput("user"))),
    
  dashboardSidebar(
    width = 310,
    googleSignInUI_head("login"),
    wellPanel(
      h4("Interactions"),
      selectInput(
        "sel_ixn_tags", "Tags", tag_choices, multiple = T),
      uiOutput("ixn_btns")),
    wellPanel(
      h4("Location"),
      div(
        class="shiny-input-container",
        leafletOutput("map_side", height = 200)),
      actionButton(
        "btn_mod_map", "Add", icon = icon("plus"), width = "270px"))),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    # boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )))
)

# server ----
server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)

  values <- reactiveValues(
    ixns   = list())
  
  # login ----
  glogin <- shiny::callModule(googleSignIn, "login")
  
  output$user <- renderUser({
    dashboardUser(
      name     = glogin()$name,
      image    = glogin()$image,
      subtitle = glogin()$email,
      footer   = googleSignInUI_btn_signout("login"))
  })
  
  # map ----
  output$map_side <- renderLeaflet({
    leaflet(
      options = leafletOptions(
        zoomControl        = F,
        attributionControl = F)) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>% 
      setView(-93.4, 37.4, 2)
  })
  
  crud <- callModule(
    editMod, "mapEdit", map_edit, "ply",
    editorOptions = list(
      polylineOptions = F, markerOptions = F, circleMarkerOptions = F,
      singleFeature = T))
  
  observeEvent(input$btn_mod_map, {
    showModal(modalDialog(
      title = "Modify Location",
      editModUI("mapEdit"),
      easyClose = T))
  })
  
  observe({
    ply <- crud()$finished
    
    leafletProxy("map_side") %>%
      clearShapes()
    
    if (is.null(ply)){
      actionButton(
        "btn_mod_map", "Add", icon=icon("plus"))
    } else {
      bb <- st_bbox(ply)
      
      leafletProxy("map_side") %>%
        addPolygons(data = ply) %>% 
        flyToBounds(bb[['xmin']], bb[['ymin']], bb[['xmax']], bb[['ymax']])
      
      updateActionButton(
        session,
        "btn_mod_map", "Modify", icon=icon("gear"))
    }
  })
  
  # ixns ----
  
  output$ixn_btns <- renderUI({
    
    if (length(values$ixns) == 0)
      return(
        actionButton(
          "btn_add_ixn", "Add", icon=icon("plus"), width="270px"))
    
    tagList(
      actionButton(
        "btn_add_ixn" , "Add"         , icon=icon("plus"), width="120px", style="display:inline-block;"),
      actionButton(
        "btn_mod_ixns", "Modify (n=0)", icon=icon("gear"), width="120px", style="display:inline-block;margin-right:15px;float:right"))
  })
  
  observeEvent(input$btn_add_ixn, {
    req(input$sel_ixn_tags)
    
    values$ixns <- append(values$ixns, list(input$sel_ixn_tags))
    
    updateSelectInput(
      session, 
      "sel_ixn_tags",
      selected = "")
  })
  
  observeEvent(input$btn_mod_ixns, {
    showModal(modalDialog(
      title = "Modify Interactions",
      tagList(
        DTOutput("tbl_ixns"),
        actionButton("btn_del_ixns", "Delete selected interaction(s)")),
      easyClose = T))
  })
  
  output$tbl_ixns <- renderDT({
    req(values$ixns)
    
    # TODO: improve table in phases:
    #   1) replace df_tags.sql with prettier shorter df_tags.tag
    #   2) break into columns: technology | stressor | receptor
    tibble(
      Interaction = map_chr(values$ixns, paste, collapse = "; "))
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
  
  # temp plot ----
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
}

shinyApp(ui, server)