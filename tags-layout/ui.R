# ui ----
ui <- dashboardPage(
  
  #* header ----
  dashboardHeader(
    title = HTML("<a class='navbar-brand' href='#'><img alt='Brand' src='./images/logo-horizontal-square.svg' width='40px'></a>MarineEnergy.app"),
    titleWidth = 310,
    leftUi = navbarMenu(
      navbarTab(tabName = "tab_prj", "Projects"),
      navbarTab(tabName = "Tab2", "Tab 2")),
    tags$li(
      googleSignInUI_btn_signin("login"), class = "dropdown"),
    userOutput("user")),
  
  #* sidebar ----
  dashboardSidebar(
    width = 310,
    googleSignInUI_head("login"),
    sidebarMenu(
      menuItem(
        "Configure", 
        tabName = "configure",icon = icon("gear"),
        startExpanded = T,
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
            "btn_mod_map", "Add", icon = icon("plus"), width = "263px"))))),
  
  #* body ----
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    
    tabItems(
      
      # tab_prj ----
      tabItem(
        tabName = "tab_prj",
        
        helpText(
          'This page provides an overview of all past and present Marine Energy projects 
          in the United States at different levels of development and provides active links 
          to permitting documents from either the', 
          tags$a(
            "FERC eLibrary", href="https://elibrary.ferc.gov/eLibrary/search", 
            target="_blank"), ' or ', 
          tags$a(
            "Tethys Knowledge Base.", 
            href="https://tethys.pnnl.gov/knowledge-base", target="_blank")),
        
        helpText(
          'To learn more about a project, select the blue pin on the map with your 
          cursor and a pop up will open with the project’s name, timeline, geographic 
          coordinates and a list of all submitted permitting and licensing materials to 
          date. Where available, the permitting/licensing documents have been linked to 
          a downloadable PDF of the document or to pages with additional information.'),
        
        fluidRow(
          box(
            width=12,
            leafletOutput("prj_map"))),
        
        helpText(
          'The figure below shows all past and present Marine Energy projects 
          and the permitting milestones of each over time organized by energy type 
          (riverine, tidal, and wave). Click on the triangles in the plot to zoom the 
          map to the study location of interest. You can access relevant FERC documents 
          per project and permitting milestones by clicking on the study location icon in the map.'),
        
        fluidRow(
          box(
            width=12, height="100%",
            plotlyOutput("prj_p")))),
        
        #verbatimTextOutput("click")),
    
  
      tabItem(
        tabName = "Tab2",
        "Tab 2",
        fluidRow(
          # boxes need to be put in a row (or column)
          box(plotOutput("plot1", height = 250)),
          box(
            title = "Controls",
            sliderInput("slider", "Number of observations:", 1, 100, 50)
          )))))
)