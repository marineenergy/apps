# ui ----
ui <- dashboardPage(
  title = "MarineEnergy.app",
  
  #* header ----
  dashboardHeader(
    title = HTML("<a class='navbar-brand' href='#'><img alt='Brand' src='./images/logo-horizontal-square.svg' width='40px'></a>MarineEnergy.app"),
    titleWidth = 310,
    #** navbarMenu ----
    leftUi = navbarMenu(
      navbarTab(tabName = "tab_prj", "Projects"),
      navbarTab(tabName = "tab_mgt", "Management"),
      navbarTab(tabName = "tab_rpt", "Reports")),
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
        tabName = "configure",icon = icon("gears"),
        startExpanded = T,
        wellPanel(
          h5(icon("tags"), "Interactions"),
          selectInput(
            "sel_ixn_tags", "Tags", tag_choices, multiple = T),
          uiOutput("ixn_btns")),
        wellPanel(
          h5(icon("map-marked"), "Location"),
          div(
            class="shiny-input-container",
            leafletOutput("map_side", height = 200)),
          actionButton(
            "btn_mod_map", "Add", icon = icon("plus"), width = "263px"))))),
  
  #* body ----
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$link(rel = "shortcut icon", href = "favicon.ico")),
    
    tabItems(
      
      #** tab_prj ----
      tabItem(
        tabName = "tab_prj",
        
        helpText(
          "This page provides an overview of all past and present Marine Energy projects 
          in the United States at different levels of development and provides active links 
          to permitting documents from either the", 
          tags$a(
            "FERC eLibrary", href="https://elibrary.ferc.gov/eLibrary/search", 
            target="_blank"), " or ", 
          tags$a(
            "Tethys Knowledge Base.", 
            href="https://tethys.pnnl.gov/knowledge-base", target="_blank")),
        
        helpText(
          "To learn more about a project, select the blue pin on the map with your 
          cursor and a pop up will open with the project’s name, timeline, geographic 
          coordinates and a list of all submitted permitting and licensing materials to 
          date. Where available, the permitting/licensing documents have been linked to 
          a downloadable PDF of the document or to pages with additional information."),
        
        fluidRow(
          box(
            title = "Map of Projects", width = 12,
            leafletOutput("prj_map"))),
        
        helpText(
          "The figure below shows all past and present Marine Energy projects 
          and the permitting milestones of each over time organized by energy type 
          (riverine, tidal, and wave). Click on the triangles in the plot to zoom the 
          map to the study location of interest. You can access relevant FERC documents 
          per project and permitting milestones by clicking on the study location icon in the map."),
        
        fluidRow(
          box(
            title = "Timeline of Projects", width = 12,
            plotlyOutput("prj_p")))),
        
        #verbatimTextOutput("click")),
    
      #** tab_mgt ----
      tabItem(
        tabName = "tab_mgt",
        helpText(
          HTML("The Management Measures tool allows users to search and query the Tethys 
          Management Measures Tool for Marine Renewable Energy - a robust compilation 
          of marine energy management measures identified by international Marine 
          Renewable Energy regulators and researchers.
          <br>
          Source: <a href='https://tethys.pnnl.gov/management-measures' target='_blank'>
            Management Measures Tool for Marine Renewable Energy | Tethys</a>")),
        fluidRow(
          box(
            title = uiOutput("box_mgt", inline=T), width = 12,
            dataTableOutput("tbl_mgt")))),
      
      #** tab_reports ----
      tabItem(
        tabName = "tab_rpt",
        uiOutput("txt_rpt_login"),
        
        fluidRow(
          box(
            title = "New Report", width = 12,
            column(
              width = 8,
              textInput(
                "txt_rpt_title", "Report Title")),
            column(
              width = 1,
              dropdownButton(
                tags$h3("Configure report"),
                selectInput(
                  inputId = "sel_rpt_ext", 
                  label = "Format", 
                  selected = "html",
                  choices = c(
                    "Portable (*.pdf)" = "pdf",
                    "Word (*.docx)"    = "docx",
                    "Web (*.html)"     = "html")),
                prettyToggle(
                  inputId   = "ck_rpt_prj",
                  value     = T,
                  label_on  = "Projects",  label_off = "Projects",
                  icon_on   = icon("check"), icon_off  = icon("remove")),
                prettyToggle(
                  inputId   = "ck_rpt_mgt",
                  value     = T,
                  label_on  = "Management",  label_off = "Management",
                  icon_on   = icon("check"), icon_off  = icon("remove")),
                circle = T, status = "warning", icon = icon("gear"), width = "20px",
                tooltip = tooltipOptions(title = "Click to configure report"))),
            column(
              width = 3,
              actionButton("btn_rpt_create", "Submit", icon = icon("arrow-circle-right"))))),
        
        fluidRow(
          box(
            title = "Existing Reports", width = 12,
            DTOutput("tbl_reports"))))
            
            
)))