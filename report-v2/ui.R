# ui ----
ui <- dashboardPage(
  title = "MarineEnergy Reporting App",
  #* header ----
  dashboardHeader(
    title = shiny::HTML(
      "<a class='navbar-brand' href='https://marineenergy.app'><img alt='Brand' src='./images/logo-horizontal-square.svg' width='40px'></a>MarineEnergy.app"),
    titleWidth = 310,
    #** navbarMenu ----
    leftUi = navbarMenu(
      navbarTab(tabName = "tab_prj",     "Projects"),
      navbarTab(tabName = "tab_mgt",     "Management"),
      navbarTab(tabName = "tab_docs",    "Documents"),
      # navbarTab(tabName = "tab_ba",      "BioAssessments"),
      navbarTab(tabName = "tab_pubs",    "Publications"),
      navbarTab(tabName = "tab_spatial", "Spatial"),
      navbarTab(tabName = "tab_rpt",     "Reports")),
    #shiny::tags$li(
      # googleSignInUI_btn_signin("login"), class = "dropdown"),
    #shinydashboardPlus::userOutput("user")
    #** google signin ----
    shiny::tags$li(googleSignInUI("login"), class = "dropdown")),

  #* sidebar ----
  dashboardSidebar(
    width = 310,
    #** google login ----
    googleSignInUI_head("login"),
    #** google analytics ----
    shiny::tags$head(includeHTML("www/google-analytics.html")),
    sidebarMenu(
      menuItem(
        "Configure", 
        tabName = "configure", icon = icon("cogs"),
        startExpanded = T,
        wellPanel(
          h5(icon("tags"), "Interactions"),
          selectizeInput(
            "sel_ixn_tags", "Tags", tag_choices,
            multiple = T),
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
        div(
          "Filters by:", icon("tags"), 
          get_content_tag_categories("projects", html=T)),
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
        conditionalPanel(
          condition = "output.msg_prj",
          htmlOutput("msg_prj")),
        conditionalPanel(
          condition = "output.n_prj > 0",
          fluidRow(
            box(
              title = "Map of Projects", width = 6,
              leafletOutput("prj_map"),
              helpText(
                "To learn more about a project, select the blue pin on the map with your 
              cursor and a pop up will open with the project’s name, timeline, geographic 
              coordinates and a list of all submitted permitting and licensing materials to 
              date. Where available, the permitting/licensing documents have been linked to 
              a downloadable PDF of the document or to pages with additional information.")),
            box(
              title = "Timeline of Projects", width = 6,
              plotlyOutput("prj_timeline", height="700px"),
              helpText(
                "The figure above shows all past and present Marine Energy projects 
                  and the permitting milestones of each over time organized by energy type 
                  (riverine, tidal, and wave). Click on the triangles in the plot to zoom the 
                  map to the study location of interest. You can access relevant FERC documents 
                  per project and permitting milestones by clicking on the study location icon in the map."))))),
      
      #** tab_mgt ----
      tabItem(
        tabName = "tab_mgt",
        div(
          "Filters by:", icon("tags"), 
          get_content_tag_categories("management", html=T)),
        helpText(
          HTML("The Management Measures tool allows users to search and query the Tethys 
          Management Measures Tool for Marine Renewable Energy - a robust compilation 
          of marine energy management measures identified by international Marine 
          Renewable Energy regulators and researchers.
          <br>
          Source: <a href='https://tethys.pnnl.gov/management-measures' target='_blank'>
            Management Measures Tool for Marine Renewable Energy | Tethys</a>")),
        conditionalPanel(
          condition = "output.msg_mgt",
          htmlOutput("msg_mgt")),
        fluidRow(
          box(
            title = uiOutput("box_mgt", inline=T), width = 12,
            withSpinner(
              color = "#3C8DBC",
              DT::dataTableOutput("tbl_mgt"))))),
      
      #** tab_docs ----
      tabItem(
        tabName = "tab_docs",
        div(
          "Filters by:", icon("tags"),
          get_content_tag_categories("documents", html=T)),
        helpText(
          HTML("The FERC eLibrary contains environmental compliance project documents, 
          of which excerpts have been manually tagged for reference.")),
        conditionalPanel(
          condition = "output.msg_docs",
          htmlOutput("msg_docs")),
        checkboxGroupInput(
          "cks_docs", 
          "Binary Filters:",
          c(
            "Ixn: Presented as potential interaction?"              = "ck_ixn",
            "Obs: Described from observations at the project site?" = "ck_obs",
            "MP: Monitoring Plan?"                                  = "ck_mp",
            "AMP: Adaptive Management Plan?"                        = "ck_amp",
            "PME: Protection, mitigation, and ehnhancement?"        = "ck_pme",
            "BMP: Best Management Practices applied?"               = "ck_bmps")),
        fluidRow(
          box(
            title = uiOutput("box_docs", inline=T), width = 12,
            withSpinner(
              color = "#3C8DBC",
              dataTableOutput("tbl_docs"))))),
      
      
      #** tab_ba ----
      tabItem(
        tabName = "tab_ba",
        # div(
        #   "Filters by:", icon("tags"),
        #   get_content_tag_categories("documents", html=T)),
        helpText(
          HTML("For a proposed project that is likely to affect species listed
          as endangered or threatened under the Endangered Species Act (ESA) or
          their designated critical habitat, the U.S. Department of Energy (DOE)
          must provide the National Marine Fisheries Service (NMFS) and/or the
          US Fish and Wildlife Service (USFWS) with a Biological Assessment (BA)
          or Biological Evaluation (BE) and seek concurrence that the project is
          unlikely to adversely affect the species or habitat.")),
        # conditionalPanel(
        #   condition = "output.msg_docs",
        #   htmlOutput("msg_docs")),
        tabsetPanel(
          #*** ba_subtab_map ----
          tabPanel(
            "Map of BA Projects",
            leafletOutput("ba_map")),
          #*** ba_subtab_tbl ----
          tabPanel(
            "Table of Excerpts",
            fluidRow(
              box(
                title = uiOutput("box_ba", inline=T), width = 12,
                withSpinner(
                  color = "#3C8DBC",
                  dataTableOutput("tbl_ba"))))) )),
      
      #** tab_pubs ----
      tabItem(
        tabName = "tab_pubs",
        div(
          "Filters by:", icon("tags"), 
          get_content_tag_categories("publications", html=T)),
        helpText(
          "The", a("Tethys Marine Energy Knowledge Base", 
                   href="https://tethys.pnnl.gov/knowledge-base-marine-energy",
                   target="_blank"),
          "contains curated white and gray literature."),
        conditionalPanel(
          condition = "output.msg_pubs",
          htmlOutput("msg_pubs")),
        fluidRow(
          box(
            title = uiOutput("box_pubs", inline=T), width = 12,
            withSpinner(
              color = "#3C8DBC",
              dataTableOutput("tbl_pubs"))))),
      
      #** tab_spatial ----
      tabItem(
        tabName = "tab_spatial",
        div(
          "Filters by:", icon("tags"), 
          get_content_tag_categories("spatial", html=T)),
        helpText(
          "Spatial intersections are displayed here between the location drawn and datasets loaded from", 
          a("MarineCadastre.gov ", 
            href="https://MarineCadastre.gov",
            target="_blank"),
          "of species, habitats and human uses as Receptor tags."),
        conditionalPanel(
          condition = "output.msg_spatial",
          htmlOutput("msg_spatial")),
        fluidRow(
          box(
            title = uiOutput("box_spatial", inline=T), # TODO: figure out initial statement for spatial without Location or Tags
            width = 12,
            withSpinner(
              color = "#3C8DBC",
              dataTableOutput("tbl_spatial"))))),
      
      #** tab_reports ----
      tabItem(
        tabName = "tab_rpt",
        # DEBUG: login off
        conditionalPanel(
          condition = "input['login-g_email'] == null || input['login-g_email'] == ''",
          tagList(
            "In order to generate reports, you'll need to",
            tags$b("Sign in"), "with Google via the button in the upper right. 
            You do not need a Google email, merely an account associated with any email,
            which can be easily created at ", 
            a(href="https://accounts.google.com", "accounts.google.com"), ".")),
        conditionalPanel(
          condition = "input['login-g_email'] != null && input['login-g_email'] != ''",
          fluidRow(
            box(
              title = "New Report", width = 12,
              column(
                width = 8,
                textInput(
                  "txt_rpt_title", "New Report Title")),
              column(
                width = 1,
                dropdownButton(
                  tags$h3("Configure report"),
                  selectInput(
                    inputId = "sel_rpt_ext", 
                    label = "Format", 
                    selected = "html",
                    choices = c(
                      "Web (*.html)"     = "html")),
                      # TODO: get other formats working with report outputs
                      #"Portable (*.pdf)" = "pdf",
                      #"Word (*.docx)"    = "docx")),
                  prettyToggle(
                    inputId   = "ck_rpt_prj",
                    value     = T,
                    label_on  = "Projects",  label_off = "Projects",
                    icon_on   = icon("check"), icon_off  = icon("times")),
                  prettyToggle(
                    inputId   = "ck_rpt_mgt",
                    value     = T,
                    label_on  = "Management",  label_off = "Management",
                    icon_on   = icon("check"), icon_off  = icon("times")),
                  prettyToggle(
                    inputId   = "ck_rpt_docs",
                    value     = T,
                    label_on  = "Documents",  label_off = "Documents",
                    icon_on   = icon("check"), icon_off  = icon("times")),
                  prettyToggle(
                    inputId   = "ck_rpt_pubs",
                    value     = T,
                    label_on  = "Publications",  label_off = "Publications",
                    icon_on   = icon("check"), icon_off  = icon("times")),
                  prettyToggle(
                    inputId   = "ck_rpt_spatial",
                    value     = T,
                    label_on  = "Spatial",  label_off = "Spatial",
                    icon_on   = icon("check"), icon_off  = icon("times")),
                  circle = T, status = "default", icon = icon("cog"), width = "20px",
                  tooltip = tooltipOptions(title = "Click to configure report"))),
              column(
                width = 3,
                actionButton("btn_rpt_create", "Submit", icon = icon("arrow-circle-right"))))),
          
          fluidRow(
            box(
              title = "Existing Reports", width = 12,
              withSpinner(
                color = "#3C8DBC",
                DTOutput("tbl_rpts")),
              actionButton("btn_del_rpts", "Delete selected report(s)", icon=icon("minus")))))
        )
      
      
    )))
