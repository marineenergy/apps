# ui ----
ui <- dashboardPage(
  title = "MarineEnergy.app",
  
  #* header ----
  dashboardHeader(
    title = HTML("<a class='navbar-brand' href='#'><img alt='Brand' src='./images/logo-horizontal-square.svg' width='40px'></a>MarineEnergy.app"),
    titleWidth = 310,
    #** navbarMenu ----
    leftUi = navbarMenu(
      navbarTab(tabName = "tab_prj",  "Projects"),
      navbarTab(tabName = "tab_mgt",  "Management"),
      navbarTab(tabName = "tab_docs", "Documents"),
      navbarTab(tabName = "tab_pubs", "Publications"),
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
        tabName = "configure", icon = icon("gears"),
        startExpanded = T,
        wellPanel(
          h5(icon("tags"), "Interactions"),
          selectizeInput(
            "sel_ixn_tags", "Tags", tag_choices,
            multiple = T
            # options  = list(render = I('{
            #   option: function(item, escape){
            #     return "<div><strong>" + escape(item) + "</strong>"
            #   }
            # }'))
            ),
            # options  = list(render = I('{
            #   option: function(item, escape){
            #     return "<div><strong>" + escape(item) + "</strong>"
            #   }
            # }'))),
          # selectInput("sel_ixn_tags", "Tags", tag_choices, multiple = T),
            # style = "background-color: red;"),
          uiOutput("ixn_btns")
          # selectInput(
          #   "sel_ixn_tags", "Tags", tag_choices, multiple = T),
          # uiOutput("ixn_btns"),
          
          # tags$label("for"="sel_ixn_tags", "Tags", class="input-label"),
          # tags$select(
          #   id = "sel_ixn_tag", 
          #   # "onfocus"='this.size=13;', "onblur"='this.size=1;' ,
          #   # "onchange"='this.size=1; this.blur();',
          #   tags$option(value = "none", ""),
          #   tags$optgroup("label"="Stressor",
          #                 tags$option(value = "Stressor 1"),
          #                 tags$option(value = "Stressor 2"),
          #                 tags$option(value = "Stressor 3")),
          #   tags$optgroup("label"="Technology",
          #                 tags$option(value = "Tech 1"),
          #                 tags$option(value = "Tech 2"),
          #                 tags$option(value = "Tech 3"))
            # input use as is in the ui or in the server (make sure id is linked in the css)
          
        ),

        # tags$optgroup(tag_choices_html$Stressor),
        # tags$optgroup(tag_choices_html$Receptor),
        # tags$optgroup(tag_choices_html$Phase),
        # tags$optgroup(tag_choices_html$Management),
        # tags$optgroup(tag_choices_html$Consequence)
            
            # HTML(tag_choices_html))), 
   
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
        div("Filters by:", icon("tags"), span(class="me-tag me-technology", "Technology")),
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
            plotlyOutput("prj_p", height="700px"),
            helpText(
              "The figure above shows all past and present Marine Energy projects 
              and the permitting milestones of each over time organized by energy type 
              (riverine, tidal, and wave). Click on the triangles in the plot to zoom the 
              map to the study location of interest. You can access relevant FERC documents 
              per project and permitting milestones by clicking on the study location icon in the map.")))),
      
      #verbatimTextOutput("click")),
      
      #** tab_mgt ----
      tabItem(
        tabName = "tab_mgt",
        div("Filters by:", 
            icon("tags"), 
            span(class="me-tag me-technology", "Technology"),
            span(class="me-tag me-receptor",   "Receptor"),
            span(class="me-tag me-stressor",   "Stressor"),
            span(class="me-tag me-management", "Management"),
            span(class="me-tag me-phase",      "Phase")),
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
            DT::dataTableOutput("tbl_mgt")))),
      
      #** tab_docs ----
      tabItem(
        tabName = "tab_docs",
        div("Filters by:", 
            icon("tags"), 
            span(class="me-tag me-technology", "Technology"),
            # TODO: handle mismatched ferc_docs.tag_sql
            #span(class="me-tag me-management", "Management missing in data"),
            #span(class="me-tag me-effect?", "Effect missing in db.tags"),
            span(class="me-tag me-stressor",   "Stressor"),
            span(class="me-tag me-receptor",   "Receptor"),
            span(class="me-tag me-phase",      "Phase")),
        helpText(
          HTML("The FERC eLibrary contains environmental compliance project documents, 
          of which excerpts have been manually tagged for reference.")),
        checkboxGroupInput(
          "cks_docs", 
          "Binary Filters:",
          c(
            "Ixn: Presented as potential interaction?"              = "ck_ixn",
            "Obs: Described from observations at the project site?" = "ck_obs",
            "MP: Monitoring Plan?"                                  = "ck_mp",
            "AMP: Adaptive Management Plan?"                        = "ck_amp",
            "PME: Protection, mitigation, and ehnhancement?"        = "ck_pme",
            "BMP: Best Management Practices applied?"               = "ck_bmp")),
        fluidRow(
          box(
            title = uiOutput("box_docs", inline=T), width = 12,
            dataTableOutput("tbl_docs")))),
      
      
      #** tab_pubs ----
      tabItem(
        tabName = "tab_pubs",
        div("Filters by:", 
            icon("tags"), 
            span(class="me-tag me-technology", "Technology"),
            span(class="me-tag me-stressor",   "Stressor"),
            span(class="me-tag me-receptor",   "Receptor"),
            span(class="me-tag me-phase",      "Phase")),
        helpText(
          "The", a("Tethys Marine Energy Knowledge Base", 
                   href="https://tethys.pnnl.gov/knowledge-base-marine-energy"),
          "contains curated white and gray literature."),
        fluidRow(
          box(
            title = uiOutput("box_pubs", inline=T), width = 12,
            dataTableOutput("tbl_pubs")))),
      
      #** tab_reports ----
      tabItem(
        tabName = "tab_rpt",
        uiOutput("txt_rpt_login"),
        conditionalPanel(
          condition = "input['login-g_email'] != null && input['login-g_email'] != ''",
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
                      "Web (*.html)"     = "html",
                      "Portable (*.pdf)" = "pdf",
                      "Word (*.docx)"    = "docx")),
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
                  circle = T, status = "default", icon = icon("gear"), width = "20px",
                  tooltip = tooltipOptions(title = "Click to configure report"))),
              column(
                width = 3,
                actionButton("btn_rpt_create", "Submit", icon = icon("arrow-circle-right"))))),
          
          fluidRow(
            box(
              title = "Existing Reports", width = 12,
              DTOutput("tbl_rpts"),
              actionButton("btn_del_rpts", "Delete selected report(s)", icon=icon("minus"))))))
      
      
    )))