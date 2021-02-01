tagList(
  tags$head(
    includeHTML("www/components/head.html")),
  introjsUI(),
  useShinyjs(),
  use_waiter(),
  # TODO: place Tour bttn
  # actionButton("tour", "Tour the app", icon = icon("question-circle")),
  
  navbarPage(
    windowTitle = "MarineEnergy.app",
    title = a(
      img(src="images/logo-horizontal.svg",       class="default"),
      img(src="images/logo-horizontal-white.svg", class="white"),
      href  = web_url
    ),
    id = 'nav',
    fluid = 'true',
    theme = 'styles.css',
    collapsible = 'true',
    selected = "Configure",
    
    tabPanel(
      a("Projects", href = glue("{web_url}/projects.html"))),
      
    tabPanel(
      "Configure",
      tabsetPanel(
        id = "tabConfigure",
        
        # introBox(),
        # data.step  = 3,
        # data.intro = "Select."),
        tabPanel(
          "Location",
          fluidRow(
            box(
              width = 12,
              introBox(
                editModUI("mapEdit"),      
                data.step  = 1,
                data.intro = "
            Enter Location by selecting a drawing tool and clicking on the map.
            For polygons, click on successive points and be sure to finish at the first point.
            For boxes, click and hold for one corner, drag and release to other.
            Before drawing, you can also zoom in/out by double clicking, and pan by click and dragging.")))),
        
        tabPanel(
          "Tags",
          helpText("Choose combination of tags for Receptors, Stressors and Technology with which to search the Literature and Spatial (limited to Receptors)."),
          
          fluidRow(
            align="center", 
            actionButton("btnAddAllLitQueries", "Add ALL Stressor-Receptor interactions", icon = icon("plus"))),
          
          fluidRow(
            box(
              width = 4,
              introBox(
                selectInput(
                  "selReceptors", "Receptors",
                  choices = choices_receptors,
                  multiple = T),
                actionButton("btnAddReceptorLitQueries", tagList("Add all Stressor interactions",  br(), "with selected Receptor(s)"), icon = icon("plus")),
                data.step  = 5,
                data.intro = "Select Receptor tags for Literature.")),
            
            box(
              width = 4,
              introBox(
                selectInput(
                  "selStressors", "Stressors",
                  choices = choices_stressors,
                  multiple = T),
                actionButton("btnAddStressorLitQueries", tagList("Add all Receptor interactions",  br(), "with selected Stressor(s)"), icon = icon("plus")),
                data.step  = 6,
                data.intro = "Select Stressor tags for Literature.")),
            
            box(
              width = 4,
              introBox(
                selectInput(
                  "selTech", "Technology",
                  choices = choices_tech),
                data.step  = 4,
                data.intro = "Select Technology for proposed marine renewable energy development."))),
          
          fluidRow(
            align="center", 
            actionButton("btnAddLitQuery", "Add Stressor/Receptor", icon = icon("plus"))),
          
          fluidRow(
            align="center", 
            introBox(
              box(
                title = "Queries", width=12,
                
                dataTableOutput("tblLitQueries"),
                
                actionButton("btnRmLitQuery"   , "Remove selected queries", icon = icon("minus")),
                actionButton("btnRmAllLitQuery", "Remove ALL queries"     , icon = icon("minus")),
                data.step  = 7,
                data.intro = "Refine your queries from the combinations of Receptors, Stressors & Technology.")))))), #)),      
    
    tabPanel(
      "Literature",
      DTOutput("tblLiterature")),
    
    tabPanel(
      "Spatial",
      
      helpText(
        "The Spatial results represent intersections with the given Location and spatial data on Receptors 
            (i.e. species, habitats and human activities)."),
      
      DTOutput("tblSpatial"),
      
      helpText(
        "To see the detailed results, you need to for now generate a Report. 
          In future, these results will be nested here by dataset. 
          Please also give results an extra 10 seconds to show up after setting Location and Tags under the Configure tab.
          So far, the only spatial datasets loaded from MarineCadastre.gov include the following tags:", 
        choices_sp_receptors %>% pull(tag) %>% paste(collapse = ", "), ".")),
    
    tabPanel(
      "Reports",
      
      introBox(
        textInput(
          "txtTitle", "Report Title"),
        data.step  = 8,
        data.intro = "Enter title of your custom report."),
      nbsp, "Include in report:",
      introBox(
        checkboxInput("ckboxLitTethys"       , "Literature"             , value = T),
        data.step  = 9,
        data.intro = "Check to include Literature from Tethys: linked titles based on selected Stressors, Receptors and Stressor-Receptor interactions."),
      introBox(
        checkboxInput("ckboxSpatialReceptors", "Spatial", value = T),
        data.step  = 10,
        data.intro = "Check to include Spatial data from MarineCadastre.gov: summary of intersection with drawn Location and footprints of selected Receptors (species, habitats and human activities)."),
      
      # disabled(
      #   checkboxInput("ckboxLitFERC"       , "Literature from FERC")),
      # disabled(
      #   checkboxInput("ckboxMgtTethys"     , "Management measures from Tethys")),
      introBox(
        dropdownButton(
          label = 'Download Report', icon=icon('file-o'), circle=F, size='sm',
          #bookmarkButton(id='btn_bookmark'),
          #actionButton(  'btn_download_url', 'Bookmark (url)', icon=icon('bookmark-o')),
          downloadButton('btn_download_pdf', 'Portable (*.pdf)', icon=icon('file-pdf-o')),
          downloadButton('btn_download_doc', 'Word (*.docx)', icon=icon('file-word-o')),
          downloadButton('btn_download_htm', 'Web (*.html)', icon=icon('file-text-o'))),
        data.step  = 11,
        data.intro = "Finally, generate report based on your desired output format: Word document, Adobe PDF or web page."))
    
    # tabPanel(
    #   "Projects",
    #   "Under construction", emo::ji("construction")),
    # 
    # tabPanel(
    #   "Regulations",
    #   "Under construction", emo::ji("construction")),
    # 
    # tabPanel(
    #   "Management",
    #   "Under construction", emo::ji("construction")),
    # 
    # tabPanel(
    #   "Reports",
    #   "Under construction", emo::ji("construction"))
  ))