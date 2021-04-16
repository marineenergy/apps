tagList(
  tags$head(
    includeHTML("www/components/head.html")),
  introjsUI(),
  useShinyjs(),
  use_waiter(),
  # TODO: place Tour bttn
  actionButton("tour", "Tour the app", icon = icon("question-circle")),
  
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
      a("Regulations", href = glue("{web_url}/regs.html"))),
      
    tabPanel(
      a("Interactions", href = glue("{web_url}/env.html"))),
    
    tabPanel(
      a("Documents", href = glue("{web_url}/ferc.html"))),
      
    tabPanel(
      a("Management", href = glue("{web_url}/mgt.html"))),
      
    tabPanel(
      "Configure",
      helpText('The Report tool allows users to search Marine Energy literature and spatial information by geography, and generate a report of your search. The tool is separated into functional tabs described below.'),
      tabsetPanel(
        id = "tabConfigure",
        # introBox(),
        # data.step  = 3,
        # data.intro = "Select."),
        tabPanel(
          "Location",
          helpText('This tab lets users develop queries for their report. To develop a query based on geographic area of interest,  select the "polygon" or "rectangle" icon in the menu bar to the left of the map. Using either tool, create a boundary around the location of interest. If there are multiple locations of interest, complete the boundaries of one location before starting the next. To adjust the shape of the boundary, select the "layer" icon and edit points as necessary. To delete a shape, select the "delete" icon, followed by the shape.'),
          helpText('Once the desired shape has been created, navigate to the "Tags" tab to choose the receptors, stressors, and technology of interest.')
          ,
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
          helpText('Select the receptors, stressors, and technology that you would like to query for the geography selected. Note that only one option can be selected in the "Technology" form field at a time. Once the desired combination of receptors, stressors, and technology have been chosen, select the "Add Stressor/Receptor" button to add that tag combination to the list of queries. You may add as many queries to the list as you like. If there\'s interest to query additional combinations, clear the form fields using the "Backspace" or "Delete" keys on the keyboard, and repeat the steps above. To query all stressor-receptor interactions in the geographic range, click the "Add ALL Stressor-Receptor Interactions" button located above the "Stressor" form field.'),
          helpText('Once all queries have been added, navigate to the "Literature" or "Spatial" tab to see a summary of the literature and spatial results. A more detailed compilation of that information will be provided in the report.')
          ,
          
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
      strong("Option"), ": Limit Literature to those tagged with a location and within", 
      tags$ul(
        tags$li(textInput("txtDistance", "distance of drawn Location")),
        tags$li(selectInput("selDistanceUnits", "distance units", c("mi","nmi","km")))),
      helpText('Below is the available literature based on the selected tags for stressor, receptor, and technology. Click a link to access the literature.'),
      DTOutput("tblLiterature")),
    
    tabPanel(
      "Spatial",
      helpText('Below is a summary of the available spatial information for the selected geography, and receptor, stressor, and technology tags.'),
      
      DTOutput("tblSpatial"),
      
      helpText(
        "To see the detailed results, you need to for now generate a Report. 
          In future, these results will be nested here by dataset. 
          Please also give results an extra 10 seconds to show up after setting Location and Tags under the Configure tab.
          So far, the only spatial datasets loaded from MarineCadastre.gov include the following tags:", 
        choices_sp_receptors %>% pull(tag) %>% paste(collapse = ", "), ".")),
    
    tabPanel(
      "Reports",
      helpText('Generate a Report of the selected geography, and receptor, stressor, and technology tags.'),
      introBox(
        textInput(
          "txtTitle", "Report Title: create a title for your report and enter it here."),
        data.step  = 8,
        data.intro = "Enter title of your custom report."),
      helpText("Select whether you would like literature or spatial information included in the report, or both:"),
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
      helpText('Select the download file type here:'),
      introBox(
        dropdownButton(
          label = 'Download Report', icon=icon('file-o'), circle=F, size='sm',
          #bookmarkButton(id='btn_bookmark'),
          #actionButton(  'btn_download_url', 'Bookmark (url)', icon=icon('bookmark-o')),
          downloadButton('btn_download_pdf', 'Portable (*.pdf)', icon=icon('file-pdf-o')),
          downloadButton('btn_download_doc', 'Word (*.docx)', icon=icon('file-word-o')),
          downloadButton('btn_download_htm', 'Web (*.html)', icon=icon('file-text-o'))),
        data.step  = 11,
        data.intro = "Finally, generate report based on your desired output format: Word document, Adobe PDF or web page.")),
    
    tabPanel(
      a("About", href = glue("{web_url}/about.html"))),
    
    tabPanel(
      a("Help", href = glue("{web_url}/help.html"))),
    
    div(
      class = "footer",
      includeHTML("www/components/footer.html"))
    
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