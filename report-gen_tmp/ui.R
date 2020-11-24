dashboardPage(
  dashboardHeader(title = "MHK-env Report"),

  dashboardSidebar(
    collapsed = F,
    
    introjsUI(),
    actionButton("tour", "Tour the app", icon = icon("question-circle")),
    
    introBox(
      textInput(
        "txtTitle", "Report Title"),
      data.step  = 7,
      data.intro = "Enter title of your custom report."),
    nbsp, "Include in report:",
    introBox(
      checkboxInput("ckboxLitTethys"       , "Literature from Tethys"             , value = T),
      data.step  = 8,
      data.intro = "Check to include Literature from Tethys: linked titles based on selected Stressors, Receptors and Stressor-Receptor interactions."),
    disabled(
      checkboxInput("ckboxLitFERC"       , "Literature from FERC")),
    introBox(
      checkboxInput("ckboxSpatialReceptors", "Spatial intersection with Receptors", value = T),
      data.step  = 9,
      data.intro = "Check to include Spatial data from MarineCadastre.gov: summary of intersection with drawn Location and footprints of selected Receptors (species, habitats and human activities)."),
    disabled(
      checkboxInput("ckboxMgtTethys"     , "Management measures from Tethys")),
    introBox(
      dropdownButton(
        label = 'Download Report', icon=icon('file-o'), circle=F, size='sm',
        #bookmarkButton(id='btn_bookmark'),
        #actionButton(  'btn_download_url', 'Bookmark (url)', icon=icon('bookmark-o')),
        downloadButton('btn_download_pdf', 'Portable (*.pdf)', icon=icon('file-pdf-o')),
        downloadButton('btn_download_doc', 'Word (*.docx)', icon=icon('file-word-o')),
        downloadButton('btn_download_htm', 'Web (*.html)', icon=icon('file-text-o'))),
      data.step  = 10,
      data.intro = "Finally, generate report based on your desired output format: Word document, Adobe PDF or web page.")),
  
  dashboardBody(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    useShinyjs(),
    use_waiter(),
    
    tabsetPanel(
      
      tabPanel(
        "Spatial",

        fluidRow(
          box(
            width = 12,
            introBox(
              "Location",
              # tagLabel("Location") %>% 
              # tagAppendAttributes(style = "margin-left: 40px;"), 
              editModUI("mapEdit"),      
              data.step  = 2,
              data.intro = "
              Enter Location by selecting a drawing tool and clicking on the map. 
              For polygons, click on successive points and be sure to finish at the first point.
              For boxes, click and hold for one corner, drag and release to other. 
              Before drawing, you can also zoom in/out by double clicking, and pan by click and dragging.")))),
      
      tabPanel(
        introBox(
          "Literature",
          data.step  = 3,
          data.intro = "Stressors & Receptors tab for another section of inputs."),
        helpText("Choose combination of tags for Technology, Stressors and Receptors with which to search the Tethys literature."),
        
        fluidRow(
          box(
            width = 12,
            introBox(
              selectInput(
                "selTech", "Technology",
                choices = choices_tech), # %>% 
              #tagAppendAttributes(style = "margin-left: 40px"),
              data.step  = 3.5,
              data.intro = "Select Technology for proposed marine renewable energy development."))),
        
        fluidRow(
          align="center", 
          actionButton("btnAddAllLitQueries", "Add ALL Stressor-Receptor interactions", icon = icon("plus"))),
        
        fluidRow(
          box(
            width = 6,
            introBox(
              selectInput(
                "selReceptors", "Receptors",
                choices = choices_receptors,
                multiple = T),
              actionButton("btnAddReceptorLitQueries", "Add all Stressor interactions with selected Receptor(s)", icon = icon("plus")),
              data.step  = 5,
              data.intro = "Select Receptor tags for Literature.")),
          
          box(
            width = 6,
            introBox(
              selectInput(
                "selStressors", "Stressors",
                choices = choices_stressors,
                multiple = T),
              actionButton("btnAddStressorLitQueries", "Add all Receptor interactions with selected Stressor(s)", icon = icon("plus")),
              data.step  = 4,
              data.intro = "Select Stressor tags for Literature."))),
        
        fluidRow(
          align="center", 
          actionButton("btnAddLitQuery", "Add Stressor-Receptor interaction", icon = icon("plus"))),
        
        fluidRow(
          box(
            title = "Queries", width=12,
            dataTableOutput("tblLitQueries"),
            
            # selectInput(
            #   "selLitQueries", "Queries (n=0)",
            #   choices = "", size = 5, multiple = T, selectize=F, width="100%"),
            # box(
            #   width = 12,
            #   introBox(
            #     title = "Stressor AND Receptor interactions",
            #     helpText("Click on a Stressor-Receptor interaction in the table below to include it in your report."),
            #     data.step  = 6,
            #     data.intro = "Select combinations of Stressor and Receptor tags for relevant Literature on interactions."),
            #   rHandsontableOutput("tblStressorReceptor"))),
            
            actionButton("btnRmLitQuery", "Remove Query", icon = icon("minus"))))),
      
      tabPanel(
        "Projects",
        "Under construction", emo::ji("construction")),
      
      tabPanel(
        "Regulations",
        "Under construction", emo::ji("construction"))
    )))