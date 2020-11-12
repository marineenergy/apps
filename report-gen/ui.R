dashboardPage(
  dashboardHeader(title = "MHK-env Report"),
  
  dashboardSidebar(
    collapsed = F,
    textInput(
      "txtTitle", "Report Title"),
    nbsp, "Include in report:",
    checkboxInput("ckboxLitTethys"       , "Literature from Tethys"             , value = T),
    disabled(
      checkboxInput("ckboxLitFERC"       , "Literature from FERC")),
    checkboxInput("ckboxSpatialReceptors", "Spatial intersection with Receptors", value = T),
    disabled(
      checkboxInput("ckboxMgtTethys"     , "Management measures from Tethys")),
    dropdownButton(
      label = 'Download Report', icon=icon('file-o'), circle=F, size='sm',
      #bookmarkButton(id='btn_bookmark'),
      actionButton(  'btn_download_url', 'Bookmark (url)', icon=icon('bookmark-o')),
      downloadButton('btn_download_pdf', 'Portable (*.pdf)', icon=icon('file-pdf-o')),
      downloadButton('btn_download_doc', 'Word (*.docx)', icon=icon('file-word-o')),
      downloadButton('btn_download_htm', 'Web (*.html)', icon=icon('file-text-o')))),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    useShinyjs(),
    
    tabsetPanel(
      
      tabPanel(
        "Technology, Location",
        box(
          width = 12,
          selectInput(
            "selTech", "Technology",
            choices = choices_tech) %>% 
            tagAppendAttributes(style = "margin-left: 40px")),
        box(
          width = 12,
          tagLabel("Location") %>% 
            tagAppendAttributes(style = "margin-left: 40px;"), 
          editModUI("mapEdit"))),      
      
      tabPanel(
        "Stressors & Receptors",
        box(
          selectInput(
            "selStressors", "Stressors",
            choices = choices_stressors,
            multiple = T)),
        box(
          selectInput(
          "selReceptors", "Receptors",
          choices = choices_receptors,
          multiple = T)),
        box(
          title = "Stressor AND Receptor interactions", width = 12,
          helpText("Click on a Stressor-Receptor interaction in the table below to include it in your report."),
          rHandsontableOutput("tblStressorReceptor"))),
      
      tabPanel(
        "Projects",
        "Under construction", emo::ji("construction")),
      
      tabPanel(
        "Regulations",
        "Under construction", emo::ji("construction"))
    )))