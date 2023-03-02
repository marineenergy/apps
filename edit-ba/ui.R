tagList(
  # css styling ----
  includeCSS("www/styles.css"),
  shiny::tags$head(
    shiny::tags$script(
      src = "js/index.js")),
  # title ----
  navbarPage(
    "Edit", 
  
    
    # documents ----
    tabPanel(
      "BA Document Excerpts", value = "docs",
      span(
        span(
          actionButton(
            "refresh_btn",
            "Refresh BioAssessment docs table",
            icon  = icon("sync"),
            class = "btn btn-primary btn-lg btn-block")),
        span(
          h3("Editable BioAssessment Documents"))),
      helpText(
        HTML("The BioAssessment library contains Biological Assessment and Evaluation
             documents, of which excerpts have been manually tagged for 
             reference.")),
      span(
        span("Please add new BA project documents on "),
        shiny::tags$a("BA Documents tab.", onclick="customHref('tab_docs')")),
      br(),
      span(
        HTML("Then, click <b>Refresh BioAssessment docs table</b> at right for them to 
             become available as input choices.")),
      hr(),
      div(
        span(
          HTML("<b>Tags</b> filter by:"),
          icon("tags"),
          span(class="me-tag me-technology",  "Technology"),
          span(class="me-tag me-stressor",    "Stressor"),
          span(class="me-tag me-receptor",    "Receptor"),
          span(class="me-tag me-phase",       "Phase"),
          span(class="me-tag me-consequence", "Consequence")),
        br(), 
        span(
          HTML("Records are ordered by <b>Project</b>. To view recently added 
               records first, toggle <b>ID</b> twice."))),
      hr(), 
      # * ba docs table (dtedit) ----
      div(
        id = "ba_docs_table",
        # div(
          # actionButton(
          #   "sort_recent_btn",
          #   "Sort by most recent record",
          #   icon  = icon("sort"),
          #   class = "btn btn-primary"),
          # style = "padding: 0 5px 10px 0;"),
        shinycssloaders::withSpinner(
          uiOutput("ba_dt_edit"),
          # loading spinner:
          type = 8, color = "#007BFE"),
        style = "padding: 5px;")
    ),
    
    
    # project docs: prj_doc inputs ----
    tabPanel(
      "BA Documents", value = "tab_docs",
      
      fluidRow(
        
        column(
          width = 4,
          div(
            style = {"
              display: inline;
              white-space: initial;
              position: fixed;
              overflow-wrap: break-word;
              padding: 10px 10px 20px 10px;
              overflow: hidden;
              word-wrap: break-word; 
              word-break: break-word; 
            "},
            helpText(
              "Add new BA project docs below."),
            
            # PROJECT
            selectizeInput(
              "sel_prj",
              "BA Project",
              choices = ba_projects,
              options = list(
                create = T,
                onInitialize = I(
                  'function() { this.setValue("") }'),
                placeholder = 
                  "Select from menu or type to add new project")),
            # TODO: (BB did for ecoidx-up) if new project, update google sheet [data | marineenergy.app - Google Sheets](https://docs.google.com/spreadsheets/d/1MTlWQgBeV4eNbM2JXNXU3Y-_Y6QcOOfjWFyKWfdMIQM/edit#gid=5178015)
            
            # DOC
            selectizeInput(
              "sel_prj_doc",
              "BA Project Document", 
              choices = d_ba_docs$ba_doc_file,
              # choices = prj_doc_lookup$doc, 
              options = list(
                create = T,
                onInitialize = I(
                  'function() { this.setValue("") }'),
                placeholder = 
                  "Select from menu or type to add new doc")),
            
            # SAVE/UPDATE
            actionButton(
              "save_sel",
              "Save",
              icon = icon("save"),
              class = "btn-primary"),
            br()
          )), 
        
        # * table of existing BA projects ----
        column(
          width = 8,
          div(
            style = "
              display: inline;
              position: relative;
              overflow-wrap: break-word;
              overflow: hidden;
              float: center;
              padding: 0px 10px 20px 60px;",
            h3("Existing projects"),
            withSpinner(
              DTOutput("prj_doc_table"), 
              type = 8, color = "#007BFE"))
        )
      )
    )
  )
)