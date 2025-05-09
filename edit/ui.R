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
      "Document Interactions", value = "docs",
      span(
        span(
          actionButton(
            "refresh_btn",
            "Refresh FERC docs table",
            icon  = icon("sync"),
            class = "btn btn-primary btn-lg btn-block")),
        span(
          h3("Editable FERC Documents"))),
      helpText(
        HTML("The FERC eLibrary contains environmental compliance project 
             documents, of which excerpts have been manually tagged for 
             reference.")),
      span(
        span("Please add new projects, project documents, and document sections on "),
        shiny::tags$a("Documents tab.", onclick="customHref('prj_docs')")),
      br(),
      span(
        HTML("Then, click <b>Refresh FERC docs table</b> at right for them to 
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
      # * ferc docs table (dtedit) ----
      div(
        id = "ferc_docs_table",
        # div(
          # actionButton(
          #   "sort_recent_btn",
          #   "Sort by most recent record",
          #   icon  = icon("sort"),
          #   class = "btn btn-primary"),
          # style = "padding: 0 5px 10px 0;"),
        shinycssloaders::withSpinner(
          uiOutput("ferc_dt_edit"),
          # loading spinner:
          type = 8, color = "#007BFE"),
        style = "padding: 5px;")
    ),
    
    
    # project docs: prj_doc_sec inputs ----
    tabPanel(
      "Documents", value = "prj_docs",
      
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
              "Add new projects, project docs, and project doc sections below."),
            
            # PROJECT
            selectizeInput(
              "sel_prj",
              "Project",
              choices = prj_doc_sec_lookup$prj,
              # prj_sites_lookup$project,
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
              "Project Document", 
              choices = prj_doc_sec_lookup$doc,
              # choices = prj_doc_lookup$doc, 
              options = list(
                create = T,
                onInitialize = I(
                  'function() { this.setValue("") }'),
                placeholder = 
                  "Select from menu or type to add new doc")),
            
            # SECTION
            selectizeInput(
              "sel_prj_doc_sec",
              "Project Document Section",
              choices = prj_doc_sec_lookup$sec,
              options = list(
                create = T,
                onInitialize = I(
                  'function() { this.setValue("") }'),
                placeholder = 
                  "Select from menu or type to add new section")),
            
            # URL
            selectizeInput(
              "sel_prj_doc_sec_url",
              "Project Document Section URL",
              choices = prj_doc_sec_lookup$url,
              options = list(
                create = T,
                onInitialize = I(
                  'function() { this.setValue("") }'),
                placeholder = 
                  "Select from menu or type to add URL")),
            
            # SAVE/UPDATE
            actionButton(
              "save_sel",
              "Save",
              icon = icon("save"),
              class = "btn-primary"),
            br()
          )), 
        
        # * table of existing projects ----
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
              DTOutput("prj_table"), 
              type = 8, color = "#007BFE"))
        )
      )
    )
  )
)