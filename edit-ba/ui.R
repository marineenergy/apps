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
            "Refresh BioAssessment Docs",
            icon  = icon("sync"),
            class = "btn btn-primary btn-lg btn-block")),
        span(
          h3("Editable BioAssessment Documents"))),
      helpText(
        HTML("The BioAssessment library contains Biological Assessment and Evaluation
             documents, of which excerpts have been manually tagged for 
             reference.")),
      span(
        span("Please add new BA projects and project documents into the "),
        shiny::tags$a(
          "BA Documents Google Sheet", 
          target = "_blank", 
          href   = "https://docs.google.com/spreadsheets/d/17QQ9A0G0SxIOfiuFCJFzikQ088cQbO_MMTBbjEHa9FU/edit")),
      br(),
      span(
        HTML("Then, click <b>Refresh BioAssessment Docs</b> for them to 
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
    )
  )
)