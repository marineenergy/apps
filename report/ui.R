# Example source: https://github.com/r-spatial/mapedit/issues/95

# Define UI for application that draws a histogram
dashboardPage(
    dashboardHeader(title = "MHK-env Report"),
    
    dashboardSidebar(
        textInput(
            "txtTitle", "Title"),
        selectInput(
            "selEnv", "Select Environment", c("Tidal", "Riverine", "Ocean")),
        selectInput(
            "selTech", "Select Technology", tech_choices),
        leafletOutput("side_map", width=200, height=200)),
        #actionButton('save', 'Save Site')),

    dashboardBody(
        tabsetPanel(
            tabPanel(
                "Draw Site",
                editModUI("mapEdit")),
            tabPanel(
                "Report",
                h2(verbatimTextOutput("txtTitle")),
                accordion(
                    accordionItem(
                        id = 1,
                        title = tagList(icon("gear"), "Configuration"),
                        color = "danger",
                        collapsed = FALSE,
                        h4("Location"),
                        leafletOutput("report_map", width=200, height=200),
                        uiOutput("tech_ui")),
                    accordionItem(
                        id = 2,
                        title = tagList(
                            icon("tint"), "Physical & Oceanographic"),
                        color = "warning",
                        collapsed = TRUE,
                        HTML(renderMarkdown(here("report/data/text_01.md")))),
                    accordionItem(
                        id = 2,
                        title = tagList(
                            icon("leaf"), "Habitats & Species"),
                        color = "warning",
                        collapsed = TRUE,
                        HTML(renderMarkdown(here("report/data/text_01.md"))))
                    ))))
)

# See also: shiny_modules.R in mapedit
# https://github.com/r-spatial/mapedit/blob/master/inst/examples/shiny_modules.R
