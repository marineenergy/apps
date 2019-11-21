# Example source: https://github.com/r-spatial/mapedit/issues/95

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("MHK-env Report"),

    # Sidebar with a ui for grabbing mapedit data
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "selEnv", "Select Environment", c("Tidal", "Riverine", "Ocean")),
            selectInput(
                "selTech", "Select Technology", tech_choices),
            
            actionButton('save', 'Save Site')),
        
        # add map
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Draw Site",
                    editModUI("map")),
                tabPanel(
                    "Report",
                    h2("Custom Report"),
                    h3("Overview"),
                    h4("Location"),
                    leafletOutput("report_map", width=200, height=200),
                    uiOutput("tech_ui")))))
))

# See also: shiny_modules.R in mapedit
# https://github.com/r-spatial/mapedit/blob/master/inst/examples/shiny_modules.R
