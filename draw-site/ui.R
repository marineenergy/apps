# Example source: https://github.com/r-spatial/mapedit/issues/95

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Draw Polygon of Site"),

    # Sidebar with a ui for grabbing mapedit data
    sidebarLayout(
        sidebarPanel(
            actionButton('save', 'Save from Map')
        ),
        
        # add map
        mainPanel(
            editModUI("map")
        )
    )
))

# See also: shiny_modules.R in mapedit
# https://github.com/r-spatial/mapedit/blob/master/inst/examples/shiny_modules.R
