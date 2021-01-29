library(shiny)
library(googleAuthR)

# * [Client ID for Web application – APIs & Services – iea-uploader – Google API Console](https://console.developers.google.com/apis/credentials/oauthclient/596429062120-lsgv9f8pbas11o7ub9b9cm0earf1dpu5.apps.googleusercontent.com?authuser=3&project=iea-uploader)
options(googleAuthR.webapp.client_id = "596429062120-lsgv9f8pbas11o7ub9b9cm0earf1dpu5.apps.googleusercontent.com")

ui <- fluidPage(
    
    titlePanel("Sample Google Sign-In"),
    
    sidebarLayout(
        sidebarPanel(
            googleSignInUI("demo")
        ),
        
        mainPanel(
            with(tags, dl(dt("Name"), dd(textOutput("g_name")),
                          dt("Email"), dd(textOutput("g_email")),
                          dt("Image"), dd(uiOutput("g_image")) ))
        )
    )
)

server <- function(input, output, session) {
    
    sign_ins <- shiny::callModule(googleSignIn, "demo")
    
    output$g_name = renderText({ sign_ins()$name })
    output$g_email = renderText({ sign_ins()$email })
    output$g_image = renderUI({ img(src=sign_ins()$image) })
    
}

# Run the application 
shinyApp(ui = ui, server = server)