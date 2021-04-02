library(shiny)
library(googleAuthR)
library(fs)
library(glue)
library(here)
library(readr)
library(DT)
library(dplyr)

# * [Client ID for Web application – APIs & Services – iea-uploader – Google API Console](https://console.developers.google.com/apis/credentials/oauthclient/596429062120-lsgv9f8pbas11o7ub9b9cm0earf1dpu5.apps.googleusercontent.com?authuser=3&project=iea-uploader)
options(googleAuthR.webapp.client_id = "596429062120-lsgv9f8pbas11o7ub9b9cm0earf1dpu5.apps.googleusercontent.com")

ui <- fluidPage(
    
  titlePanel("Sample Google Sign-In"),
  
  sidebarLayout(
    
    sidebarPanel(
      googleSignInUI("gar"), width = 2),
    
    mainPanel(
      with(
        tags, 
        dl(dt("Name"), dd(textOutput("g_name")),
        dt("Email"), dd(textOutput("g_email")),
        dt("Image"), dd(uiOutput("g_image")) )),
      textInput(
        "txtTitle", "Report Title"), 
      actionButton(
        "btnGenerate",
        "Generate Report"),
      
      fluidRow(
        column(
          12,
          DTOutput("tblReports")))
      # fluidRow(
      #   column(
      #     12,
      #     p("This app has a log file which is appended to", "every second."))),
      # fluidRow(
      #   column(
      #     6, 
      #     wellPanel(
      #       "This side uses a reactiveFileReader, which is monitoring",
      #       "the log file for changes every 0.5 seconds.",
      #       verbatimTextOutput("fileReaderText")) ),
      #   column(6, wellPanel(
      #     "This side uses a reactivePoll, which is monitoring",
      #     "the log file for changes every 4 seconds.",
      #     verbatimTextOutput("pollText")) ))
    )
  )
)

server <- function(input, output, session) {
    
  sign_ins <- shiny::callModule(googleSignIn, "demo")
    
  output$g_name  = renderText({ input[["gar-g_name"]] })
  output$g_email = renderText({ input[["gar-g_email"]] })
  output$g_image = renderUI({ img(src=input[["gar-g_image"]]) })
  
  # Create a random name for the log file
  logfilename <- paste0('logfile',
                        floor(runif(1, 1e+05, 1e+06 - 1)),
                        '.txt')
  
  
  # ============================================================
  # This part of the code writes to the log file every second.
  # Writing to the file could be done by an external process.
  # In this example, we'll write to the file from inside the app.
  logwriter <- observe({
    # Invalidate this observer every second (1000 milliseconds)
    invalidateLater(1000, session)
    
    # Clear log file if more than 10 entries
    if (file.exists(logfilename) &&
        length(readLines(logfilename)) > 10) {
      unlink(logfilename)
    }
    
    # Add an entry to the log file
    cat(as.character(Sys.time()), '\n', file = logfilename,
        append = TRUE)
  })
  
  # When the client ends the session, suspend the observer and
  # remove the log file.
  session$onSessionEnded(function() {
    logwriter$suspend()
    unlink(logfilename)
  })
  
  # ============================================================
  # This part of the code monitors the file for changes once per
  # 0.5 second (500 milliseconds).
  fileReaderData <- reactiveFileReader(500, session,
                                       logfilename, readLines)
  
  output$fileReaderText <- renderText({
    # Read the text, and make it a consistent number of lines so
    # that the output box doesn't grow in height.
    text <- fileReaderData()
    length(text) <- 14
    text[is.na(text)] <- ""
    paste(text, collapse = '\n')
  })
  
  
  # ============================================================
  # This part of the code monitors the file for changes once
  # every four seconds.
  # pollData <- reactivePoll(
  #   4000, session,
  #   checkFunc = function() {
  #     if (file.exists(logfilename))
  #       file.info(logfilename)$mtime[1]
  #     else
  #       ""
  #   },
  #   # This function returns the content of the logfile
  #   valueFunc = function() {
  #     readLines(logfilename)
  #   }
  # )
  
  usrReports <- reactivePoll(
    2000, session,
    checkFunc = function() {
      
      if (is.null(input[["gar-signed_in"]])) 
        return("")
      
      g_email          <- input[["gar-g_email"]]
      dir_user_reports <- here(glue("reports/{g_email}"))
      
      usr_reports <- dir_ls(dir_user_reports) %>% basename()
      
      #message(glue("usrReports - checkFunc: {paste(usr_reports, collapse = ', ')}"))
      usr_reports
    },
    # This function returns the content of the logfile
    valueFunc = function() {
      
      if (!input[["gar-signed_in"]]) 
        return(tibble(path = "", file = ""))
      
      g_email          <- input[["gar-g_email"]]
      dir_user_reports <- here(glue("reports/{g_email}"))
      
      #message(glue("usrReports - valueFunc: {paste(usr_reports, collapse = ', ')}"))
      tibble(
        path = dir_ls(dir_user_reports)) %>% 
        mutate(
          file = basename(path))
    }
  )
  
  # output$pollText <- renderText({
  #   # Read the text, and make it a consistent number of lines so
  #   # that the output box doesn't grow in height.
  #   text <- pollData()
  #   length(text) <- 14
  #   text[is.na(text)] <- ""
  #   paste(text, collapse = '\n')
  # })
  # 

  btnGenerateRun <- observe({

    # Execute selections on data upon button-press
    if(input$btnGenerate == 0) return()
    
    message(glue("input$btnGenerate: {isolate(input$btnGenerate)}"))
    
    req(input[["gar-signed_in"]])
    
    rpt_title <- isolate(input$txtTitle)
    g_email   <- isolate(input[["gar-g_email"]])
    
    message(glue("rpt_title: {rpt_title}"))
    message(glue("g_email: {g_email}"))
    
    if (rpt_title=='') return()
    
    rpt_out <- here(glue("reports/{g_email}/{rpt_title}.html"))
    
    message(glue("rpt_out: {rpt_out}"))
    
    dir_create(dirname(rpt_out))
    
    write_lines("test", rpt_out)
    
    # # subset based on selection
    # Subgeodata <- subset(geodata, BSL == selection & Position.Location == selection2)
    # 
    # # browser()
    # # aggregate by postcode
    # Subgeodata <- Subgeodata[1:2] #no longer need other columns
    # AggSubGdata <- aggregate(. ~ Postcode, data=Subgeodata, FUN=sum)
    # write.csv(AggSubGdata
    #           , file = "solution.csv"
    #           , row.names=F
    # )
  })
  
  output$tblReports = renderDT({
    req(input[["gar-signed_in"]])
    usrReports()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)