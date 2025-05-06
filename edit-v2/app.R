# AI generated at https://gallery.shinyapps.io/assistant/ 
# [Shiny Assistant: Prototype and build Shiny applications with the help of AI | Winston Chang | Posit - YouTube](https://www.youtube.com/watch?v=fJNKdwdVQ8Q)
#
# Prompt 1:
# Create a table editing app that updates a database with two tables, one of
# which is for a multi-select describing various tags for the given row in the
# main table.
#
# Prompt 2:
# Great! Add buttons for New, Edit and Delete (selected) and use a modal window
# to edit a given row, rather than the current form below the table.

library(shiny)
library(DT)
library(RSQLite)
library(dplyr)
library(bslib)

# Initialize the database
init_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), "app_data.sqlite")
  
  # Create main table if it doesn't exist
  dbExecute(con, "CREATE TABLE IF NOT EXISTS main_table (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT,
    value INTEGER
  )")
  
  # Create tags table if it doesn't exist
  dbExecute(con, "CREATE TABLE IF NOT EXISTS tags_table (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    main_id INTEGER,
    tag TEXT,
    FOREIGN KEY (main_id) REFERENCES main_table(id)
  )")
  
  # Insert sample data if tables are empty
  if (dbGetQuery(con, "SELECT COUNT(*) FROM main_table")[1,1] == 0) {
    dbExecute(con, "INSERT INTO main_table (name, value) VALUES ('Item 1', 10), ('Item 2', 20), ('Item 3', 30)")
    dbExecute(con, "INSERT INTO tags_table (main_id, tag) VALUES (1, 'Tag A'), (1, 'Tag B'), (2, 'Tag B'), (2, 'Tag C'), (3, 'Tag A')")
  }
  
  dbDisconnect(con)
}

# Initialize the database
init_db()

# Define UI
ui <- page_fluid(
  title = "Table Editing App",
  layout_column_wrap(
    width = 1/3,
    actionButton("new_btn", "New", class = "btn-primary"),
    actionButton("edit_btn", "Edit", class = "btn-info"),
    actionButton("delete_btn", "Delete", class = "btn-danger")
  ),
  DTOutput("main_table"),
  verbatimTextOutput("selected_row_info")
)

# Define server logic
server <- function(input, output, session) {
  # Reactive value to store the selected row
  selected_row <- reactiveVal(NULL)
  
  # Function to get data from database
  get_data <- reactive({
    con <- dbConnect(RSQLite::SQLite(), "app_data.sqlite")
    data <- dbGetQuery(con, "SELECT * FROM main_table")
    dbDisconnect(con)
    data
  })
  
  # Render main table
  output$main_table <- renderDT({
    datatable(get_data(), selection = 'single', options = list(pageLength = 5))
  })
  
  # Update selected row when a row is clicked
  observeEvent(input$main_table_rows_selected, {
    row <- get_data()[input$main_table_rows_selected, ]
    if (!is.null(row)) {
      selected_row(row$id)
    }
  })
  
  # New button click
  observeEvent(input$new_btn, {
    showModal(modalDialog(
      title = "Add New Row",
      textInput("modal_name", "Name"),
      numericInput("modal_value", "Value", value = 0),
      selectInput("modal_tags", "Tags", choices = c("Tag A", "Tag B", "Tag C", "Tag D"), multiple = TRUE),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("modal_save", "Save")
      )
    ))
  })
  
  # Edit button click
  observeEvent(input$edit_btn, {
    req(selected_row())
    con <- dbConnect(RSQLite::SQLite(), "app_data.sqlite")
    row <- dbGetQuery(con, sprintf("SELECT * FROM main_table WHERE id = %d", selected_row()))
    tags <- dbGetQuery(con, sprintf("SELECT tag FROM tags_table WHERE main_id = %d", selected_row()))$tag
    dbDisconnect(con)
    
    showModal(modalDialog(
      title = "Edit Row",
      textInput("modal_name", "Name", value = row$name),
      numericInput("modal_value", "Value", value = row$value),
      selectInput("modal_tags", "Tags", choices = c("Tag A", "Tag B", "Tag C", "Tag D"), selected = tags, multiple = TRUE),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("modal_save", "Save")
      )
    ))
  })
  
  # Delete button click
  observeEvent(input$delete_btn, {
    req(selected_row())
    showModal(modalDialog(
      title = "Confirm Deletion",
      "Are you sure you want to delete this row?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("modal_delete_confirm", "Delete", class = "btn-danger")
      )
    ))
  })
  
  # Confirm delete
  observeEvent(input$modal_delete_confirm, {
    con <- dbConnect(RSQLite::SQLite(), "app_data.sqlite")
    dbExecute(con, sprintf("DELETE FROM main_table WHERE id = %d", selected_row()))
    dbExecute(con, sprintf("DELETE FROM tags_table WHERE main_id = %d", selected_row()))
    dbDisconnect(con)
    selected_row(NULL)
    removeModal()
  })
  
  # Save modal data
  observeEvent(input$modal_save, {
    con <- dbConnect(RSQLite::SQLite(), "app_data.sqlite")
    
    if (is.null(selected_row())) {
      # Add new row
      dbExecute(con, sprintf("INSERT INTO main_table (name, value) VALUES ('%s', %d)", input$modal_name, input$modal_value))
      new_id <- dbGetQuery(con, "SELECT last_insert_rowid() as id")$id
    } else {
      # Update existing row
      dbExecute(con, sprintf("UPDATE main_table SET name = '%s', value = %d WHERE id = %d", input$modal_name, input$modal_value, selected_row()))
      new_id <- selected_row()
      
      # Delete existing tags for this row
      dbExecute(con, sprintf("DELETE FROM tags_table WHERE main_id = %d", new_id))
    }
    
    # Insert new tags
    if (length(input$modal_tags) > 0) {
      tags_df <- data.frame(main_id = rep(new_id, length(input$modal_tags)), tag = input$modal_tags)
      dbWriteTable(con, "tags_table", tags_df, append = TRUE, row.names = FALSE)
    }
    
    dbDisconnect(con)
    selected_row(NULL)
    removeModal()
  })
  
  # Display selected row info
  output$selected_row_info <- renderText({
    if (is.null(selected_row())) {
      return("No row selected")
    }
    
    con <- dbConnect(RSQLite::SQLite(), "app_data.sqlite")
    row <- dbGetQuery(con, sprintf("SELECT * FROM main_table WHERE id = %d", selected_row()))
    tags <- dbGetQuery(con, sprintf("SELECT tag FROM tags_table WHERE main_id = %d", selected_row()))$tag
    dbDisconnect(con)
    
    sprintf("Selected Row:\nID: %d\nName: %s\nValue: %d\nTags: %s",
            row$id, row$name, row$value, paste(tags, collapse = ", "))
  })
}

# Run the application
shinyApp(ui = ui, server = server)