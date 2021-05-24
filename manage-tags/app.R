librarian::shelf(
  dplyr, DT, shiny)

source(here("functions.R"))

# mgt_tags ----
mgt_csv <- here("data/tethys_mgt.csv")

stopifnot(file.exists(mgt_csv))
d_mgt <- read_csv(mgt_csv)

d_mgt_tags <- d_mgt %>% 
  select(
    Technology, Stressor, 
    Management = `Management Measure Category`,
    Phase = `Phase of Project`) %>% 
  pivot_longer(everything(), names_to="category", values_to="tag_mgt") %>% 
  group_by(category, tag_mgt) %>% 
  summarise(.groups="drop") %>% 
  bind_rows(
    d_mgt %>% 
      select(
        Receptor, ReceptorSpecifics = `Specific Receptor`) %>% 
      group_by(Receptor, ReceptorSpecifics) %>% 
      summarise(.groups="drop") %>% 
      mutate(
        category = "Receptor") %>% 
      select(category, tag_mgt = Receptor, ReceptorSpecifics)) %>% 
  arrange(category, tag_mgt, ReceptorSpecifics)

ui <- fluidPage(

  titlePanel("Manage Tags"),

  sidebarLayout(
    sidebarPanel(width=1),

    mainPanel(
      width=11,
      dataTableOutput("mgt_tags"))
))

server <- function(input, output, session) {

  output$mgt_tags <- renderDataTable({
    d_mgt_tags
  })
}

shinyApp(ui = ui, server = server)