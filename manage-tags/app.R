librarian::shelf(
  dplyr, DT, shiny)

source(here("functions.R"))

# mgt_tags ----
mgt_csv      <- here("data/tethys_mgt.csv")
mgt_tags_csv <- here("data/tethys_mgt_tags.csv")

stopifnot(file.exists(mgt_csv))
d_mgt <- read_csv(mgt_csv)

d_mgt_tags <- d_mgt %>% 
  select(
    Technology, Stressor, 
    Management = `Management Measure Category`,
    Phase = `Phase of Project`) %>% 
  pivot_longer(everything(), names_to="tag_category", values_to="tag_content") %>% 
  group_by(tag_category, tag_content) %>% 
  summarise(.groups="drop") %>% 
  bind_rows(
    d_mgt %>% 
      select(
        Receptor, `Specific Receptor`) %>% 
      group_by(Receptor, `Specific Receptor`) %>% 
      summarise(.groups="drop") %>% 
      mutate(
        tag_category = "Receptor") %>% 
      select(tag_category, tag_content = Receptor, tag_mgt_ReceptorSpecifics = `Specific Receptor`)) %>% 
  arrange(tag_category, tag_content, tag_mgt_ReceptorSpecifics) %>% 
  mutate(
    content = "tethys_mgt") %>% 
  relocate(content)
write_csv(d_mgt_tags, mgt_tags_csv)

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