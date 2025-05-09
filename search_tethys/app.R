# libraries ----
source(here::here("functions.R")) # connection object

# variables ----
tags_csv <- here("data/tags.csv") # generated by tethys_scrape.Rmd

# data ----
tbl_tags <- read_csv(tags_csv) %>% 
    arrange(facet, item_label)
lst_tags <- list(
    # TODO: 
    # - add technologyType
    # - sort by tbl_tags !tag_parent, subindent (and later trim) child tags
    #Content   = filter(tbl_tags, facet == "content" ) %>% pull(item_label),
    Receptors = filter(tbl_tags, facet == "receptor") %>% pull(item_label),
    Stressors = filter(tbl_tags, facet == "stressor") %>% pull(item_label))



# user interface ----
ui <- fluidPage(
    
    fluidRow(
    # titlePanel("Tethys Literature Search"),
    
    # sidebarLayout(
    #     sidebarPanel(
            # TODO: 
            #  - 1. listbox of AND statements to select/remove with OR between
            #  - 2. add technology to select list: wind, wave, tide
            #  - 3. limit selectable elements after first choice to those having second
        selectInput(
            "tags", "Select tag(s):", 
            lst_tags,
            multiple = T)),
    fluidRow(
        
        mainPanel(
            DTOutput("tbl"))))

# server functions ----
server <- function(input, output) {

    output$tbl <- renderDT({
        req(input$tags)
        
        tags <- input$tags
        
        #browser()
        
        # Environment, Human Dimensions: https://tethys.pnnl.gov/node/643, https://tethys.pnnl.gov/node/2817,...
        # tags <- c("Environment", "Human Dimensions")
        
        res <- dbGetQuery(
            con, 
            glue("
                SELECT uri, title, COUNT(tag_text) AS tag_cnt from (
                    SELECT 
                      uri, 
                      data ->'title' ->> 0 AS title,
                      json_array_elements(data->'tags') ->> 0 as tag_text
                    FROM tethys_pubs) q 
                 WHERE tag_text IN ('{paste(tags, collapse = \"','\")}')
                 GROUP BY uri, title")) %>% 
            filter(tag_cnt == length(tags))
        
        res %>%
            mutate(
                Title = map2_chr(
                    title, uri,
                    function(x, y)
                        glue("<a href={y} target='_blank'>{x}</a>"))) %>%
            select(Title) %>%
            arrange(Title) %>%
            datatable(
                escape = F,
                options = list(
                    pageLength = 50,
                    dom = 't'))
        })
}

shinyApp(ui = ui, server = server)
