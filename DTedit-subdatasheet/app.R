# global.R ----

dir_scripts <<- here::here("scripts")
source(file.path(dir_scripts, "common_2.R"))
source(file.path(dir_scripts, "db.R"))
source(file.path(dir_scripts, "shiny_report.R"))
source(file.path(dir_scripts, "update.R"))

# update_projects() # update prj_sites_csv
prj_sites_csv <- here("data/project_sites.csv")

# prep data
match_prj <- function(
    prj_doc, 
    prj_names     = projects$project, 
    alt_prj_names = projects$project_alt_name) {
    
    prj_match <- prj_doc %>% 
        str_extract(
            regex(
                prj_names %>% 
                    str_replace_all("-", " ") %>% 
                    str_sub(), 
                ignore_case = T)) %>% 
        na.omit() %>% 
        unlist(recursive = T) %>% 
        first()
    if (is.na(prj_match)) {
        prj_match <- prj_doc %>% 
            str_extract(
                regex(
                    alt_prj_names %>% 
                        str_replace_all("-", " ") %>% 
                        str_sub(), 
                    ignore_case = T)) %>% 
            na.omit() %>% 
            unlist(recursive = T) %>% 
            first()
    }
    prj_match
}

projects <- read_csv(prj_sites_csv) %>% 
    mutate(
        project_alt_name = case_when(
            project == "OPT Reedsport" ~ "REEDSPORT OPT",
            project == "Pacwave-N"     ~ "Pacwave North",
            project == "Pacwave-S"     ~ "Pacwave South",
            project == "RITE"          ~ "Roosevelt Island Tidal Energy")) %>% 
    select(project, project_alt_name)

prj_docs <- get_ferc() %>%
    select(prj_document, prj_doc_attachment, prj_doc_attach_url) %>% 
    distinct() %>% 
    mutate(project = map(prj_document, match_prj)) %>% 
    relocate(project) %>% 
    arrange(project)

# ui.R ----

ui <- fluidPage(
  titlePanel("Subdatasheet"),
  sidebarLayout(
    #sidebarPanel(),
    mainPanel(
      dataTableOutput("tbl"))))

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$tbl <- renderDataTable({
    prj_docs }, 
    options = list(
      columnDefs = list(
        list(visible = FALSE, targets = c(0, 2:4)),
        list(orderable = FALSE, className = 'details-control', targets = 1),
        list(className = 'dt-left', targets = 1:3),
        list(className = 'dt-right', targets = 4))),
    callback = JS("
      table.column(1).nodes().to$().css({cursor: 'pointer'});
      var format = function(d) {
        return '<div style=\"background-color:#eee; padding: .5em;\">' + 
                'Project: ' + d[1] + 
                ', Doc: ' + d[2] + 
                ', Sections: ' + d[3] + 
                ', URL: ' + d[4] + '</div>';
      };
      table.on('click', 'td.details-control', function() {
        var td = $(this), row = table.row(td.closest('tr'));
        if (row.child.isShown()) {
          row.child.hide();
          td.html('&oplus;');
        } else {
          row.child(format(row.data())).show();
          td.html('&CircleMinus;');
        }
      });"))
}

# Run the application 
shinyApp(ui = ui, server = server)
