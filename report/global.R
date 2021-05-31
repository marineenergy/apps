message(paste("user:", Sys.info()[["user"]]))
source(here::here("functions.R")) # libraries, db connection object (con)
shelf(
  # shiny
  DT, rintrojs, shinydashboard, shinyEventLogger, shinyWidgets, shinyjs, waiter, yaml, # shinydashboardPlus, nutterb/shinydust,
  # report
  hadley/emo, rhandsontable,
  # spatial
  mapedit)
set_logging(file = "/share/github/apps/logging_tmp.txt")

web_url = "https://marineenergy.app"

map_default <- leaflet(
  options = leafletOptions(
    zoomControl = T,
    attributionControl = F)) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  setView(-93.4, 37.4, 4)

nbsp <- "\u00A0" # Unicode (UTF-8) for non-breaking space

# using "keys" instead of "tags", since tags is a Shiny / HTML thing
# download.file(
#   "https://raw.githubusercontent.com/marineenergy/apps/7a8db9e4e8c17a075f2dd229aa7719c6854b20ec/data/tags.csv",
#   here("data/tags_extra.csv"))
keys <- read_csv(here("data/tags_extra.csv")) %>% 
  select(key_facet = facet, key = item_label, key_parent = tag_parent) %>% 
  mutate(
    key_order = ifelse(
      is.na(key_parent),
      key,
      glue("{key_parent}:{key}")),
    key_spaced = ifelse(
      is.na(key_parent),
      key,
      glue("{nbsp %>% strrep(4)}{key}"))) %>% 
  arrange(key_facet, key_order) # View(keys)

# choices_tech <- keys %>% 
#   filter(key_facet == "technology") %>% 
#   {setNames(.$key, .$key_spaced)}
# dbGetQuery(
#   con,
#   glue("
#         SELECT json_array_elements(data->'technologyType') ->> 0 as tech_text
#         FROM tethys_pubs")) %>%
#   group_by(tech_text) %>%
#   summarize(n = n())
# 4 Wave                591
# 1 Current             873 # Tidal
# 2 OTEC                 57
# 3 Salinity Gradient     8
choices_tech <- c(
  "Marine Energy"         = "Marine Energy",
  "    OTEC"              = "OTEC",
  "    Salinity Gradient" = "Salinity Gradient",
  "    Tidal"             = "Current",
  "    Wave"              = "Wave")
names(choices_tech) <- str_replace(names(choices_tech), "    ", strrep(nbsp, 4))

choices_stressors <- keys %>% 
  filter(key_facet == "stressor") %>% 
  {setNames(.$key, .$key_spaced)}
choices_receptors <- keys %>% 
  filter(key_facet == "receptor") %>% 
  {setNames(.$key, .$key_spaced)}

n_r <- length(choices_receptors)
n_s <- length(choices_stressors)
s_r <- matrix(
  rep(F, n_r * n_s), 
  nrow = n_r, ncol = n_s) %>% 
  as.data.frame() %>% 
  setNames(., choices_stressors) %>%  
  mutate(
    Receptor = names(choices_receptors)) %>% 
  relocate(Receptor, .before = 1)

tagLabel <- function(lbl){
  tag("label", lbl) %>% 
    tagAppendAttributes(class = "control-label")  
}

waiting_screen <- tagList(
  spin_flower(),
  h3("Generating custom report...")
) 

# TODO: pull from db: https://docs.google.com/spreadsheets/d/1MMVqPr39R5gAyZdY2iJIkkIdYqgEBJYQeGqDk1z-RKQ/edit#gid=936111013
# datasets_gsheet2db()

# choices_sp_receptors <- c(
#   "Marine Mammals","Cetaceans", "Fish", 
#   "Marine Spatial Planning", "Social & Economic Data")
choices_sp_receptors <- dbGetQuery(con, "SELECT tags FROM datasets WHERE ready") %>% 
  separate_rows(tags, sep = ";") %>% 
  rename(tag = tags) %>% 
  mutate(
    tag = str_trim(tag)) %>% 
  filter(!is.na(tag), tag != "") %>% 
  distinct(tag) %>% 
  arrange(tag)

# tabdisable_js <- "
# shinyjs.disableTab = function(name) {
#   var tab = $('.nav li a[data-value=' + name + ']');
#   tab.bind('click.tab', function(e) {
#     e.preventDefault();
#     return false;
#   });
#   tab.addClass('disabled');
# }
# 
# shinyjs.enableTab = function(name) {
#   var tab = $('.nav li a[data-value=' + name + ']');
#   tab.unbind('click.tab');
#   tab.removeClass('disabled');
# }
# "
# 
# tabdisable_css <- "
# .nav li a.disabled {
#   background-color: #aaa !important;
#   color: #333 !important;
#   cursor: not-allowed !important;
#   border-color: #aaa !important;
# }"