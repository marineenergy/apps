source(here::here("functions.R")) # libraries, db connection object (con)
shelf(
  # shiny
  rintrojs, shinydashboard, shinyEventLogger, shinyWidgets, shinyjs, waiter, yaml, # shinydashboardPlus, nutterb/shinydust,
  # report
  hadley/emo, rhandsontable,
  # spatial
  mapedit)
set_logging(file = "/share/github/mhk-env_shiny-apps/logging.txt")

map_default <- leaflet(
  options = leafletOptions(
    zoomControl = T,
    attributionControl = F)) %>%
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  setView(-93.4, 37.4, 4)

nbsp <- "\u00A0" # Unicode (UTF-8) for non-breaking space

# using keys instead of tags, since tags is a Shiny / HTML thing
keys <- read_csv(here("data/tags.csv")) %>% 
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

choices_tech <- keys %>% 
  filter(key_facet == "technology") %>% 
  {setNames(.$key, .$key_spaced)}
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
