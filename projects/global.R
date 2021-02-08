if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  dplyr, glue, here, htmltools, leaflet, plotly, RColorBrewer, readr)

prj_sites_csv   <- here("data/project_sites.csv")
prj_times_csv   <- here("data/project_times.csv")
prj_permits_csv <- here("data/project_permits.csv")

prj_sites <- read_csv(prj_sites_csv) 
prj_sites$label_html <- prj_sites$label_html %>% lapply(HTML)
prj_sites$popup_html <- prj_sites$popup_html %>% lapply(HTML)

d_times   <- read_csv(prj_times_csv) 
d_permits <- read_csv(prj_permits_csv) 
