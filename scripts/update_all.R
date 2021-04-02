if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(
  dplyr, 
  DT, 
  here, 
  jsonlite, 
  purrr, 
  rvest, 
  tibble, 
  tidyjson)

source(here("functions.R")) # connection object

# datasets_gsheet2db()

# update db tables tethys_pubs, tethys_pub_tags; data/tethys_docs.[json|csv]
update_tethys_docs()

# update data/tethys_mgt.csv
update_tethys_mgt()

# update data/tethys_tags.csv
update_tethys_tags()

# update data/tethys_intxns.csv
update_tethys_intxns()

# update data/project_sites.csv
update_project_sites()

# update data/project_permits.csv, data/project_times.csv
update_project_timelines()

# dev cron for updating: tethys_docs, tethys_mgt, tethys_tags, tethys_intxns, ferc_docs

# TODO: rmarkdown::render(mgt.Rmd, env.Rmd), 

# TODO: git commit & push
