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

source(here("scripts/common.R")) # connection object
source(here("scripts/db.R")) # connection object
source(here("scripts/update.R")) # connection object

# update db tables tethys_pubs, tethys_pub_tags; data/tethys_docs.[json|csv]
update_tethys_pubs()

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

# update ferc docs
update_ferc_docs()

# update spatial
update_spatial()

# dev cron for updating: tethys_docs, tethys_mgt, tethys_tags, tethys_intxns, ferc_docs

# TODO: rmarkdown::render(mgt.Rmd, env.Rmd), 

# TODO: git commit & push
