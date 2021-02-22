if (!require(librarian)){
  install.packages("librarian")
  library(librarian)
}
shelf(crosstalk, dplyr, DT, here, jsonlite, listviewer, purrr, timelyportfolio/reactR, readr, rvest, tibble, tidyjson)

source("functions.R") # connection object

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
update_project_timelines

# dev cron for updating: tethys_docs, tethys_mgt, tethys_tags, tethys_intxns, ferc_docs

# TODO: rmarkdown::render(mgt.Rmd, env.Rmd), 

# TODO: git commit & push
