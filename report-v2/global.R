# libraries ----
source(here::here("scripts/common.R"))
source(file.path(dir_scripts, "db.R"))
source(file.path(dir_scripts, "shiny_report.R"))

library(shiny)
librarian::shelf(
  DT, googleAuthR, htmltools, httr, jsonlite, leaflet, mapedit, plotly, purrr,
  shinydashboard, shinydashboardPlus, shinyjs, shinyWidgets)

# navbar ----
dashboardHeader <- function(
  ..., title = NULL, titleWidth = NULL, 
  disable = FALSE, .list = NULL, leftUi = NULL,
  controlbarIcon = shiny::icon("gears"), fixed = FALSE) {
  
  # handle right menu items
  items <- c(list(...), .list)
  lapply(items, shinydashboardPlus:::tagAssert, type = "li", class = "dropdown")
  
  # handle left menu items
  if (!is.null(leftUi)) {
    left_menu_items <- leftUi
    # left_menu_items <- lapply(seq_along(leftUi), FUN = function(i) {
    #   left_menu_item <- leftUi[[i]]
    #   name <- left_menu_item$name
    #   class <- left_menu_item$attribs$class
    #   
    #   # if the left menu item is not a li tag and does not have
    #   # the dropdown class, create a wrapper to make it work
    #   if (name != "li" || !is.null(class) || class != "dropdown") {
    #     dropdownTag <- shiny::tags$li(class = "dropdown")
    #     left_menu_item <- shiny::tagAppendChild(dropdownTag, left_menu_item)
    #     # add some custom css to make it nicer
    #     left_menu_item <- shiny::tagAppendAttributes(
    #       left_menu_item,
    #       style = "margin-top: 7.5px; margin-left: 5px; margin-right: 5px;"
    #     )
    #   } else {
    #     left_menu_item
    #   }
    # })
    # when left_menu is null, left_menu_items are also NULL 
  } else {
    left_menu_items <- leftUi
  }
  
  titleWidth <- shiny::validateCssUnit(titleWidth)
  
  # Set up custom CSS for custom width.
  custom_css <- NULL
  if (!is.null(titleWidth)) {
    # This CSS is derived from the header-related instances of '230px' (the
    # default sidebar width) from inst/AdminLTE/AdminLTE.css. One change is that
    # instead making changes to the global settings, we've put them in a media
    # query (min-width: 768px), so that it won't override other media queries
    # (like max-width: 767px) that work for narrower screens.
    custom_css <- shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          gsub(
            "_WIDTH_", 
            titleWidth, 
            fixed = TRUE, 
            '@media (min-width: 768px) {
              .main-header > .navbar {
                margin-left: _WIDTH_;
              }
              .main-header .logo {
                width: _WIDTH_;
              }
             }
              '
          )
        )
      )
    )
  }
  
  shiny::tags$header(
    class = "main-header",
    custom_css,
    style = if (disable) "display: none;",
    # only hide on small screen devices when title is NULL
    shiny::tags$span(class = if (is.null(title)) "logo hidden-xs" else "logo", title),
    shiny::tags$nav(
      class = paste0("navbar navbar-", if (fixed) "fixed" else "static", "-top"), 
      role = "navigation",
      # Embed hidden icon so that we get the font-awesome dependency
      shiny::tags$span(shiny::icon("bars"), style = "display:none;"),
      # Sidebar toggle button
      shiny::tags$a(
        href = "#", 
        class = "sidebar-toggle", 
        `data-toggle` = "offcanvas",
        role = "button",
        shiny::tags$span(class = "sr-only", "Toggle navigation")
      ),
      # left menu
      shiny::tags$div(
        class = "navbar-custom-menu",
        style = "float: left; margin-left: 10px;",
        shiny::tags$ul(
          class = "nav navbar-nav",
          left_menu_items
        )
      ),
      # right menu
      shiny::tags$div(
        class = "navbar-custom-menu",
        shiny::tags$ul(
          class = "nav navbar-nav",
          items,
          # right sidebar
          shiny::tags$li(
            shiny::tags$a(
              href = "#", 
              `data-toggle` = "control-sidebar", 
              controlbarIcon
            )
          )
        )
      )
    )
  )
}

# trick: [How to Show Tabpanels in bs4navbar() ? · Issue #108 · RinteRface/bs4Dash](https://github.com/RinteRface/bs4Dash/issues/108)

navbarTab <- function(tabName, ..., icon = NULL) {
  tags$li(
    class = "nav-item",
    tags$a(
      class = "nav-link",
      id = paste0("tab-", tabName),
      href = paste0("#shiny-tab-", tabName),
      `data-toggle` = "tab",
      `data-value` = tabName,
      icon,
      tags$p(...)))
}


navbarMenu <- function(..., id = NULL) {
  if (is.null(id))
    id <- paste0("tabs_", round(stats::runif(1, min = 0, max = 1e9)))
  
  tags$ul(
    class = "navbar-nav dropdown sidebar-menu", 
    role = "menu",
    ...,
    div(
      id = id,
      class = "sidebarMenuSelectedTabItem",
      `data-value` = "null"))
}


# tag_choices ----
df_tags <- tbl(con, "tags") %>% 
  collect() %>% 
  filter(tag != category) %>% 
  mutate(
    tag = map2_chr(tag, category, function(tag, category){
      stringr::str_replace(tag, glue("{category}/"), "")}),
    tag_named = map2(tag_sql, tag, setNames))

tag_choices = list()
for (cat in unique(df_tags$category)){ # (cat = df_tags$category[1])
  tag_choices <- append(
    tag_choices,
    setNames(
      list(
        df_tags %>% 
          filter(category == cat) %>% 
          pull(tag_named) %>% 
          unlist()),
      cat))
}


# googleAuthR ----

# * [Client ID for Web application – APIs & Services – iea-uploader – Google API Console](https://console.developers.google.com/apis/credentials/oauthclient/596429062120-lsgv9f8pbas11o7ub9b9cm0earf1dpu5.apps.googleusercontent.com?authuser=3&project=iea-uploader)
options(googleAuthR.webapp.client_id = "596429062120-lsgv9f8pbas11o7ub9b9cm0earf1dpu5.apps.googleusercontent.com")

# googleSignInUI() original definition:
#   https://github.com/MarkEdmondson1234/googleAuthR/blob/73221f3cf5a561cbb17fe04943ce85e2130204ff/R/shiny-js-signin.R#L16-L37
#   broke up into 3 bits for placement: *_head, *_btn_signin, *_btn_signout
googleSignInUI_head <- function(id, logout_name = "Sign Out", logout_class = "btn-danger"){
  
  ns <- shiny::NS(id)
  
  tagList(
    tags$head(
      tags$meta(name="google-signin-scope", content="profile email"),
      tags$meta(name="google-signin-client_id", content=getOption("googleAuthR.webapp.client_id")),
      HTML('<script src="https://apis.google.com/js/platform.js?onload=init"></script>')),
    googleAuthR:::load_js_template(
      "js/signin-top.js", 
      ns("signin"), ns("signout") ,ns("g_id"), ns("g_name"), ns("g_image"), ns("g_email")),
    googleAuthR:::load_js_template(
      "js/signin-bottom.js",
      ns("g_id"), ns("g_name"), ns("g_image"), ns("g_email"), ns("signed_in")))
}

googleSignInUI_btn_signin <- function(id, logout_name = "Sign Out", logout_class = "btn-danger"){
  ns <- shiny::NS(id)
  
  shiny::tagList(
    div(id=ns("signin"), class="g-signin2", "data-onsuccess"="onSignIn"))
}

googleSignInUI_btn_signout <- function(id, logout_name = "Sign Out", logout_class = "btn-danger"){
  ns <- shiny::NS(id)
  
  shiny::tagList(
    tags$button(id = ns("signout"), logout_name, onclick = "signOut();", class = logout_class))
}


# map_edit ----

map_edit <- leaflet(
  options = leafletOptions(
    zoomControl = T,
    attributionControl = F)) %>% 
  addProviderTiles(providers$Esri.OceanBasemap) %>% 
  setView(-93.4, 37.4, 4)

# projects ----
# p_csvs <- list.files("/share/github/apps/data", "project_.*")
# file.copy(file.path("/share/github/apps/data", p_csvs), file.path("/share/github/apps_dev/data", p_csvs), overwrite = T)
prj_sites_csv        <- here("data/project_sites.csv")
prj_times_csv        <- here("data/project_times.csv")
prj_permits_csv      <- here("data/project_permits.csv")
prj_permit_types_csv <- here("data/project_permit_types.csv")

# TODO: load prj_*  into db, read from db
prj_sites <- readr::read_csv(prj_sites_csv, col_types = readr::cols()) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F)

d_times <- readr::read_csv(prj_times_csv, col_types = readr::cols())  %>% 
  arrange(technology_type, project) %>% 
  mutate(
    technology_type = factor(technology_type))
d_times <- d_times %>% 
  mutate(
    # order projects by technology_type, then project
    project = factor(project, levels = d_times$project))
# levels(d_times$project) # Igiugig,...,Yakutat

d_permits <- readr::read_csv(prj_permits_csv, col_types = readr::cols()) %>% 
  left_join(
    d_times %>% 
      select(project, technology_type), 
    by = "project") %>% 
  arrange(technology_type, project)
d_permits <- d_permits %>% 
  mutate(
    # order projects by technology_type, then project
    project = factor(project, levels = distinct(d_permits, project) %>% pull(project)))
# levels(d_permits$project) # Igiugig,...,Yakutat

# order permit_types
permit_types <- readr::read_csv(prj_permit_types_csv, col_types = readr::cols()) %>% 
  pull(permit_types)
permit_types <- permit_types %>% 
  intersect(d_permits$permit_type)
d_permits <- d_permits %>% 
  mutate(
    # order by permit_types
    permit_type = factor(permit_type, levels = permit_types))

prj_sites$label_html <- prj_sites$label_html %>% lapply(HTML)
prj_sites$popup_html <- prj_sites$popup_html %>% lapply(HTML)

# prj_sites %>%
#   filter(project == "WETS") %>%
#   pull(popup_md)


# colors & symbols
project_statuses <- unique(d_times$project_status)
cols_type  <- colorRampPalette(RColorBrewer::brewer.pal(n=11, name = 'PiYG'))(length(permit_types))
cols_status <- c("#30A4E1", "#999999") # Active/Inactive Projects
cols <- setNames(
  c(cols_type, cols_status), 
  c(permit_types, project_statuses))
syms_type  <- c(rep('triangle-up', 3), 'triangle-down', 'triangle-up', 'triangle-down', 'triangle-up', 'triangle-down', rep('triangle-up', 3))
syms_status <- rep(NA, 2)
syms <- setNames(
  c(syms_type, syms_status), 
  c(permit_types, project_statuses))

# technology_type numbers for horizontal line and label placement along y axis
n_tech <- d_times %>% 
  group_by(technology_type) %>% 
  summarize(
    n = n())
n_riv <- n_tech %>% filter(technology_type == "Riverine Energy") %>% pull(n)
n_tid <- n_tech %>% filter(technology_type == "Tidal Energy")    %>% pull(n)
n_wav <- n_tech %>% filter(technology_type == "Wave Energy")     %>% pull(n)

# management ----
mgt_csv <- "/share/github/apps/data/tethys_mgt.csv"

d_mgt <- tbl(con, "tethys_mgt") %>% 
  rename(
    Category = `Management Measure Category`,
    Phase    = `Phase of Project`)

d_mgt_n <- d_mgt %>% summarize(n = n()) %>% pull(n)

# reports ----
dir_rpt_pfx <- "/share/user_reports"
url_rpt_pfx <- "https://api.marineenergy.app/report"

rpts_0 <- tibble(
  title    = character(),
  date     = character(), 
  status   = character(), 
  contents = character(), 
  n_ixns   = double(), 
  url      = character())

get_user_reports <- function(email){
  # email = "bdbest@gmail.com"
  if (is.null(email))
    return(rpts_0)
  r <- httr::GET(
    "https://api.marineenergy.app/user_reports", 
    query = list(email=email))
  # r$status
  httr::content(r, col_types=readr::cols())
}

get_user_reports_last_modified <- function(email){
  # email = "bdbest@gmail.com"
  if (is.null(email))
    return("")
  httr::GET(
    "https://api.marineenergy.app/user_reports_last_modified", 
    query = list(email=email)) %>% 
    httr::content()
}

del_user_report <- function(email, rpt){
  
  if (is.null(email) | is.null(rpt))
    return("")
  
  # email = "bdbest@gmail.com"; rpt = "report_cef7d716.docx"
  pw  <- readLines("/share/.password_mhk-env.us")
  tkn <- digest::digest(c(rpt, pw), algo="crc32")
  
  r <- httr::GET(
    "https://api.marineenergy.app/delete_report", 
    query = list(email=email, report=rpt, token=tkn))
  # TODO: handle error
  #   if (r$status_code == 500)...
  httr::content(r)
}
