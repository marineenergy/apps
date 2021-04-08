# apps

Main Shiny app: https://shiny.marineenergy.app/report

This README with html listing: https://marineenergy.github.io/apps

## html

These web pages (\*.html) are typically rendered from Rmarkdown (\*.Rmd):

<!-- Jekyll rendering: https://marineenergy.github.io/apps/ -->
{% for file in site.static_files %}
  {% if file.extname == '.html' %}
* [{{ file.basename }}]({{ site.baseurl }}{{ file.path }})
  {% endif %}
{% endfor %}

## development

Normally, on command line, get this repo:

```bash
git clone https://github.com/mareineenergy/apps
```

Or in Rstudio, File -> New Project... -> Version Control -> git ...

Might need to install [Git](https://git-scm.com/).

## todo

To get Rmd to show, eg https://mhk-env.github.io/shiny-apps/ply2rwhale.html, had to delete sym linked index.html that pointed to `/opt/shiny-server/samples/welcome.html`.