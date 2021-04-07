---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: home
---

These web pages (\*.html) are typically rendered from Rmarkdown (\*.Rmd):

{% for file in site.static_files %}
  {% if file.extname == '.html' %}
* [{{ file.path }}]({{ site.baseurl }}{{ file.path }})
  {% endif %}
{% endfor %}
