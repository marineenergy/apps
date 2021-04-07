## index of rendered html files

{% for file in site.static_files %}
  {% if file.extname == '.html' %}
* [{{ file.path }}]({{ site.baseurl }}{{ file.path }})
  {% endif %}
{% endfor %}
