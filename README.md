# meta-app

Main Shiny app: [shiny.ecoquants.com/meta](https://shiny.ecoquants.com/meta)

This README with html listing: [noaa-iea.github.io/meta-app](https://noaa-iea.github.io/meta-app)

## html

These web pages (\*.html) are typically rendered from Rmarkdown (\*.Rmd):

<!-- Jekyll rendering -->
{% for file in site.static_files %}
  {% if file.extname == '.html' %}
* [{{ file.basename }}]({{ site.baseurl }}{{ file.path }})
  {% endif %}
{% endfor %}
