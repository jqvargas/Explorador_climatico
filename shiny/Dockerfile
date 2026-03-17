FROM rocker/shiny:4.2

# Use install2.r -e so build FAILS if any package does not install (no silent failures)
RUN install2.r -e leaflet httr jsonlite plotly DT bslib shinycssloaders glue dotenv shinydashboard fresh shinyWidgets shinythemes

# Verify leaflet loads (catches cached layers where install was skipped)
RUN R -e "if (!requireNamespace('leaflet', quietly=TRUE)) stop('leaflet not installed')"

COPY . /srv/shiny-server/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]

