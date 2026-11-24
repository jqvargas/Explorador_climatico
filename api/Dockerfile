FROM rstudio/plumber
RUN apt-get update && apt-get install -y libpq-dev libhiredis-dev curl libuv1-dev && rm -rf /var/lib/apt/lists/*
RUN R -e "install.packages(c('DBI', 'RPostgres', 'pool', 'glue', 'jsonlite', 'uuid', 'dotenv', 'redux', 'httr', 'base64enc', 'digest', 'openssl', 'jose', 'emayili'), repos='https://cloud.r-project.org')"
# sf (geometria del Reporte personalizado): GDAL, GEOS, PROJ y udunits del sistema.
RUN apt-get update && apt-get install -y libgdal-dev libgeos-dev libproj-dev libudunits2-dev cmake && rm -rf /var/lib/apt/lists/*
RUN R -e "install.packages('sf', repos='https://cloud.r-project.org'); stopifnot(requireNamespace('sf', quietly=TRUE))"
WORKDIR /app
COPY . /app
EXPOSE 8000
ENTRYPOINT []
CMD ["Rscript", "/app/plumber.R"]