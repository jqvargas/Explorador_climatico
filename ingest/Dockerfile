# Imagen dedicada para ingesta MeteoVision (scraping DGA/DMC + persistencia).
# Las dependencias quedan "horneadas" en build-time para evitar instalaciones
# lentas/fallidas en runtime. rocker/r-ver usa Posit Package Manager (binarios).
FROM rocker/r-ver:4.3.0

# Librerias de sistema:
#  - libpq-dev / libhiredis-dev: BD (RPostgres) y Redis (redux)
#  - libxml2-dev / libcurl4-openssl-dev / libssl-dev: scraping (xml2, httr, rvest)
RUN apt-get update && apt-get install -y --no-install-recommends \
        libpq-dev \
        libhiredis-dev \
        libxml2-dev \
        libcurl4-openssl-dev \
        libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Paquetes R de ingesta (BD + scraping + utilidades).
RUN R -e "install.packages(c( \
        'DBI', 'RPostgres', 'redux', 'jsonlite', 'glue', 'dotenv', \
        'dplyr', 'tidyr', 'lubridate', \
        'httr', 'rvest', 'xml2' \
    ), repos = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest')" \
    && R -e "stopifnot(all(c('httr','rvest','xml2','RPostgres','redux','dplyr','tidyr','lubridate') %in% rownames(installed.packages())))"

WORKDIR /app

# El codigo se monta como volumen (./:/app) en docker-compose, de modo que los
# scripts de ingest/ se editan en host sin reconstruir la imagen.
CMD ["Rscript", "--version"]
