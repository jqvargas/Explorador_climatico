FROM rocker/r-ver:4.3.0

RUN apt-get update && apt-get install -y \
    libpq-dev libhiredis-dev libcurl4-openssl-dev libxml2-dev libssl-dev libuv1-dev \
    zip qpdf wget ca-certificates xz-utils \
    fonts-dejavu-core fonts-liberation \
    && rm -rf /var/lib/apt/lists/*

# pandoc 3.x (typst output)
RUN wget -q "https://github.com/jgm/pandoc/releases/download/3.6.4/pandoc-3.6.4-linux-amd64.tar.gz" -O /tmp/pandoc.tar.gz \
    && tar -xzf /tmp/pandoc.tar.gz -C /tmp \
    && mv /tmp/pandoc-3.6.4/bin/pandoc /usr/local/bin/pandoc \
    && rm -rf /tmp/pandoc.tar.gz /tmp/pandoc-3.6.4

# typst CLI (Linux x86_64)
RUN wget -q "https://github.com/typst/typst/releases/download/v0.12.0/typst-x86_64-unknown-linux-musl.tar.xz" -O /tmp/typst.tar.xz \
    && tar -xf /tmp/typst.tar.xz -C /tmp \
    && mv /tmp/typst-x86_64-unknown-linux-musl/typst /usr/local/bin/typst \
    && chmod +x /usr/local/bin/typst \
    && rm -rf /tmp/typst.tar.xz /tmp/typst-x86_64-unknown-linux-musl

# ponytail: R_COMPILE_PKGS=0 desactiva el byte-compile (se colgaba por RAM al compilar
# el metapaquete paws en este host de 7.5GB). El stopifnot hace que el build FALLE si
# alguna dep critica (p.ej. emayili) no quedo instalada, en vez de fallar silenciosamente.
RUN R_COMPILE_PKGS=0 R -e "install.packages(c('DBI', 'RPostgres', 'redux', 'jsonlite', 'glue', 'paws', 'emayili', 'dotenv', 'httr', 'Kendall', 'zyp', 'ggplot2', 'SPEI'), repos='https://cloud.r-project.org'); stopifnot(all(c('DBI','RPostgres','redux','jsonlite','glue','paws','emayili','dotenv','httr','Kendall','zyp','ggplot2','SPEI') %in% rownames(installed.packages())))"

# Web Push. Node queda fuera de /app: ese path es un volumen y pisaria node_modules.
RUN wget -q "https://nodejs.org/dist/v20.18.1/node-v20.18.1-linux-x64.tar.xz" -O /tmp/node.tar.xz \
    && tar -xJf /tmp/node.tar.xz -C /usr/local --strip-components=1 \
    && rm /tmp/node.tar.xz \
    && mkdir -p /opt/rodaja-push \
    && npm install --prefix /opt/rodaja-push web-push@3.6.7 --omit=dev

# sf (geometria del Reporte personalizado): GDAL, GEOS, PROJ y udunits del sistema.
RUN apt-get update && apt-get install -y libgdal-dev libgeos-dev libproj-dev libudunits2-dev cmake \
    && rm -rf /var/lib/apt/lists/*
RUN R_COMPILE_PKGS=0 R -e "install.packages('sf', repos='https://cloud.r-project.org'); stopifnot(requireNamespace('sf', quietly=TRUE))"

WORKDIR /app
COPY . /app
CMD ["Rscript", "worker.R"]
