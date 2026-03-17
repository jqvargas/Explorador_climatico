FROM rocker/r-ver:4.3.0
RUN apt-get update && apt-get install -y libpq-dev libhiredis-dev && rm -rf /var/lib/apt/lists/*
RUN R -e "install.packages(c('DBI', 'RPostgres', 'redux', 'jsonlite', 'glue', 'paws', 'emayili', 'dotenv'), repos='https://cloud.r-project.org')"
WORKDIR /app
COPY . /app
CMD ["Rscript", "worker.R"]

