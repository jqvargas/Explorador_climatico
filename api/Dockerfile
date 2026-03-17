FROM rstudio/plumber
RUN apt-get update && apt-get install -y libpq-dev && rm -rf /var/lib/apt/lists/*
RUN R -e "install.packages(c('DBI', 'RPostgres', 'glue', 'jsonlite', 'uuid', 'dotenv', 'redux'), repos='https://cloud.r-project.org')"
WORKDIR /app
COPY . /app
EXPOSE 8000
ENTRYPOINT []
CMD ["Rscript", "/app/plumber.R"]