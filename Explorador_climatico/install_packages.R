# Install R packages required for migration.R
# Run: Rscript install_packages.R

pkgs <- c("DBI", "RPostgres", "glue", "cli", "dotenv", "jsonlite", "plumber", "uuid", "redux")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}
message("All packages installed.")
