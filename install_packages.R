# install_packages.R
# Ejecutar una vez antes de lanzar la app

pkgs <- c(
  "shiny",
  "shinyFiles",
  "shinyjs",
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "lubridate",
  "DT",
  "shinycssloaders",
  "bslib",
  "htmltools"
)

nuevos <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nuevos)) {
  install.packages(nuevos, dependencies = TRUE)
} else {
  message("✅ Todos los paquetes ya están instalados.")
}
