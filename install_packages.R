# install_packages.R
# Ejecutar UNA VEZ antes de lanzar la app (o en la consola de shinyapps.io)
#
# CORRECCIONES:
#   [FIX-PKG-1] shinymanager ELIMINADO — app.R lo removió; instalarlo igual
#               provoca conflictos con session$setBookmarkExclude() en Shiny moderno.
#   [FIX-PKG-2] Se agregó "digest" que faltaba y es requerido por supabase_utils.R
#               y app.R (credentials_supabase).

pkgs <- c(
  "shiny",
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "lubridate",
  "DT",
  "shinycssloaders",
  "bslib",
  "htmltools",
  "httr2",
  "jsonlite",
  "ggplot2",
  "plotly",
  "scales",
  "RColorBrewer",
  "digest"        # [FIX-PKG-2] requerido por supabase_utils.R y app.R
  # shinymanager REMOVIDO [FIX-PKG-1]
  # shinyFiles y shinyjs no se usan → no se instalan
)

nuevos <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nuevos)) {
  message("⬇  Instalando: ", paste(nuevos, collapse = ", "))
  install.packages(nuevos, dependencies = TRUE)
} else {
  message("✅ Todos los paquetes ya están instalados.")
}
