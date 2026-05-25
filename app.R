# =============================================================================
# app.R — Compilador de Caudales Medios Diarios DGA
# Shiny App | DGA Chile — Recursos Hídricos
# =============================================================================
# Dependencias: shiny, shinyFiles, readxl, dplyr, tidyr, stringr, lubridate,
#               DT, shinycssloaders, bslib
# =============================================================================

library(shiny)
library(shinyFiles)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(DT)
library(shinycssloaders)
library(bslib)

source("leer_dga_caudales.R")

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────
ui <- page_fluid(
  theme = bs_theme(
    version   = 5,
    bootswatch = "flatly",
    primary   = "#2c7fb8",
    base_font = font_google("Inter")
  ),

  tags$head(
    tags$style(HTML("
      .card-header-dga {
        background-color: #2c7fb8;
        color: white;
        font-weight: 600;
        padding: 0.75rem 1.25rem;
        border-radius: 0.375rem 0.375rem 0 0;
      }
      .stat-box {
        background: #f0f7ff;
        border-left: 4px solid #2c7fb8;
        padding: 12px 16px;
        border-radius: 4px;
        margin-bottom: 10px;
      }
      .stat-box .stat-value { font-size: 1.6rem; font-weight: 700; color: #2c7fb8; }
      .stat-box .stat-label { font-size: 0.8rem; color: #555; text-transform: uppercase; }
      .log-box {
        background: #1e1e1e; color: #d4d4d4;
        font-family: monospace; font-size: 0.78rem;
        padding: 10px; border-radius: 4px;
        max-height: 180px; overflow-y: auto;
      }
      .badge-ok  { background-color: #27ae60; }
      .badge-err { background-color: #e74c3c; }
    "))
  ),

  # ── Encabezado ──────────────────────────────────────────────────────────────
  div(class = "py-3 mb-4",
    style = "border-bottom: 2px solid #2c7fb8;",
    fluidRow(
      column(8,
        h3(tags$b("Compilador de Caudales Medios Diarios — DGA Chile"),
           style = "margin: 0; color: #2c7fb8;"),
        p("Procesamiento masivo de archivos XLS de la Dirección General de Aguas",
          style = "margin: 0; color: #666; font-size: 0.9rem;")
      ),
      column(4, class = "text-end",
        tags$img(
          src   = "https://www.dga.cl/wp-content/uploads/2019/09/logo_DGA.png",
          height = "50px",
          style = "opacity: 0.8;"
        )
      )
    )
  ),

  # ── Panel principal ─────────────────────────────────────────────────────────
  fluidRow(

    # ── Columna izquierda: controles ─────────────────────────────────────────
    column(3,
      card(
        card_header(
          div(class = "card-header-dga", "⚙️  Configuración")
        ),
        card_body(
          # Carpeta de entrada
          h6("📂 Carpeta de entrada (XLS)", class = "fw-bold mt-2"),
          p("Selecciona la carpeta que contiene los archivos XLS de caudales DGA.",
            style = "font-size:0.82rem; color:#666;"),
          shinyDirButton("dir_input", "Seleccionar carpeta entrada",
                         title = "Selecciona carpeta con archivos XLS DGA",
                         class = "btn btn-outline-primary btn-sm w-100"),
          verbatimTextOutput("txt_dir_input", placeholder = TRUE),

          hr(),

          # Carpeta de salida
          h6("💾 Carpeta de salida (CSV)", class = "fw-bold"),
          p("Carpeta donde se guardará el CSV compilado.",
            style = "font-size:0.82rem; color:#666;"),
          shinyDirButton("dir_output", "Seleccionar carpeta salida",
                         title = "Selecciona carpeta de destino del CSV",
                         class = "btn btn-outline-secondary btn-sm w-100"),
          verbatimTextOutput("txt_dir_output", placeholder = TRUE),

          hr(),

          # Nombre del archivo de salida
          h6("📄 Nombre del archivo CSV", class = "fw-bold"),
          textInput("nombre_csv",
                    label = NULL,
                    value = paste0("caudales_compilado_", format(Sys.Date(), "%Y%m%d"), ".csv"),
                    placeholder = "nombre_salida.csv"),

          hr(),

          # Opciones
          h6("🔧 Opciones", class = "fw-bold"),
          checkboxInput("solo_validos", "Excluir registros sin valor numérico", value = TRUE),
          checkboxInput("incluir_indicador", "Incluir columna Indicador (*, <, >)", value = TRUE),

          hr(),

          # Botón procesar
          actionButton("btn_procesar",
                       label = tags$span(icon("play"), " Procesar archivos"),
                       class = "btn btn-primary w-100 fw-bold",
                       disabled = TRUE),

          # Botón guardar
          br(), br(),
          actionButton("btn_guardar",
                       label = tags$span(icon("download"), " Guardar CSV"),
                       class = "btn btn-success w-100 fw-bold",
                       disabled = TRUE),

          br(), br(),
          downloadButton("btn_download",
                         label = "⬇  Descargar CSV",
                         class = "btn btn-outline-success w-100")
        )
      )
    ),

    # ── Columna derecha: resultados ──────────────────────────────────────────
    column(9,

      # Estadísticas resumen
      uiOutput("ui_stats"),

      # Log de procesamiento
      card(
        card_header(
          div(class = "card-header-dga", "📋 Log de procesamiento")
        ),
        card_body(padding = "0.5rem",
          div(class = "log-box", id = "log_container",
            uiOutput("ui_log")
          )
        )
      ),

      br(),

      # Tabla de datos
      card(
        card_header(
          div(class = "card-header-dga",
            fluidRow(
              column(8, "📊 Datos compilados"),
              column(4, class = "text-end",
                uiOutput("ui_badge_registros")
              )
            )
          )
        ),
        card_body(padding = "0",
          withSpinner(
            DTOutput("tabla_datos"),
            type = 6, color = "#2c7fb8"
          )
        )
      )
    )
  )
)

# ─────────────────────────────────────────────────────────────────────────────
# Server
# ─────────────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Raíz del sistema de archivos accesible
  roots <- c(
    Home      = path.expand("~"),
    Raiz      = "/",
    Escritorio = path.expand("~/Desktop")
  )

  # ── Selectores de carpeta ──────────────────────────────────────────────────
  shinyDirChoose(input, "dir_input",  roots = roots, filetypes = c("xls", "xlsx"))
  shinyDirChoose(input, "dir_output", roots = roots)

  ruta_input  <- reactive({
    req(input$dir_input)
    tryCatch(parseDirPath(roots, input$dir_input), error = function(e) NULL)
  })

  ruta_output <- reactive({
    req(input$dir_output)
    tryCatch(parseDirPath(roots, input$dir_output), error = function(e) NULL)
  })

  output$txt_dir_input <- renderText({
    p <- ruta_input()
    if (is.null(p) || length(p) == 0) "— no seleccionada —" else p
  })

  output$txt_dir_output <- renderText({
    p <- ruta_output()
    if (is.null(p) || length(p) == 0) "— no seleccionada —" else p
  })

  # Habilitar botón procesar cuando hay carpeta de entrada
  observe({
    tiene_input <- !is.null(ruta_input()) && length(ruta_input()) > 0
    shinyjs::toggleState("btn_procesar", condition = tiene_input)
    updateActionButton(session, "btn_procesar",
                       disabled = !tiene_input)
  })

  # ── Estado reactivo ────────────────────────────────────────────────────────
  datos_compilados <- reactiveVal(NULL)
  log_mensajes     <- reactiveVal(character(0))

  agregar_log <- function(msg, tipo = "info") {
    color <- switch(tipo,
      "ok"    = "#4ec9b0",
      "error" = "#f44747",
      "warn"  = "#dcdcaa",
      "#d4d4d4"
    )
    timestamp <- format(Sys.time(), "%H:%M:%S")
    html_msg  <- sprintf(
      '<span style="color:#858585;">[%s]</span> <span style="color:%s;">%s</span>',
      timestamp, color, htmltools::htmlEscape(msg)
    )
    log_mensajes(c(log_mensajes(), html_msg))
  }

  # ── Procesamiento principal ────────────────────────────────────────────────
  observeEvent(input$btn_procesar, {

    carpeta <- ruta_input()
    req(carpeta, length(carpeta) > 0)

    # Limpiar estado previo
    datos_compilados(NULL)
    log_mensajes(character(0))

    # Buscar archivos XLS/XLSX
    archivos <- list.files(
      path       = carpeta,
      pattern    = "\\.(xls|xlsx)$",
      full.names = TRUE,
      recursive  = FALSE,
      ignore.case = TRUE
    )

    if (length(archivos) == 0) {
      agregar_log("❌ No se encontraron archivos XLS/XLSX en la carpeta seleccionada.", "error")
      return()
    }

    agregar_log(sprintf("📂 Carpeta: %s", carpeta))
    agregar_log(sprintf("📁 Archivos encontrados: %d", length(archivos)))

    # Progreso
    withProgress(message = "Procesando archivos DGA...", value = 0, {

      todos <- list()
      errores <- 0

      for (i in seq_along(archivos)) {
        archivo <- archivos[i]
        nombre  <- basename(archivo)

        incProgress(1 / length(archivos),
                    detail = sprintf("(%d/%d) %s", i, length(archivos), nombre))

        agregar_log(sprintf("  ▶ [%d/%d] %s", i, length(archivos), nombre))

        resultado <- tryCatch({
          df <- leer_dga_caudales(
            file   = archivo,
            log_fn = function(m) agregar_log(paste("    ", m), "warn")
          )
          df$ArchivoFuente <- nombre
          df
        }, error = function(e) {
          agregar_log(sprintf("    ✗ Error: %s", e$message), "error")
          errores <<- errores + 1
          NULL
        })

        if (!is.null(resultado) && nrow(resultado) > 0) {
          todos[[i]] <- resultado
          n_est <- n_distinct(resultado$Estacion)
          agregar_log(
            sprintf("    ✓ %d registros | %d estación(es)", nrow(resultado), n_est),
            "ok"
          )
        }
      }

      # Compilar
      if (length(todos) > 0) {
        compilado <- bind_rows(todos) %>% arrange(Estacion, Fecha)

        # Aplicar filtros opcionales
        if (input$solo_validos) {
          compilado <- compilado %>% filter(!is.na(Valor))
        }
        if (!input$incluir_indicador) {
          compilado <- compilado %>% select(-Indicador)
        }

        datos_compilados(compilado)

        agregar_log(
          sprintf("✅ Compilación exitosa: %s registros | %s estaciones | %s archivos procesados",
                  format(nrow(compilado), big.mark = "."),
                  n_distinct(compilado$Estacion),
                  length(todos)),
          "ok"
        )
        if (errores > 0)
          agregar_log(sprintf("⚠ %d archivo(s) con errores.", errores), "warn")

        # Habilitar botones de descarga/guardado
        updateActionButton(session, "btn_guardar", disabled = FALSE)

      } else {
        agregar_log("❌ No se pudo extraer ningún dato.", "error")
      }
    })
  })

  # ── Guardar CSV en carpeta de salida ───────────────────────────────────────
  observeEvent(input$btn_guardar, {
    df <- datos_compilados()
    req(df, nrow(df) > 0)

    carpeta_out <- ruta_output()
    if (is.null(carpeta_out) || length(carpeta_out) == 0) {
      showNotification("⚠ Selecciona una carpeta de salida primero.", type = "warning")
      return()
    }

    nombre_archivo <- input$nombre_csv
    if (!str_ends(nombre_archivo, "\\.csv$", negate = FALSE)) {
      nombre_archivo <- paste0(nombre_archivo, ".csv")
    }

    ruta_csv <- file.path(carpeta_out, nombre_archivo)

    tryCatch({
      write.csv(df, file = ruta_csv, row.names = FALSE, fileEncoding = "UTF-8")
      agregar_log(sprintf("💾 CSV guardado en: %s", ruta_csv), "ok")
      showNotification(
        paste("✅ Archivo guardado:", ruta_csv),
        type     = "message",
        duration = 6
      )
    }, error = function(e) {
      agregar_log(sprintf("✗ Error al guardar: %s", e$message), "error")
      showNotification(paste("❌ Error:", e$message), type = "error")
    })
  })

  # ── Descarga directa ───────────────────────────────────────────────────────
  output$btn_download <- downloadHandler(
    filename = function() {
      nm <- input$nombre_csv
      if (!str_ends(nm, "\\.csv$")) nm <- paste0(nm, ".csv")
      nm
    },
    content = function(file) {
      df <- datos_compilados()
      if (is.null(df) || nrow(df) == 0) {
        write.csv(data.frame(Mensaje = "Sin datos"), file, row.names = FALSE)
      } else {
        write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    },
    contentType = "text/csv"
  )

  # ── UI: estadísticas resumen ───────────────────────────────────────────────
  output$ui_stats <- renderUI({
    df <- datos_compilados()
    if (is.null(df) || nrow(df) == 0) return(NULL)

    stat_box <- function(value, label) {
      div(class = "stat-box",
        div(class = "stat-value", value),
        div(class = "stat-label", label)
      )
    }

    n_reg    <- format(nrow(df), big.mark = ".")
    n_est    <- format(n_distinct(df$Estacion), big.mark = ".")
    n_cuenca <- format(n_distinct(df$Cuenca), big.mark = ".")
    rango    <- if ("Fecha" %in% names(df) && any(!is.na(df$Fecha))) {
      sprintf("%s — %s",
              format(min(df$Fecha, na.rm = TRUE), "%Y"),
              format(max(df$Fecha, na.rm = TRUE), "%Y"))
    } else "—"

    card(
      card_body(
        fluidRow(
          column(3, stat_box(n_reg,    "Registros totales")),
          column(3, stat_box(n_est,    "Estaciones")),
          column(3, stat_box(n_cuenca, "Cuencas")),
          column(3, stat_box(rango,    "Período"))
        )
      )
    )
  })

  # ── UI: log ────────────────────────────────────────────────────────────────
  output$ui_log <- renderUI({
    msgs <- log_mensajes()
    if (length(msgs) == 0) {
      return(HTML('<span style="color:#858585;">— Esperando procesamiento... —</span>'))
    }
    # Scroll automático al final
    html_content <- paste(msgs, collapse = "<br>")
    tagList(
      HTML(html_content),
      tags$script("
        var log = document.getElementById('log_container');
        if (log) log.scrollTop = log.scrollHeight;
      ")
    )
  })

  # ── UI: badge de registros ─────────────────────────────────────────────────
  output$ui_badge_registros <- renderUI({
    df <- datos_compilados()
    if (is.null(df)) return(NULL)
    span(class = "badge bg-light text-dark",
         format(nrow(df), big.mark = "."), " filas")
  })

  # ── Tabla de datos ─────────────────────────────────────────────────────────
  output$tabla_datos <- renderDT({
    df <- datos_compilados()
    req(df, nrow(df) > 0)

    # Columnas numéricas con 3 decimales
    cols_num <- c("Altitud_msnm", "Latitud_S", "Longitud_W",
                  "UTM_Norte", "UTM_Este", "AreaDrenaje_km2", "Valor")

    datatable(
      df,
      filter   = "top",
      rownames = FALSE,
      options  = list(
        pageLength   = 15,
        scrollX      = TRUE,
        dom          = "Bfrtip",
        buttons      = c("csv", "excel"),
        columnDefs   = list(
          list(className = "dt-center",
               targets   = which(names(df) %in% c("Fecha", "Indicador")) - 1)
        ),
        language = list(
          url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"
        )
      ),
      extensions = "Buttons"
    ) %>%
      formatRound(
        columns = intersect(cols_num, names(df)),
        digits  = 3
      ) %>%
      formatStyle(
        "Valor",
        background = styleColorBar(range(df$Valor, na.rm = TRUE), "#d6e8f5"),
        backgroundSize   = "100% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })
}

# ─────────────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
