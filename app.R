# =============================================================================
# app.R — Compilador de Caudales Medios Diarios DGA
# Shiny App | DGA Chile — Recursos Hídricos
# =============================================================================

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(DT)
library(shinycssloaders)
library(bslib)
library(ggplot2)
library(plotly)
library(scales)
library(htmltools)
library(RColorBrewer)
library(shinymanager) # [NUEVO: shinymanager] Paquete de autenticación

source("leer_dga_caudales.R")

# [NUEVO: shinymanager] ── Definición de Credenciales ─────────────────────────
credentials <- data.frame(
  user = c("Diego_HP", "Alisonmariela", "ICASS_1"),
  password = c("diegoHP07", "Alisonmariela", "ICASS_268"),
  # El rol 'admin' permite a shinymanager dar accesos especiales si lo requieres a futuro
  admin = c(TRUE, TRUE, FALSE), 
  stringsAsFactors = FALSE
)

# Paleta fija para hasta 5 estaciones
# ── Paleta dinámica basada en RColorBrewer "Paired" ──────────────────────────
# Paired tiene 12 colores base. Para n > 12 se interpola con colorRampPalette()
# garantizando que nunca haya colores repetidos independiente del número de estaciones.
PAIRED_BASE <- brewer.pal(12, "Paired")

paleta_dinamica <- function(n) {
  if (n <= 0)  return(character(0))
  if (n <= 12) return(PAIRED_BASE[seq_len(n)])
  # Más de 12: interpolar suavemente manteniendo la identidad visual de Paired
  colorRampPalette(PAIRED_BASE)(n)
}

# Sin límite fijo de estaciones — la paleta se adapta
MAX_EST <- 5

# Servir logo local desde el directorio de la app
addResourcePath("logo.png", file.path(getwd(), "logo.png"))

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────
ui <- secure_app(
  page_fluid(
  theme = bs_theme(
    version    = 5,
    bootswatch = "flatly",
    primary    = "#2c7fb8",
    base_font  = font_google("Inter")
  ),
  
  tags$head(
    tags$style(HTML("
      /* ── Headers ── */
      .card-header-dga {
        background-color: #2c7fb8; color: white;
        font-weight: 600; padding: 0.75rem 1.25rem;
        border-radius: 0.375rem 0.375rem 0 0;
      }
      /* ── Stat boxes ── */
      .stat-box {
        background: #f0f7ff; border-left: 4px solid #2c7fb8;
        padding: 12px 16px; border-radius: 4px; margin-bottom: 10px;
      }
      .stat-box .stat-value { font-size: 1.5rem; font-weight: 700; color: #2c7fb8; }
      .stat-box .stat-label { font-size: 0.78rem; color: #666; text-transform: uppercase; letter-spacing: .04em; }
      /* ── Log terminal ── */
      .log-box {
        background: #1e1e1e; color: #d4d4d4;
        font-family: 'Fira Mono', monospace; font-size: 0.77rem;
        padding: 10px; border-radius: 4px;
        max-height: 170px; overflow-y: auto;
      }
      /* ── Resumen tabla ── */
      .resumen-tabla thead { background-color: #eaf3fb; }
      /* ── Checkboxes estaciones ── */
      .est-check-group .checkbox { margin-bottom: 6px; }
      .est-check-group .checkbox label {
        display: flex; align-items: center; gap: 8px;
        font-size: 0.85rem; cursor: pointer;
      }
      .est-dot {
        width: 12px; height: 12px; border-radius: 50%;
        display: inline-block; flex-shrink: 0;
      }
      /* ── Tabs ── */
      .nav-tabs .nav-link.active { font-weight: 600; color: #2c7fb8; }
      /* ── FileInput estilizado ── */
      .form-group { margin-bottom: 0 !important; }
      .form-control[type=file]::file-selector-button {
        background-color: #2c7fb8; color: white;
        border: none; padding: 4px 10px;
        font-size: 0.82rem; border-radius: 4px;
        cursor: pointer;
      }
      .form-control[type=file]::file-selector-button:hover {
        background-color: #1a5f8a;
      }
      .form-control[type=file] {
        font-size: 0.8rem; padding: 4px 8px;
        border: 1px solid #c8dff0; border-radius: 4px;
      }
      #archivos_input_progress { display: none !important; }
      /* ── Navbar superior con logo ── */
      .dga-navbar {
        background: linear-gradient(135deg, #0d2b45 0%, #1a4a6e 60%, #2c7fb8 100%);
        padding: 0;
        margin-bottom: 1.25rem;
        box-shadow: 0 3px 12px rgba(0,0,0,0.25);
        border-radius: 0 0 8px 8px;
      }
      .dga-navbar-inner {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 10px 24px;
        min-height: 88px;
      }
      .dga-navbar-logo {
        height: 72px;
        max-width: 340px;
        object-fit: contain;
        filter: drop-shadow(0 2px 6px rgba(0,0,0,0.35));
        transition: transform 0.2s ease;
      }
      .dga-navbar-logo:hover { transform: scale(1.03); }
      .dga-navbar-title {
        flex: 1;
        padding: 0 24px;
        text-align: center;
      }
      .dga-navbar-title h2 {
        color: #ffffff;
        font-size: 1.15rem;
        font-weight: 700;
        margin: 0 0 2px 0;
        letter-spacing: 0.02em;
        text-shadow: 0 1px 3px rgba(0,0,0,0.3);
      }
      .dga-navbar-title p {
        color: #a8d4f0;
        font-size: 0.78rem;
        margin: 0;
        letter-spacing: 0.04em;
        text-transform: uppercase;
      }
      .dga-navbar-badge {
        background: rgba(255,255,255,0.12);
        border: 1px solid rgba(255,255,255,0.25);
        border-radius: 20px;
        padding: 6px 14px;
        color: #d0eeff;
        font-size: 0.72rem;
        font-weight: 600;
        letter-spacing: 0.05em;
        text-transform: uppercase;
        white-space: nowrap;
      }
      .dga-navbar-badge span {
        display: block;
        font-size: 0.65rem;
        color: #8ec8f0;
        font-weight: 400;
        margin-top: 1px;
      }
    "))
  ),
  
  # ── Navbar con logo prominente ───────────────────────────────────────────────
  div(class = "dga-navbar",
      div(class = "dga-navbar-inner",
          # Logo empresa — protagonista
          tags$img(src   = "logo.png",
                   class = "dga-navbar-logo",
                   alt   = "Plataforma Compiladora DGA Chile"),
          # Título centrado
          div(class = "dga-navbar-title",
              tags$h2("Compilador de Caudales Medios Diarios"),
              tags$p("Dirección General de Aguas · Chile")
          ),
          # Badge versión / estado
          div(class = "dga-navbar-badge",
              "HIDROCOMP-CL"
          )
      )
  ),
  
  # ── Layout principal ─────────────────────────────────────────────────────────
  fluidRow(
    
    # ════════════════════════════════════════════════════════
    # PANEL IZQUIERDO — Configuración
    # ════════════════════════════════════════════════════════
    column(3,
           card(
             card_header(div(class = "card-header-dga", "⚙️  Configuración")),
             card_body(
               h6("📂 Archivos XLS / XLSX", class = "fw-bold mt-1"),
               p("Sube uno o más archivos XLS DGA directamente desde tu equipo.",
                 style = "font-size:.81rem; color:#666; margin-bottom:6px;"),
               fileInput("archivos_input",
                         label    = NULL,
                         multiple = TRUE,
                         accept   = c(".xls", ".xlsx"),
                         placeholder = "Ningún archivo seleccionado",
                         buttonLabel = tags$span(icon("folder-open"), " Seleccionar")),
               uiOutput("ui_resumen_archivos"),
               
               hr(style = "margin: 10px 0;"),
               
               h6("🔧 Opciones", class = "fw-bold"),
               checkboxInput("solo_validos",      "Excluir registros sin valor",  value = TRUE),
               checkboxInput("incluir_indicador", "Incluir indicador (*, <, >)",  value = TRUE),
               
               hr(style = "margin: 10px 0;"),
               
               actionButton("btn_procesar",
                            label = tags$span(icon("play"), " Procesar archivos"),
                            class = "btn btn-primary w-100 fw-bold"),
               
               hr(style = "margin: 10px 0;"),
               
               h6("\u2b07  Descargar datos", class = "fw-bold"),
               p("Disponible tras procesar.", style = "font-size:.8rem; color:#666; margin-bottom:8px;"),
               downloadButton("dl_diario",  "Caudal diario",
                              class = "btn btn-outline-primary btn-sm w-100 mb-2"),
               br(),
               downloadButton("dl_mensual", "Caudal medio mensual",
                              class = "btn btn-outline-primary btn-sm w-100 mb-2"),
               br(),
               downloadButton("dl_anual",   "Caudal medio anual",
                              class = "btn btn-outline-primary btn-sm w-100")
             )
           )
    ),
    
    # ════════════════════════════════════════════════════════
    # PANEL DERECHO — Resultados
    # ════════════════════════════════════════════════════════
    column(9,
           
           # ── Stat boxes ────────────────────────────────────────
           uiOutput("ui_stats"),
           
           # ── Log ───────────────────────────────────────────────
           card(
             card_header(div(class = "card-header-dga", "📋 Log de procesamiento")),
             card_body(padding = "0.5rem",
                       div(class = "log-box", id = "log_container", uiOutput("ui_log"))
             )
           ),
           
           br(),
           
           # ── Tabs principales ─────────────────────────────────
           navset_card_tab(
             id = "tabs_resultados",
             
             # ── Tab 1: Resumen por estación ─────────────────────
             nav_panel(
               title = tagList(icon("table"), " Resumen por estación"),
               card_body(padding = "0",
                         withSpinner(DTOutput("tabla_resumen"), type = 6, color = "#2c7fb8")
               )
             ),
             
             # ── Tab 2: Gráfico ──────────────────────────────────
             nav_panel(
               title = tagList(icon("chart-line"), " Gráfico de caudales"),
               card_body(
                 fluidRow(
                   # Selector de estaciones
                   column(3,
                          div(
                            h6(tags$b("Seleccionar estaciones"),
                               tags$small(class = "text-muted ms-1", "(máx. 5)")),
                            div(class = "est-check-group",
                                uiOutput("ui_check_estaciones")
                            ),
                            hr(),
                            # Tipo de agregación
                            h6(tags$b("Agregación temporal")),
                            selectInput("agr_tipo", label = NULL,
                                        choices = c("Diaria" = "dia",
                                                    "Media mensual" = "mes",
                                                    "Media anual"   = "anio"),
                                        selected = "dia"),
                            # Escala Y
                            checkboxInput("escala_log", "Escala Y logarítmica", value = FALSE),
                            
                            hr(),
                            actionButton("btn_deselect_all", "Deseleccionar todo",
                                         class = "btn btn-outline-secondary btn-sm w-100")
                          )
                   ),
                   # Gráfico
                   column(9,
                          withSpinner(
                            plotlyOutput("grafico_caudales", height = "480px"),
                            type = 6, color = "#2c7fb8"
                          )
                   )
                 )
               )
             ),
             
             # ── Tab 3: Datos completos ────────────────────────────
             nav_panel(
               title = tagList(icon("database"), " Datos completos"),
               card_body(padding = "0",
                         div(class = "text-end pe-3 pt-2",
                             uiOutput("ui_badge_registros")
                         ),
                         withSpinner(DTOutput("tabla_datos"), type = 6, color = "#2c7fb8")
               )
             )
           )
      )
    )
  ), 
  # [NUEVO: shinymanager] Personalización de la pantalla de login (opcional pero recomendado)
  theme = bs_theme(version = 5, bootswatch = "flatly")
)

# ─────────────────────────────────────────────────────────────────────────────
# SERVER
# ─────────────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  # [NUEVO: shinymanager] ── Verificación de credenciales al iniciar sesión ──
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # ── Archivos subidos ─────────────────────────────────────────────────────────
  # input$archivos_input es un data.frame con columnas: name, size, type, datapath
  # datapath apunta a archivos temporales en el servidor — listos para leer con readxl

  archivos_df <- reactive({
    req(input$archivos_input)
    input$archivos_input   # columnas: name, size, type, datapath
  })

  # Mini-resumen debajo del fileInput
  output$ui_resumen_archivos <- renderUI({
    df <- archivos_df()
    if (is.null(df)) return(NULL)
    n   <- nrow(df)
    mb  <- round(sum(df$size) / 1024^2, 2)
    div(style = "font-size:.78rem; color:#2c7fb8; margin-top:-8px; margin-bottom:4px;",
        icon("circle-check"),
        sprintf("  %d archivo%s · %.2f MB listos para procesar", n, if(n==1) "" else "s", mb))
  })
  
  # ── Estado global ────────────────────────────────────────────────────────────
  datos_compilados  <- reactiveVal(NULL)
  log_mensajes      <- reactiveVal(character(0))
  est_seleccionadas <- reactiveVal(character(0))   # estaciones activas en gráfico
  
  # ── Log helpers ──────────────────────────────────────────────────────────────
  agregar_log <- function(msg, tipo = "info") {
    color <- switch(tipo, ok = "#4ec9b0", error = "#f44747", warn = "#dcdcaa", "#d4d4d4")
    ts    <- format(Sys.time(), "%H:%M:%S")
    html  <- sprintf('<span style="color:#858585;">[%s]</span> <span style="color:%s;">%s</span>',
                     ts, color, htmltools::htmlEscape(msg))
    log_mensajes(c(log_mensajes(), html))
  }
  
  # ── Procesamiento ────────────────────────────────────────────────────────────
  observeEvent(input$btn_procesar, {
    df_files <- archivos_df()
    req(df_files, nrow(df_files) > 0)

    datos_compilados(NULL)
    log_mensajes(character(0))
    est_seleccionadas(character(0))

    agregar_log(sprintf("📁 Archivos recibidos: %d", nrow(df_files)))

    withProgress(message = "Procesando archivos DGA...", value = 0, {
      todos   <- list()
      errores <- 0L

      for (i in seq_len(nrow(df_files))) {
        # datapath = ruta temporal en el servidor; name = nombre original del usuario
        archivo <- df_files$datapath[i]
        nombre  <- df_files$name[i]

        incProgress(1 / nrow(df_files),
                    detail = sprintf("(%d/%d) %s", i, nrow(df_files), nombre))
        agregar_log(sprintf("  ▶ [%d/%d] %s", i, nrow(df_files), nombre))

        res <- tryCatch({
          df <- leer_dga_caudales(archivo,
                                  log_fn = function(m) agregar_log(paste("   ", m), "warn"))
          df$ArchivoFuente <- nombre   # guardar el nombre original, no la ruta temporal
          df
        }, error = function(e) {
          agregar_log(sprintf("    ✗ %s", e$message), "error")
          errores <<- errores + 1L; NULL
        })
        
        if (!is.null(res) && nrow(res) > 0) {
          todos[[length(todos) + 1L]] <- res
          agregar_log(sprintf("    ✓ %d registros | %d estación(es)",
                              nrow(res), n_distinct(res$Estacion)), "ok")
        }
      }
      
      if (length(todos) > 0) {
        comp <- bind_rows(todos) %>% arrange(Estacion, Fecha)
        if (input$solo_validos)      comp <- comp %>% filter(!is.na(Valor))
        if (!input$incluir_indicador && "Indicador" %in% names(comp))
          comp <- comp %>% select(-Indicador)
        
        datos_compilados(comp)
        
        # Pre-seleccionar todas las estaciones disponibles (máx. 12 por defecto visual)
        est_disponibles <- sort(unique(comp$Estacion))
        est_seleccionadas(head(est_disponibles, 12L))
        
        agregar_log(sprintf(
          "✅ Compilación OK: %s registros | %s estaciones | %d archivos",
          format(nrow(comp), big.mark = "."),
          n_distinct(comp$Estacion), length(todos)), "ok")
        if (errores > 0)
          agregar_log(sprintf("⚠ %d archivo(s) con errores.", errores), "warn")
        
      } else {
        agregar_log("❌ No se pudo extraer ningún dato.", "error")
      }
    })
  })
  
  # ── Descargas: diario / mensual / anual ──────────────────────────────────────
  hoy <- format(Sys.Date(), "%Y%m%d")
  
  make_dl <- function(sufijo, preparar_df) {
    downloadHandler(
      filename    = function() sprintf("caudales_%s_%s.csv", sufijo, hoy),
      content     = function(file) {
        df <- preparar_df()
        write.csv(if (is.null(df) || !nrow(df)) data.frame(Mensaje = "Sin datos") else df,
                  file, row.names = FALSE, fileEncoding = "UTF-8")
      },
      contentType = "text/csv"
    )
  }
  
  output$dl_diario <- make_dl("diario", function() {
    datos_compilados()
  })
  
  output$dl_mensual <- make_dl("mensual", function() {
    df <- datos_compilados(); req(df, nrow(df) > 0)
    df %>%
      filter(!is.na(Valor), !is.na(Fecha)) %>%
      mutate(Anio = year(Fecha), Mes = month(Fecha)) %>%
      group_by(Estacion, CodigoBNA, Cuenca, SubCuenca,
               Altitud_msnm, Latitud_S, Longitud_W, UTM_Norte, UTM_Este, AreaDrenaje_km2,
               Anio, Mes) %>%
      summarise(Q_medio_mensual = round(mean(Valor, na.rm = TRUE), 4),
                N_dias          = n(),
                .groups = "drop") %>%
      arrange(Estacion, Anio, Mes)
  })
  
  output$dl_anual <- make_dl("anual", function() {
    df <- datos_compilados(); req(df, nrow(df) > 0)
    df %>%
      filter(!is.na(Valor), !is.na(Fecha)) %>%
      mutate(Anio = year(Fecha)) %>%
      group_by(Estacion, CodigoBNA, Cuenca, SubCuenca,
               Altitud_msnm, Latitud_S, Longitud_W, UTM_Norte, UTM_Este, AreaDrenaje_km2,
               Anio) %>%
      summarise(Q_medio_anual = round(mean(Valor, na.rm = TRUE), 4),
                Q_max_anual   = round(max(Valor,  na.rm = TRUE), 4),
                Q_min_anual   = round(min(Valor,  na.rm = TRUE), 4),
                N_dias        = n(),
                .groups = "drop") %>%
      arrange(Estacion, Anio)
  })
  
  # ═══════════════════════════════════════════════════════════════════════════════
  # OUTPUTS
  # ═══════════════════════════════════════════════════════════════════════════════
  
  # ── Stat boxes ────────────────────────────────────────────────────────────────
  output$ui_stats <- renderUI({
    df <- datos_compilados(); if (is.null(df) || !nrow(df)) return(NULL)
    sb <- function(v, l) div(class = "stat-box",
                             div(class = "stat-value", v),
                             div(class = "stat-label", l))
    rango <- if (any(!is.na(df$Fecha)))
      sprintf("%s — %s", format(min(df$Fecha, na.rm=T), "%Y"),
              format(max(df$Fecha, na.rm=T), "%Y")) else "—"
    
    card(card_body(fluidRow(
      column(3, sb(format(nrow(df), big.mark="."),    "Registros totales")),
      column(3, sb(n_distinct(df$Estacion),           "Estaciones")),
      column(3, sb(n_distinct(df$Cuenca),             "Cuencas")),
      column(3, sb(rango,                             "Período"))
    )))
  })
  
  # ── Log ───────────────────────────────────────────────────────────────────────
  output$ui_log <- renderUI({
    msgs <- log_mensajes()
    if (!length(msgs)) return(HTML('<span style="color:#858585;">— Esperando procesamiento... —</span>'))
    tagList(
      HTML(paste(msgs, collapse = "<br>")),
      tags$script("var l=document.getElementById('log_container'); if(l) l.scrollTop=l.scrollHeight;")
    )
  })
  
  # ── Badge registros ───────────────────────────────────────────────────────────
  output$ui_badge_registros <- renderUI({
    df <- datos_compilados(); if (is.null(df)) return(NULL)
    span(class = "badge bg-secondary", format(nrow(df), big.mark="."), " filas")
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # TAB 1 — RESUMEN POR ESTACIÓN
  # ════════════════════════════════════════════════════════════════════════════
  resumen_estaciones <- reactive({
    df <- datos_compilados(); req(df, nrow(df) > 0)
    
    df %>%
      group_by(Estacion, CodigoBNA, Cuenca, SubCuenca,
               Altitud_msnm, Latitud_S, Longitud_W,
               UTM_Norte, UTM_Este, AreaDrenaje_km2) %>%
      summarise(
        N_registros  = n(),
        Fecha_inicio = min(Fecha, na.rm = TRUE),
        Fecha_fin    = max(Fecha, na.rm = TRUE),
        Q_media      = round(mean(Valor, na.rm = TRUE), 3),
        Q_max        = round(max(Valor,  na.rm = TRUE), 3),
        Q_min        = round(min(Valor,  na.rm = TRUE), 3),
        .groups = "drop"
      ) %>%
      mutate(
        Años_registro = as.integer(year(Fecha_fin) - year(Fecha_inicio) + 1L),
        Rango_fechas  = sprintf("%s → %s",
                                format(Fecha_inicio, "%d/%m/%Y"),
                                format(Fecha_fin,    "%d/%m/%Y"))
      ) %>%
      select(
        Estacion, CodigoBNA, Cuenca, SubCuenca,
        Altitud_msnm, Latitud_S, Longitud_W, UTM_Norte, UTM_Este, AreaDrenaje_km2,
        N_registros, Años_registro, Rango_fechas,
        Q_media, Q_max, Q_min
      ) %>%
      arrange(Cuenca, Estacion)
  })
  
  output$tabla_resumen <- renderDT({
    df <- resumen_estaciones()
    
    cols_show <- c("Estacion", "CodigoBNA", "Cuenca", "SubCuenca",
                   "Altitud_msnm", "Latitud_S", "Longitud_W",
                   "UTM_Norte", "UTM_Este", "AreaDrenaje_km2",
                   "N_registros", "Años_registro", "Rango_fechas",
                   "Q_media", "Q_max", "Q_min")
    
    datatable(
      df[, intersect(cols_show, names(df))],
      rownames   = FALSE,
      filter     = "top",
      class      = "resumen-tabla compact stripe hover",
      extensions = c("Buttons", "FixedHeader"),
      options = list(
        pageLength  = 20,
        scrollX     = TRUE,
        fixedHeader = TRUE,
        dom         = "Bfrtip",
        buttons     = c("csv", "excel"),
        language    = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"),
        columnDefs  = list(
          list(className = "dt-center",
               targets   = which(cols_show %in% c("CodigoBNA","N_registros",
                                                  "Años_registro","Rango_fechas")) - 1),
          # Columnas numéricas geoespaciales: anchura fija
          list(width = "90px",
               targets = which(cols_show %in% c("Altitud_msnm","Latitud_S","Longitud_W",
                                                "UTM_Norte","UTM_Este","AreaDrenaje_km2")) - 1)
        )
      )
    ) %>%
      formatRound(c("Latitud_S","Longitud_W"), digits = 5) %>%
      formatRound(c("Altitud_msnm","UTM_Norte","UTM_Este","AreaDrenaje_km2",
                    "Q_media","Q_max","Q_min"), digits = 2) %>%
      formatCurrency("N_registros", currency = "", digits = 0, mark = ".") %>%
      formatStyle("Q_media",
                  background = styleColorBar(c(0, max(df$Q_max, na.rm=TRUE)), "#cce5ff"),
                  backgroundSize = "100% 75%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # TAB 2 — SELECTOR DE ESTACIONES + GRÁFICO
  # ════════════════════════════════════════════════════════════════════════════
  
  # Checkboxes con punto de color por estación
  output$ui_check_estaciones <- renderUI({
    df <- datos_compilados(); req(df, nrow(df) > 0)
    ests   <- sort(unique(df$Estacion))
    selec  <- est_seleccionadas()
    colores <- paleta_dinamica(length(ests))

    checkboxes <- lapply(seq_along(ests), function(i) {
      est   <- ests[i]
      color <- colores[i]
      checked <- est %in% selec
      
      tags$div(class = "checkbox",
               tags$label(
                 tags$input(type = "checkbox", name = "est_graf", value = est,
                            checked = if (checked) "checked" else NULL,
                            style   = "margin-right:4px;"),
                 tags$span(class = "est-dot", style = sprintf("background-color:%s;", color)),
                 tags$span(est, style = "font-size:0.82rem; word-break:break-word;")
               )
      )
    })
    
    div(
      # JS que captura los cambios y los envía como input$est_graf_vals
      tags$script(HTML("
        $(document).on('change', 'input[name=est_graf]', function() {
          var vals = [];
          $('input[name=est_graf]:checked').each(function() {
            vals.push($(this).val());
          });
          Shiny.setInputValue('est_graf_vals', vals, {priority: 'event'});
        });
      ")),
      tagList(checkboxes)
    )
  })
  
  # Sincronizar selección reactiva con los checkboxes
  observeEvent(input$est_graf_vals, {
    sel <- input$est_graf_vals
    if (is.null(sel)) sel <- character(0)
    est_seleccionadas(sel)
  }, ignoreNULL = FALSE)
  
  # Deseleccionar todo
  observeEvent(input$btn_deselect_all, {
    est_seleccionadas(character(0))
    output$ui_check_estaciones <- renderUI({
      df <- datos_compilados(); req(df)
      ests    <- sort(unique(df$Estacion))
      colores <- paleta_dinamica(length(ests))
      checkboxes <- lapply(seq_along(ests), function(i) {
        color <- colores[i]
        tags$div(class = "checkbox",
                 tags$label(
                   tags$input(type = "checkbox", name = "est_graf", value = ests[i],
                              style = "margin-right:4px;"),
                   tags$span(class = "est-dot", style = sprintf("background-color:%s;", color)),
                   tags$span(ests[i], style = "font-size:0.82rem;")
                 )
        )
      })
      div(
        tags$script(HTML("
          $(document).on('change', 'input[name=est_graf]', function() {
            var vals = [];
            $('input[name=est_graf]:checked').each(function() { vals.push($(this).val()); });
            Shiny.setInputValue('est_graf_vals', vals, {priority: 'event'});
          });
        ")),
        tagList(checkboxes)
      )
    })
  })
  
  # ── Datos para el gráfico (agregados) ────────────────────────────────────────
  datos_grafico <- reactive({
    df  <- datos_compilados(); req(df, nrow(df) > 0)
    sel <- est_seleccionadas()
    if (!length(sel)) return(NULL)
    
    df_sel <- df %>% filter(Estacion %in% sel, !is.na(Valor), !is.na(Fecha))
    
    agr <- switch(input$agr_tipo,
                  "mes"  = df_sel %>%
                    mutate(Periodo = floor_date(Fecha, "month")) %>%
                    group_by(Estacion, Periodo) %>%
                    summarise(Valor = mean(Valor, na.rm=TRUE), .groups="drop") %>%
                    rename(Fecha = Periodo),
                  "anio" = df_sel %>%
                    mutate(Periodo = floor_date(Fecha, "year")) %>%
                    group_by(Estacion, Periodo) %>%
                    summarise(Valor = mean(Valor, na.rm=TRUE), .groups="drop") %>%
                    rename(Fecha = Periodo),
                  df_sel %>% select(Estacion, Fecha, Valor)   # "dia" — sin cambios
    )
    
    # Color fijo por posición global — paleta crece con el número de estaciones
    ests_todas <- sort(unique(datos_compilados()$Estacion))
    colores    <- paleta_dinamica(length(ests_todas))
    agr %>%
      mutate(Color = colores[match(Estacion, ests_todas)])
  })
  
  # ── Renderizar ggplotly ───────────────────────────────────────────────────────
  output$grafico_caudales <- renderPlotly({
    df_g <- datos_grafico()
    
    if (is.null(df_g) || !nrow(df_g)) {
      # Gráfico vacío con mensaje
      p_vacio <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, size = 5, color = "#aaa",
                 label = "Selecciona al menos una estación para visualizar") +
        theme_void()
      return(ggplotly(p_vacio))
    }
    
    titulo_agr <- switch(input$agr_tipo,
                         "dia"  = "Caudal medio diario",
                         "mes"  = "Caudal medio mensual",
                         "anio" = "Caudal medio anual"
    )
    
    # Colores asignados (deframe() requiere tibble; usamos base R)
    color_df       <- df_g %>% distinct(Estacion, Color)
    colores_usados <- setNames(color_df$Color, color_df$Estacion)
    
    p <- ggplot(df_g, aes(x = Fecha, y = Valor, color = Estacion,
                          group = Estacion,
                          text = paste0(
                            "<b>", Estacion, "</b><br>",
                            "Fecha: ", format(Fecha, "%d/%m/%Y"), "<br>",
                            "Q: ", round(Valor, 3), " m³/s"
                          ))) +
      geom_line(linewidth = 0.7, alpha = 0.85) +
      scale_color_manual(values = colores_usados) +
      scale_x_date(date_labels = "%b %Y", expand = expansion(mult = .02)) +
      labs(
        title  = titulo_agr,
        x      = NULL,
        y      = "Caudal (m³/s)",
        color  = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title       = element_text(face = "bold", color = "#2c7fb8", size = 13),
        legend.position  = "bottom",
        legend.text      = element_text(size = 9),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#e8e8e8"),
        axis.text.x      = element_text(angle = 30, hjust = 1, size = 8)
      )
    
    if (input$escala_log) {
      p <- p + scale_y_log10(labels = label_comma())
    } else {
      p <- p + scale_y_continuous(labels = label_comma())
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", y = -0.15, x = 0),
        hovermode = "x unified",
        margin    = list(t = 50, b = 60)
      ) %>%
      config(displayModeBar = TRUE,
             modeBarButtonsToRemove = c("lasso2d", "select2d"),
             displaylogo = FALSE)
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # TAB 3 — DATOS COMPLETOS
  # ════════════════════════════════════════════════════════════════════════════
  output$tabla_datos <- renderDT({
    df <- datos_compilados(); req(df, nrow(df) > 0)
    cols_num <- c("Altitud_msnm","Latitud_S","Longitud_W","UTM_Norte","UTM_Este",
                  "AreaDrenaje_km2","Valor")
    datatable(
      df, filter = "top", rownames = FALSE,
      class      = "compact stripe hover",
      extensions = "Buttons",
      options = list(
        pageLength = 15, scrollX = TRUE,
        dom        = "Bfrtip",
        buttons    = c("csv", "excel"),
        language   = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"),
        columnDefs = list(
          list(className = "dt-center",
               targets = which(names(df) %in% c("Fecha","Indicador")) - 1)
        )
      )
    ) %>%
      formatRound(intersect(cols_num, names(df)), digits = 3) %>%
      formatStyle("Valor",
                  background         = styleColorBar(range(df$Valor, na.rm=TRUE), "#d6e8f5"),
                  backgroundSize     = "100% 80%",
                  backgroundRepeat   = "no-repeat",
                  backgroundPosition = "center")
  })
}

# ─────────────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)