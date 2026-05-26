# =============================================================================
# app.R — Compilador de Caudales Medios Diarios DGA
# Shiny App | DGA Chile — Recursos Hídricos
# Auth: custom sin shinymanager (incompatible con Shiny moderno)
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
library(digest)   # [FIX-APP-1] requerido por credentials_supabase(); antes faltaba el library()

source("leer_dga_caudales.R")
source("supabase_utils.R")

# [FIX-APP-3] Operador %||% (null-coalesce) usado en ui_btn_logout pero nunca definido.
# rlang lo exporta pero no lo re-exporta al entorno global en todas las versiones.
`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0 && !is.na(x[1])) x else y

# ── Verificación de credenciales contra Supabase ─────────────────────────────
# [FIX-APP-2] Correcciones:
#   - sb_get_usuarios() ahora devuelve data.frame vacío en error (no NULL)
#   - Se fuerza as.data.frame() para compatibilidad tibble/data.frame
#   - idx[1]: evita error si hay usuarios duplicados
#   - digest() usa el paquete cargado (ya no digest::digest())
credentials_supabase <- function(user, password) {
  usuarios <- tryCatch(
    as.data.frame(sb_get_usuarios()),
    error = function(e) {
      message("[AUTH] Error al obtener usuarios de Supabase: ", e$message)
      data.frame()
    }
  )

  if (!is.data.frame(usuarios) || nrow(usuarios) == 0) {
    message("[AUTH] Tabla de usuarios vacía o error de conexión.")
    return(FALSE)
  }

  idx <- which(usuarios$username == trimws(user))
  if (length(idx) == 0) { sb_log(user, "fail"); return(FALSE) }
  fila <- usuarios[idx[1], , drop = FALSE]   # primera fila, siempre data.frame

  hash_ingresado <- digest(password, algo = "sha256")   # library(digest) cargado arriba
  if (isTRUE(fila$password_hash == hash_ingresado)) {
    sb_log(user, "ok")
    return(list(
      result = TRUE,
      user   = as.character(fila$username),
      rol    = as.character(fila$rol),
      nombre = as.character(fila$nombre)
    ))
  }
  sb_log(user, "fail")
  return(FALSE)
}

# ── Paleta dinámica ───────────────────────────────────────────────────────────
PAIRED_BASE    <- brewer.pal(12, "Paired")
paleta_dinamica <- function(n) {
  if (n <= 0)  return(character(0))
  if (n <= 12) return(PAIRED_BASE[seq_len(n)])
  colorRampPalette(PAIRED_BASE)(n)
}

addResourcePath("static", getwd())   # logo accesible en src="static/logo.png"

# =============================================================================
# UI HELPERS — bloques reutilizables
# =============================================================================
css_app <- tags$style(HTML("
  .card-header-dga {
    background-color: #2c7fb8; color: white;
    font-weight: 600; padding: 0.75rem 1.25rem;
    border-radius: 0.375rem 0.375rem 0 0;
  }
  .stat-box {
    background: #f0f7ff; border-left: 4px solid #2c7fb8;
    padding: 12px 16px; border-radius: 4px; margin-bottom: 10px;
  }
  .stat-box .stat-value { font-size: 1.5rem; font-weight: 700; color: #2c7fb8; }
  .stat-box .stat-label { font-size: 0.78rem; color: #666; text-transform: uppercase; letter-spacing:.04em; }
  .log-box {
    background: #1e1e1e; color: #d4d4d4;
    font-family: 'Fira Mono', monospace; font-size: 0.77rem;
    padding: 10px; border-radius: 4px;
    max-height: 170px; overflow-y: auto;
  }
  .resumen-tabla thead { background-color: #eaf3fb; }
  .est-check-group .checkbox { margin-bottom: 6px; }
  .est-check-group .checkbox label {
    display: flex; align-items: center; gap: 8px;
    font-size: 0.85rem; cursor: pointer;
  }
  .est-dot { width:12px; height:12px; border-radius:50%; display:inline-block; flex-shrink:0; }
  .nav-tabs .nav-link.active { font-weight: 600; color: #2c7fb8; }
  .form-group { margin-bottom: 0 !important; }
  .form-control[type=file]::file-selector-button {
    background-color: #2c7fb8; color: white;
    border: none; padding: 4px 10px; font-size: 0.82rem;
    border-radius: 4px; cursor: pointer;
  }
  .form-control[type=file]::file-selector-button:hover { background-color: #1a5f8a; }
  .form-control[type=file] {
    font-size: 0.8rem; padding: 4px 8px;
    border: 1px solid #c8dff0; border-radius: 4px;
  }
  #archivos_input_progress { display: none !important; }
  .dga-navbar {
    background: linear-gradient(135deg, #0d2b45 0%, #1a4a6e 60%, #2c7fb8 100%);
    padding: 0; margin-bottom: 1.25rem;
    box-shadow: 0 3px 12px rgba(0,0,0,0.25); border-radius: 0 0 8px 8px;
  }
  .dga-navbar-inner {
    display: flex; align-items: center; justify-content: space-between;
    padding: 10px 24px; min-height: 88px;
  }
  .dga-navbar-logo {
    height: 72px; max-width: 340px; object-fit: contain;
    filter: drop-shadow(0 2px 6px rgba(0,0,0,0.35)); transition: transform 0.2s ease;
  }
  .dga-navbar-logo:hover { transform: scale(1.03); }
  .dga-navbar-title { flex:1; padding:0 24px; text-align:center; }
  .dga-navbar-title h2 {
    color:#ffffff; font-size:1.15rem; font-weight:700;
    margin:0 0 2px 0; letter-spacing:.02em; text-shadow:0 1px 3px rgba(0,0,0,0.3);
  }
  .dga-navbar-title p {
    color:#a8d4f0; font-size:0.78rem; margin:0;
    letter-spacing:.04em; text-transform:uppercase;
  }
  .dga-navbar-badge {
    background:rgba(255,255,255,0.12); border:1px solid rgba(255,255,255,0.25);
    border-radius:20px; padding:6px 14px; color:#d0eeff;
    font-size:0.72rem; font-weight:600; letter-spacing:.05em;
    text-transform:uppercase; white-space:nowrap;
  }
  /* ── Login card ── */
  .login-wrapper {
    display:flex; justify-content:center; align-items:center; min-height:90vh;
  }
  .login-card { width:360px; }
  .login-logo { max-height:70px; display:block; margin:0 auto 1rem auto; }
  .login-title {
    text-align:center; color:#2c7fb8; font-size:1.1rem;
    font-weight:700; margin-bottom:0.25rem;
  }
  .login-subtitle {
    text-align:center; color:#888; font-size:0.8rem; margin-bottom:1.2rem;
  }
  /* ── Register tab ── */
  .login-card { width:400px; }
  .nav-pills .nav-link { font-size:0.85rem; padding:6px 18px; }
  .nav-pills .nav-link.active {
    background-color:#2c7fb8; color:#fff; font-weight:600;
  }
  .register-hint {
    font-size:0.75rem; color:#999; text-align:center; margin-top:0.75rem;
  }
  .pass-strength { height:4px; border-radius:2px; margin-top:4px; transition:width .3s; }
"))

# ── UI de login / registro ───────────────────────────────────────────────────────────────
ui_login <- div(class = "login-wrapper",
  div(class = "login-card",
    card(
      card_header(div(class = "card-header-dga", "\U0001f510  HIDROCOMP-CL")),
      card_body(
        tags$img(src = "static/logo.png", class = "login-logo",
                 alt = "Logo HIDROCOMP-CL"),
        p(class = "login-title", "Plataforma de Caudales DGA"),

        # ── Tabs Login / Registro ──────────────────────────────────────────────────
        navset_pill(
          id = "login_tab",

          # ── TAB: INGRESAR ────────────────────────────────────────────────────────────────
          nav_panel("Ingresar",
            br(),
            p(class = "login-subtitle",
              "Ingresa tus credenciales para continuar"),
            textInput("login_user", "Usuario",
                      placeholder = "Nombre de usuario"),
            passwordInput("login_pass", "Contrase\u00f1a",
                          placeholder = "\u2022\u2022\u2022\u2022\u2022\u2022\u2022\u2022"),
            actionButton("btn_login",
                         tags$span(icon("right-to-bracket"), " Ingresar"),
                         class = "btn btn-primary w-100 mt-2 fw-bold"),
            uiOutput("ui_login_error")
          ),

          # ── TAB: REGISTRARSE ────────────────────────────────────────────────────────────
          nav_panel("Registrarse",
            br(),
            p(class = "login-subtitle",
              "Crea tu cuenta para acceder a la plataforma"),
            textInput("reg_user",   "Usuario",
                      placeholder = "Sin espacios ni caracteres especiales"),
            textInput("reg_nombre", "Nombre completo",
                      placeholder = "Tu nombre y apellido"),
            textInput("reg_email",  "Correo electr\u00f3nico",
                      placeholder = "correo@ejemplo.com"),
            passwordInput("reg_pass",  "Contrase\u00f1a",
                          placeholder = "M\u00ednimo 8 caracteres"),
            passwordInput("reg_pass2", "Confirmar contrase\u00f1a",
                          placeholder = "Repite la contrase\u00f1a"),
            actionButton("btn_registrar",
                         tags$span(icon("user-plus"), " Crear cuenta"),
                         class = "btn btn-success w-100 mt-2 fw-bold"),
            uiOutput("ui_registro_msg"),
            p(class = "register-hint",
              icon("shield-halved"),
              " Tu contrase\u00f1a se almacena cifrada (SHA-256).")
          )
        )
      )
    )
  )
)

# ── UI de la app principal ────────────────────────────────────────────────────
ui_main <- tagList(
  # Navbar
  div(class = "dga-navbar",
    div(class = "dga-navbar-inner",
      tags$img(src = "static/logo.png", class = "dga-navbar-logo",
               alt = "Plataforma Compiladora DGA Chile"),
      div(class = "dga-navbar-title",
        tags$h2("Compilador de Caudales Medios Diarios"),
        tags$p("Dirección General de Aguas · Chile")
      ),
      div(class = "dga-navbar-badge", "HIDROCOMP-CL")
    )
  ),

  fluidRow(
    # ── Panel izquierdo ──────────────────────────────────────────────────────
    column(3,
      card(
        card_header(div(class = "card-header-dga", "⚙️  Configuración")),
        card_body(
          h6("\U0001f4c2 Archivos XLS / XLSX", class = "fw-bold mt-1"),
          p("Sube uno o más archivos XLS DGA directamente desde tu equipo.",
            style = "font-size:.81rem; color:#666; margin-bottom:6px;"),
          fileInput("archivos_input", label = NULL, multiple = TRUE,
                    accept = c(".xls", ".xlsx"),
                    placeholder = "Ningún archivo seleccionado",
                    buttonLabel = tags$span(icon("folder-open"), " Seleccionar")),
          uiOutput("ui_resumen_archivos"),
          hr(style = "margin:10px 0;"),
          h6("\U0001f527 Opciones", class = "fw-bold"),
          checkboxInput("solo_validos",      "Excluir registros sin valor",  value = TRUE),
          checkboxInput("incluir_indicador", "Incluir indicador (*, <, >)",  value = TRUE),
          hr(style = "margin:10px 0;"),
          actionButton("btn_procesar",
                       label = tags$span(icon("play"), " Procesar archivos"),
                       class = "btn btn-primary w-100 fw-bold"),
          hr(style = "margin:10px 0;"),
          h6("⬇  Descargar datos", class = "fw-bold"),
          p("Disponible tras procesar.", style = "font-size:.8rem; color:#666; margin-bottom:8px;"),
          downloadButton("dl_diario",  "Caudal diario",
                         class = "btn btn-outline-primary btn-sm w-100 mb-2"),
          br(),
          downloadButton("dl_mensual", "Caudal medio mensual",
                         class = "btn btn-outline-primary btn-sm w-100 mb-2"),
          br(),
          downloadButton("dl_anual",   "Caudal medio anual",
                         class = "btn btn-outline-primary btn-sm w-100"),
          hr(style = "margin:10px 0;"),
          uiOutput("ui_btn_logout")
        )
      )
    ),

    # ── Panel derecho ─────────────────────────────────────────────────────────
    column(9,
      uiOutput("ui_stats"),
      card(
        card_header(div(class = "card-header-dga", "\U0001f4cb Log de procesamiento")),
        card_body(padding = "0.5rem",
          div(class = "log-box", id = "log_container", uiOutput("ui_log"))
        )
      ),
      br(),
      navset_card_tab(
        id = "tabs_resultados",
        nav_panel(
          title = tagList(icon("table"), " Resumen por estación"),
          card_body(padding = "0",
            withSpinner(DTOutput("tabla_resumen"), type = 6, color = "#2c7fb8")
          )
        ),
        nav_panel(
          title = tagList(icon("chart-line"), " Gráfico de caudales"),
          card_body(
            fluidRow(
              column(3,
                div(
                  h6(tags$b("Seleccionar estaciones"),
                     tags$small(class = "text-muted ms-1", "(máx. 5)")),
                  div(class = "est-check-group", uiOutput("ui_check_estaciones")),
                  hr(),
                  h6(tags$b("Agregación temporal")),
                  selectInput("agr_tipo", label = NULL,
                              choices  = c("Diaria" = "dia", "Media mensual" = "mes",
                                           "Media anual" = "anio"),
                              selected = "dia"),
                  checkboxInput("escala_log", "Escala Y logarítmica", value = FALSE),
                  hr(),
                  actionButton("btn_deselect_all", "Deseleccionar todo",
                               class = "btn btn-outline-secondary btn-sm w-100")
                )
              ),
              column(9,
                withSpinner(plotlyOutput("grafico_caudales", height = "480px"),
                            type = 6, color = "#2c7fb8")
              )
            )
          )
        ),
        nav_panel(
          title = tagList(icon("database"), " Datos completos"),
          card_body(padding = "0",
            div(class = "text-end pe-3 pt-2", uiOutput("ui_badge_registros")),
            withSpinner(DTOutput("tabla_datos"), type = 6, color = "#2c7fb8")
          )
        ),
        nav_panel(
          title = tagList(icon("users-cog"), " Administración"),
          uiOutput("ui_panel_admin")
        )
      )
    )
  )
)

# =============================================================================
# UI principal — renderiza login o app según auth
# =============================================================================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly",
                   primary = "#2c7fb8", base_font = font_google("Inter")),
  tags$head(css_app),
  uiOutput("page_content")
)

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # ── Estado de autenticación ───────────────────────────────────────────────
  auth_ok      <- reactiveVal(FALSE)
  user_info_r  <- reactiveVal(NULL)
  login_msg    <- reactiveVal(NULL)

  # Renderiza login o app principal
  output$page_content <- renderUI({
    if (!auth_ok()) ui_login else ui_main
  })

  # ── Manejo de login ───────────────────────────────────────────────────────
  observeEvent(input$btn_login, {
    req(nchar(trimws(input$login_user)) > 0,
        nchar(input$login_pass) > 0)

    resultado <- tryCatch(
      credentials_supabase(trimws(input$login_user), input$login_pass),
      error = function(e) {
        message("Error Supabase: ", e$message)
        FALSE
      }
    )

    if (!isFALSE(resultado) && isTRUE(resultado$result)) {
      auth_ok(TRUE)
      user_info_r(resultado)
      login_msg(NULL)
    } else {
      login_msg("Usuario o contraseña incorrectos.")
    }
  })

  output$ui_login_error <- renderUI({
    msg <- login_msg()
    if (!is.null(msg))
      div(class = 'alert alert-danger mt-3 mb-0',
          icon("circle-exclamation"), " ", msg)
  })

  # ── Registro de nuevo usuario ────────────────────────────────────────────
  reg_msg <- reactiveVal(NULL)   # NULL | list(tipo, texto)

  output$ui_registro_msg <- renderUI({
    m <- reg_msg()
    if (is.null(m)) return(NULL)
    cls <- if (m$tipo == "ok") "alert alert-success mt-3 mb-0"
           else                "alert alert-danger  mt-3 mb-0"
    icn <- if (m$tipo == "ok") icon("circle-check") else icon("circle-exclamation")
    div(class = cls, icn, " ", m$texto)
  })

  observeEvent(input$btn_registrar, {
    reg_msg(NULL)

    user   <- trimws(input$reg_user)
    nombre <- trimws(input$reg_nombre)
    email  <- trimws(input$reg_email)
    pass1  <- input$reg_pass
    pass2  <- input$reg_pass2

    # ── Validaciones lado cliente ──────────────────────────────────────────
    if (nchar(user) == 0) {
      reg_msg(list(tipo="error", texto="El nombre de usuario es obligatorio.")); return()
    }
    if (!grepl("^[a-zA-Z0-9_.-]+$", user)) {
      reg_msg(list(tipo="error",
        texto="Usuario solo puede contener letras, n\u00fameros, guiones y puntos.")); return()
    }
    if (nchar(nombre) == 0) {
      reg_msg(list(tipo="error", texto="El nombre completo es obligatorio.")); return()
    }
    if (!grepl("^[^@]+@[^@]+\\.[^@]+$", email)) {
      reg_msg(list(tipo="error", texto="Ingresa un correo electr\u00f3nico v\u00e1lido.")); return()
    }
    if (nchar(pass1) < 8) {
      reg_msg(list(tipo="error", texto="La contrase\u00f1a debe tener al menos 8 caracteres.")); return()
    }
    if (pass1 != pass2) {
      reg_msg(list(tipo="error", texto="Las contrase\u00f1as no coinciden.")); return()
    }

    # ── Verificar que el usuario no exista ya ──────────────────────────────
    existentes <- tryCatch(
      as.data.frame(sb_get_usuarios()),
      error = function(e) data.frame()
    )
    if (is.data.frame(existentes) && nrow(existentes) > 0 &&
        user %in% existentes$username) {
      reg_msg(list(tipo="error",
        texto=paste0("El usuario '", user, "' ya est\u00e1 registrado."))); return()
    }

    # ── Crear en Supabase ──────────────────────────────────────────────────
    resultado <- tryCatch({
      sb_crear_usuario(user, pass1, nombre, email, rol = "consultor")
      TRUE
    }, error = function(e) {
      message("[REGISTRO] Error Supabase: ", e$message)
      e$message
    })

    if (isTRUE(resultado)) {
      sb_log(user, "ok", "register")
      reg_msg(list(tipo="ok",
        texto=paste0("\u2705 Cuenta creada. Ya puedes ingresar con el usuario '", user, "'.")))
      # Limpiar campos
      updateTextInput(session,     "reg_user",   value = "")
      updateTextInput(session,     "reg_nombre", value = "")
      updateTextInput(session,     "reg_email",  value = "")
      updateTextInput(session,     "reg_pass",   value = "")
      updateTextInput(session,     "reg_pass2",  value = "")
    } else {
      reg_msg(list(tipo="error",
        texto=paste0("Error al crear la cuenta: ", resultado)))
    }
  })

  # ── Logout ────────────────────────────────────────────────────────────────
  output$ui_btn_logout <- renderUI({
    req(auth_ok())
    info <- user_info_r()
    tagList(
      p(style = "font-size:.78rem; color:#666;",
        icon("user"), " ", strong(info$nombre %||% info$user),
        " (", info$rol, ")"),
      actionButton("btn_logout", tags$span(icon("right-from-bracket"), " Cerrar sesión"),
                   class = "btn btn-outline-danger btn-sm w-100")
    )
  })

  observeEvent(input$btn_logout, {
    auth_ok(FALSE)
    user_info_r(NULL)
    datos_compilados(NULL)
    log_mensajes(character(0))
  })

  # ── Helpers de info de usuario ────────────────────────────────────────────
  info_usuario <- reactive({ req(auth_ok()); user_info_r() })
  es_admin     <- reactive({ isTRUE(info_usuario()$rol == "admin") })

  # ── Panel de administración ───────────────────────────────────────────────
  output$ui_panel_admin <- renderUI({
    req(auth_ok())
    if (!es_admin()) {
      return(div(class = "alert alert-warning m-3",
                 icon("lock"), " Solo disponible para administradores."))
    }
    tagList(
      fluidRow(
        column(6,
          card(
            card_header(div(class = "card-header-dga", "Crear usuario")),
            card_body(
              textInput("nu_user",     "Nombre de usuario"),
              textInput("nu_nombre",   "Nombre completo"),
              textInput("nu_email",    "Email"),
              passwordInput("nu_pass", "Contraseña temporal"),
              selectInput("nu_rol", "Rol",
                          choices = c("consultor", "viewer", "admin")),
              actionButton("btn_crear_usuario", "Crear usuario",
                           class = "btn btn-primary w-100")
            )
          )
        ),
        column(6,
          card(
            card_header(div(class = "card-header-dga", "Restablecer contraseña")),
            card_body(
              textInput("rp_user",      "Usuario a resetear"),
              passwordInput("rp_nueva", "Nueva contraseña"),
              actionButton("btn_reset_pass", "Restablecer",
                           class = "btn btn-warning w-100"),
              hr(),
              h6("Usuarios activos"),
              withSpinner(DTOutput("tabla_usuarios"), type = 6, color = "#2c7fb8")
            )
          )
        )
      )
    )
  })

  observeEvent(input$btn_crear_usuario, {
    req(es_admin())
    # [FIX-APP-5] Validaciones previas antes de llamar a Supabase
    if (nchar(trimws(input$nu_user)) == 0) {
      showNotification("❌ El nombre de usuario es obligatorio.", type = "error"); return()
    }
    if (nchar(input$nu_pass) < 8) {
      showNotification("❌ La contraseña debe tener al menos 8 caracteres.", type = "error"); return()
    }
    tryCatch({
      sb_crear_usuario(input$nu_user, input$nu_pass,
                       input$nu_nombre, input$nu_email, input$nu_rol)
      sb_log(info_usuario()$user, "ok", "create")
      showNotification(paste("✅ Usuario", input$nu_user, "creado."), type = "message")
    }, error = function(e) {
      showNotification(paste("❌ Error:", e$message), type = "error")
    })
  })

  observeEvent(input$btn_reset_pass, {
    req(es_admin(), nchar(input$rp_user) > 0, nchar(input$rp_nueva) >= 8)
    tryCatch({
      sb_cambiar_password(input$rp_user, input$rp_nueva)
      sb_log(info_usuario()$user, "ok", "reset")
      showNotification(paste("✅ Contraseña de", input$rp_user, "actualizada."),
                       type = "message")
    }, error = function(e) {
      showNotification(paste("❌ Error:", e$message), type = "error")
    })
  })

  output$tabla_usuarios <- renderDT({
    req(es_admin())
    # [FIX-APP-4] sb_get_usuarios() puede retornar data.frame vacío; se maneja graciosamente.
    df <- tryCatch(as.data.frame(sb_get_usuarios()), error = function(e) data.frame())
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(datatable(data.frame(Mensaje = "Sin usuarios o error de conexión"),
                       rownames = FALSE))
    }
    cols_mostrar <- intersect(c("username", "nombre", "email", "rol", "created_at"), names(df))
    datatable(df[, cols_mostrar, drop = FALSE],
              rownames = FALSE,
              options = list(pageLength = 10, dom = "ft"))
  })

  # ══════════════════════════════════════════════════════════════════════════
  # LÓGICA PRINCIPAL (requiere autenticación)
  # ══════════════════════════════════════════════════════════════════════════

  archivos_df <- reactive({
    req(auth_ok(), input$archivos_input)
    input$archivos_input
  })

  output$ui_resumen_archivos <- renderUI({
    req(auth_ok())
    df <- archivos_df()
    if (is.null(df)) return(NULL)
    n  <- nrow(df)
    mb <- round(sum(df$size) / 1024^2, 2)
    div(style = "font-size:.78rem; color:#2c7fb8; margin-top:-8px; margin-bottom:4px;",
        icon("circle-check"),
        sprintf("  %d archivo%s · %.2f MB listos para procesar",
                n, if (n == 1) "" else "s", mb))
  })

  datos_compilados  <- reactiveVal(NULL)
  log_mensajes      <- reactiveVal(character(0))
  est_seleccionadas <- reactiveVal(character(0))

  agregar_log <- function(msg, tipo = "info") {
    color <- switch(tipo, ok="#4ec9b0", error="#f44747", warn="#dcdcaa", "#d4d4d4")
    ts    <- format(Sys.time(), "%H:%M:%S")
    html  <- sprintf('<span style="color:#858585;">[%s]</span> <span style="color:%s;">%s</span>',
                     ts, color, htmltools::htmlEscape(msg))
    log_mensajes(c(log_mensajes(), html))
  }

  observeEvent(input$btn_procesar, {
    req(auth_ok())
    df_files <- archivos_df()
    req(df_files, nrow(df_files) > 0)

    datos_compilados(NULL)
    log_mensajes(character(0))
    est_seleccionadas(character(0))
    agregar_log(sprintf("\U0001f4c1 Archivos recibidos: %d", nrow(df_files)))

    withProgress(message = "Procesando archivos DGA...", value = 0, {
      todos   <- list()
      errores <- 0L

      for (i in seq_len(nrow(df_files))) {
        archivo <- df_files$datapath[i]
        nombre  <- df_files$name[i]
        incProgress(1 / nrow(df_files),
                    detail = sprintf("(%d/%d) %s", i, nrow(df_files), nombre))
        agregar_log(sprintf("  ▶ [%d/%d] %s", i, nrow(df_files), nombre))

        res <- tryCatch({
          df <- leer_dga_caudales(archivo,
                                  log_fn = function(m) agregar_log(paste("   ", m), "warn"))
          df$ArchivoFuente <- nombre
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

  # ── Descargas ─────────────────────────────────────────────────────────────
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

  output$dl_diario  <- make_dl("diario",  function() datos_compilados())
  output$dl_mensual <- make_dl("mensual", function() {
    df <- datos_compilados(); req(df, nrow(df) > 0)
    df %>%
      filter(!is.na(Valor), !is.na(Fecha)) %>%
      mutate(Anio = year(Fecha), Mes = month(Fecha)) %>%
      group_by(Estacion, CodigoBNA, Cuenca, SubCuenca,
               Altitud_msnm, Latitud_S, Longitud_W, UTM_Norte, UTM_Este, AreaDrenaje_km2,
               Anio, Mes) %>%
      summarise(Q_medio_mensual = round(mean(Valor, na.rm = TRUE), 4),
                N_dias = n(), .groups = "drop") %>%
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
      summarise(Q_medio_anual = round(mean(Valor, na.rm=TRUE), 4),
                Q_max_anual   = round(max(Valor,  na.rm=TRUE), 4),
                Q_min_anual   = round(min(Valor,  na.rm=TRUE), 4),
                N_dias = n(), .groups = "drop") %>%
      arrange(Estacion, Anio)
  })

  # ── Stat boxes ─────────────────────────────────────────────────────────────
  output$ui_stats <- renderUI({
    req(auth_ok())
    df <- datos_compilados(); if (is.null(df) || !nrow(df)) return(NULL)
    sb <- function(v, l) div(class = "stat-box",
                             div(class = "stat-value", v),
                             div(class = "stat-label", l))
    rango <- if (any(!is.na(df$Fecha)))
      sprintf("%s — %s", format(min(df$Fecha, na.rm=TRUE), "%Y"),
              format(max(df$Fecha, na.rm=TRUE), "%Y")) else "—"
    card(card_body(fluidRow(
      column(3, sb(format(nrow(df), big.mark="."), "Registros totales")),
      column(3, sb(n_distinct(df$Estacion),        "Estaciones")),
      column(3, sb(n_distinct(df$Cuenca),          "Cuencas")),
      column(3, sb(rango,                          "Período"))
    )))
  })

  # ── Log ────────────────────────────────────────────────────────────────────
  output$ui_log <- renderUI({
    req(auth_ok())
    msgs <- log_mensajes()
    if (!length(msgs))
      return(HTML('<span style="color:#858585;">— Esperando procesamiento... —</span>'))
    tagList(
      HTML(paste(msgs, collapse = "<br>")),
      tags$script("var l=document.getElementById('log_container'); if(l) l.scrollTop=l.scrollHeight;")
    )
  })

  output$ui_badge_registros <- renderUI({
    req(auth_ok())
    df <- datos_compilados(); if (is.null(df)) return(NULL)
    span(class = "badge bg-secondary", format(nrow(df), big.mark="."), " filas")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # TAB 1 — RESUMEN POR ESTACIÓN
  # ══════════════════════════════════════════════════════════════════════════
  resumen_estaciones <- reactive({
    req(auth_ok())
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
        Anios_registro = as.integer(year(Fecha_fin) - year(Fecha_inicio) + 1L),
        Rango_fechas  = sprintf("%s → %s",
                                format(Fecha_inicio, "%d/%m/%Y"),
                                format(Fecha_fin,    "%d/%m/%Y"))
      ) %>%
      select(Estacion, CodigoBNA, Cuenca, SubCuenca,
             Altitud_msnm, Latitud_S, Longitud_W, UTM_Norte, UTM_Este, AreaDrenaje_km2,
             N_registros, Anios_registro, Rango_fechas, Q_media, Q_max, Q_min) %>%
      arrange(Cuenca, Estacion)
  })

  output$tabla_resumen <- renderDT({
    req(auth_ok())
    df <- resumen_estaciones()
    cols_show <- c("Estacion","CodigoBNA","Cuenca","SubCuenca",
                   "Altitud_msnm","Latitud_S","Longitud_W",
                   "UTM_Norte","UTM_Este","AreaDrenaje_km2",
                   "N_registros","Anios_registro","Rango_fechas","Q_media","Q_max","Q_min")
    datatable(
      df[, intersect(cols_show, names(df))],
      rownames = FALSE, filter = "top",
      class = "resumen-tabla compact stripe hover",
      extensions = c("Buttons","FixedHeader"),
      options = list(
        pageLength = 20, scrollX = TRUE, fixedHeader = TRUE,
        dom = "Bfrtip", buttons = c("csv","excel"),
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"),
        columnDefs = list(
          list(className = "dt-center",
               targets = which(cols_show %in%
                 c("CodigoBNA","N_registros","Anios_registro","Rango_fechas")) - 1),
          list(width = "90px",
               targets = which(cols_show %in%
                 c("Altitud_msnm","Latitud_S","Longitud_W",
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

  # ══════════════════════════════════════════════════════════════════════════
  # TAB 2 — SELECTOR DE ESTACIONES + GRÁFICO
  # ══════════════════════════════════════════════════════════════════════════
  output$ui_check_estaciones <- renderUI({
    req(auth_ok())
    df <- datos_compilados(); req(df, nrow(df) > 0)
    ests    <- sort(unique(df$Estacion))
    selec   <- est_seleccionadas()
    colores <- paleta_dinamica(length(ests))

    checkboxes <- lapply(seq_along(ests), function(i) {
      est     <- ests[i]
      color   <- colores[i]
      checked <- est %in% selec
      tags$div(class = "checkbox",
               tags$label(
                 tags$input(type = "checkbox", name = "est_graf", value = est,
                            checked = if (checked) "checked" else NULL,
                            style = "margin-right:4px;"),
                 tags$span(class = "est-dot",
                           style = sprintf("background-color:%s;", color)),
                 tags$span(est, style = "font-size:0.82rem; word-break:break-word;")
               ))
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

  observeEvent(input$est_graf_vals, {
    sel <- input$est_graf_vals
    if (is.null(sel)) sel <- character(0)
    est_seleccionadas(sel)
  }, ignoreNULL = FALSE)

  observeEvent(input$btn_deselect_all, {
    est_seleccionadas(character(0))
    output$ui_check_estaciones <- renderUI({
      req(auth_ok())
      df <- datos_compilados(); req(df)
      ests    <- sort(unique(df$Estacion))
      colores <- paleta_dinamica(length(ests))
      checkboxes <- lapply(seq_along(ests), function(i) {
        tags$div(class = "checkbox",
                 tags$label(
                   tags$input(type = "checkbox", name = "est_graf", value = ests[i],
                              style = "margin-right:4px;"),
                   tags$span(class = "est-dot",
                             style = sprintf("background-color:%s;", colores[i])),
                   tags$span(ests[i], style = "font-size:0.82rem;")
                 ))
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

  datos_grafico <- reactive({
    req(auth_ok())
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
      df_sel %>% select(Estacion, Fecha, Valor)
    )

    ests_todas <- sort(unique(datos_compilados()$Estacion))
    colores    <- paleta_dinamica(length(ests_todas))
    agr %>% mutate(Color = colores[match(Estacion, ests_todas)])
  })

  output$grafico_caudales <- renderPlotly({
    req(auth_ok())
    df_g <- datos_grafico()

    if (is.null(df_g) || !nrow(df_g)) {
      p_vacio <- ggplot() +
        annotate("text", x=0.5, y=0.5, size=5, color="#aaa",
                 label="Selecciona al menos una estación para visualizar") +
        theme_void()
      return(ggplotly(p_vacio))
    }

    titulo_agr <- switch(input$agr_tipo,
                         "dia"  = "Caudal medio diario",
                         "mes"  = "Caudal medio mensual",
                         "anio" = "Caudal medio anual")

    color_df       <- df_g %>% distinct(Estacion, Color)
    colores_usados <- setNames(color_df$Color, color_df$Estacion)

    p <- ggplot(df_g, aes(x=Fecha, y=Valor, color=Estacion, group=Estacion,
                          text=paste0("<b>", Estacion, "</b><br>",
                                      "Fecha: ", format(Fecha, "%d/%m/%Y"), "<br>",
                                      "Q: ", round(Valor, 3), " m³/s"))) +
      geom_line(linewidth=0.7, alpha=0.85) +
      scale_color_manual(values=colores_usados) +
      scale_x_date(date_labels="%b %Y", expand=expansion(mult=.02)) +
      labs(title=titulo_agr, x=NULL, y="Caudal (m³/s)", color=NULL) +
      theme_minimal(base_size=12) +
      theme(plot.title      = element_text(face="bold", color="#2c7fb8", size=13),
            legend.position = "bottom", legend.text = element_text(size=9),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(color="#e8e8e8"),
            axis.text.x = element_text(angle=30, hjust=1, size=8))

    if (input$escala_log) {
      p <- p + scale_y_log10(labels=label_comma())
    } else {
      p <- p + scale_y_continuous(labels=label_comma())
    }

    ggplotly(p, tooltip="text") %>%
      layout(legend   = list(orientation="h", y=-0.15, x=0),
             hovermode = "x unified",
             margin    = list(t=50, b=60)) %>%
      config(displayModeBar=TRUE,
             modeBarButtonsToRemove=c("lasso2d","select2d"),
             displaylogo=FALSE)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # TAB 3 — DATOS COMPLETOS
  # ══════════════════════════════════════════════════════════════════════════
  output$tabla_datos <- renderDT({
    req(auth_ok())
    df <- datos_compilados(); req(df, nrow(df) > 0)
    cols_num <- c("Altitud_msnm","Latitud_S","Longitud_W","UTM_Norte","UTM_Este",
                  "AreaDrenaje_km2","Valor")
    datatable(
      df, filter="top", rownames=FALSE,
      class = "compact stripe hover",
      extensions = "Buttons",
      options = list(
        pageLength=15, scrollX=TRUE,
        dom="Bfrtip", buttons=c("csv","excel"),
        language=list(url="//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json"),
        columnDefs=list(
          list(className="dt-center",
               targets=which(names(df) %in% c("Fecha","Indicador")) - 1)
        )
      )
    ) %>%
      formatRound(intersect(cols_num, names(df)), digits=3) %>%
      formatStyle("Valor",
                  background         = styleColorBar(range(df$Valor, na.rm=TRUE), "#d6e8f5"),
                  backgroundSize     = "100% 80%",
                  backgroundRepeat   = "no-repeat",
                  backgroundPosition = "center")
  })
}

# ─────────────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
