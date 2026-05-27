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

# =============================================================================
# SISTEMA DE ROLES Y LÍMITES
# =============================================================================
# Roles disponibles:  "gratis" | "estandar" | "pro" | "admin"
# (Los registros nuevos desde la pantalla de login reciben "gratis" por defecto)

LIMITE_ARCHIVOS <- list(
  gratis   = 3L,
  estandar = 50L,   # compilaciones/mes — se controla en btn_procesar
  pro      = Inf,
  admin    = Inf
)

LIMITE_KB_ARCHIVO <- list(
  gratis   = 615,   # 615 KB por archivo
  estandar = Inf,
  pro      = Inf,
  admin    = Inf
)

LIMITE_COMPILACIONES_MES <- list(
  gratis   = Inf,   # sin límite mensual (solo límite de archivos y tamaño)
  estandar = 50L,
  pro      = Inf,
  admin    = Inf
)

# Helper: devuelve el rol normalizado (por si llega NULL o vacío)
rol_usuario <- function(info) {
  r <- tolower(trimws(info$rol %||% "gratis"))
  if (!r %in% c("gratis", "estandar", "pro", "admin")) "gratis" else r
}

# Helper: ¿puede este rol acceder a una función premium?
puede <- function(info, funcion) {
  r <- rol_usuario(info)
  switch(funcion,
    # Descarga mensual y anual — solo estandar / pro / admin
    dl_mensual  = r %in% c("estandar", "pro", "admin"),
    dl_anual    = r %in% c("estandar", "pro", "admin"),
    # Gráfico — solo estandar / pro / admin
    grafico     = r %in% c("estandar", "pro", "admin"),
    # Panel admin
    admin_panel = r == "admin",
    FALSE
  )
}

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
  .login-logo { max-height:200px; display:block; margin:0 auto 1rem auto; }
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
  /* ── Link olvidaste tu clave ── */
  .forgot-link {
    display:block; text-align:center; font-size:.8rem; color:#2c7fb8;
    margin-top:10px; background:none; border:none; padding:0;
    cursor:pointer; text-decoration:none;
  }
  .forgot-link:hover { text-decoration:underline; color:#1a5f8a; }
  /* Anular estilos de botón Shiny que hereda actionLink */
  a.forgot-link, a.forgot-link:focus, a.forgot-link:active {
    color:#2c7fb8 !important; background:none !important;
    box-shadow:none !important; outline:none !important;
  }
  /* ── Panel de reset dentro de login card ── */
  .reset-panel {
    background:#f0f7ff; border:1px solid #c8dff0;
    border-radius:6px; padding:16px; margin-top:14px;
  }
"))

# ── UI de login / registro ───────────────────────────────────────────────────────────────
ui_login <- div(class = "login-wrapper",
  div(class = "login-card",
    card(
      card_header(div(class = "card-header-dga", "\U0001f510  HIDROCOMP-CL")),
      card_body(
        tags$img(src = "static/logo.png", class = "login-logo",
                 alt = "Logo HIDROCOMP-CL"),
        p(class = "login-title", "Plataforma Compiladora de datos DGA"),

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
            uiOutput("ui_login_error"),
            # ── Link "Olvidaste tu clave" ──────────────────────────────────
            actionLink("link_olvide_clave",
                       label = tagList(icon("key"), " ¿Olvidaste tu clave?"),
                       class = "forgot-link d-block text-center mt-2"),
            # Panel que se despliega al hacer clic
            uiOutput("ui_panel_recuperar")
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

# ── UI de nueva contraseña (accedida desde el enlace del correo) ───────────────
ui_reset_pass <- div(class = "login-wrapper",
  div(class = "login-card",
    card(
      card_header(div(class = "card-header-dga",
                      "🔑  HIDROCOMP-CL — Nueva contraseña")),
      card_body(
        p(class = "login-subtitle",
          "Ingresa y confirma tu nueva contraseña para continuar."),
        uiOutput("ui_reset_token_info"),
        passwordInput("nueva_pass",  "Nueva contraseña",
                      placeholder = "Mínimo 8 caracteres"),
        passwordInput("nueva_pass2", "Confirmar contraseña",
                      placeholder = "Repite la contraseña"),
        actionButton("btn_set_nueva_pass",
                     tags$span(icon("lock"), " Guardar nueva contraseña"),
                     class = "btn btn-success w-100 mt-2 fw-bold"),
        uiOutput("ui_reset_result")
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
          h6("\u2b07  Descargar datos", class = "fw-bold"),
          p("Disponible tras procesar.", style = "font-size:.8rem; color:#666; margin-bottom:8px;"),
          uiOutput("ui_botones_descarga"),
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
            uiOutput("ui_tab_grafico")
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

  # Renderiza login, nueva contraseña (reset) o app principal
  output$page_content <- renderUI({
    token_url <- session$clientData$url_search
    has_token <- !is.null(token_url) && grepl("reset_token=", token_url)
    if (has_token && !auth_ok()) {
      ui_reset_pass
    } else if (!auth_ok()) {
      ui_login
    } else {
      ui_main
    }
  })

  # Extraer token de la URL
  reset_token_url <- reactive({
    q <- session$clientData$url_search
    if (is.null(q) || !grepl("reset_token=", q)) return(NULL)
    sub(".*[?&]reset_token=([^&]+).*", "\\1", q)
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
      sb_crear_usuario(user, pass1, nombre, email, rol = "gratis")
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

  # ── Recuperación de contraseña ───────────────────────────────────────────────

  estado_recuperar <- reactiveVal(NULL)
  recuperar_msg    <- reactiveVal(NULL)

  observeEvent(input$link_olvide_clave, {
    est_actual <- isolate(estado_recuperar())
    if (is.null(est_actual) || est_actual == "enviado") {
      estado_recuperar("form")
      recuperar_msg(NULL)
    } else {
      estado_recuperar(NULL)
    }
  }, ignoreInit = TRUE)

  output$ui_panel_recuperar <- renderUI({
    est <- estado_recuperar()
    if (is.null(est)) return(NULL)
    if (est == "enviado") {
      return(div(
        class = "reset-panel",
        div(class = "alert alert-success mb-2",
            icon("circle-check"),
            " Revisa tu correo. Si el email está registrado recibirás las instrucciones en breve."),
        tags$small(style = "color:#888;", "No olvides revisar la carpeta de spam.")
      ))
    }
    div(
      class = "reset-panel",
      tags$p(style = "font-size:.82rem; color:#555; margin-bottom:10px;",
             icon("envelope"), " Ingresa el correo con el que te registraste:"),
      textInput("recuperar_email", label = NULL,
                placeholder = "correo@ejemplo.com", width = "100%"),
      actionButton("btn_recuperar", "Enviar enlace de recuperación",
                   class = "btn btn-outline-primary btn-sm w-100 mt-1"),
      uiOutput("ui_recuperar_msg")
    )
  })

  output$ui_recuperar_msg <- renderUI({
    m <- recuperar_msg()
    if (is.null(m)) return(NULL)
    cls <- if (m$tipo == "error") "alert alert-danger mt-2 mb-0 py-2"
           else                   "alert alert-info mt-2 mb-0 py-2"
    div(class = cls, style = "font-size:.82rem;", m$texto)
  })

  observeEvent(input$btn_recuperar, {
    recuperar_msg(NULL)
    email_input <- trimws(input$recuperar_email)

    if (nchar(email_input) == 0 || !grepl("^[^@]+@[^@]+\\.[^@]+$", email_input)) {
      recuperar_msg(list(tipo = "error",
                         texto = "Por favor ingresa un correo electrónico válido."))
      return()
    }

    usuario <- tryCatch(sb_buscar_por_email(email_input), error = function(e) NULL)

    if (is.null(usuario)) {
      recuperar_msg(list(tipo = "error",
        texto = paste0(
          "No hay cuenta asociada a este correo. ",
          "Por favor intenta otro correo o crea una cuenta con este correo."
        )
      ))
      return()
    }

    envio_ok <- tryCatch({
      token <- sb_crear_reset_token(usuario$username)
      resultado_envio <- sb_enviar_email_reset(
        nombre        = usuario$nombre,
        email_destino = usuario$email,
        username      = usuario$username,
        token         = token
      )
      sb_log(usuario$username, if (isTRUE(resultado_envio)) "ok" else "email_fail",
             "recovery_request")
      isTRUE(resultado_envio)
    }, error = function(e) {
      message("[RECOVERY] Error inesperado: ", e$message)
      FALSE
    })

    if (!envio_ok) {
      # El token está guardado pero el correo no salió.
      # Mostramos aviso técnico sin bloquear al usuario.
      showNotification(
        tagList(
          icon("triangle-exclamation"), " ",
          "El enlace fue generado pero no se pudo enviar el correo. ",
          "Revisa que RESEND_API_KEY y EMAIL_FROM estén configurados correctamente ",
          "y que el dominio remitente esté verificado en resend.com."
        ),
        type = "warning", duration = 15
      )
    }

    # Mensaje neutro al usuario: no revelar si el correo existe o no
    estado_recuperar("enviado")
  })

  # ── Pantalla de nueva contraseña (token en URL) ───────────────────────────

  reset_resultado <- reactiveVal(NULL)

  output$ui_reset_token_info <- renderUI({
    token <- reset_token_url()
    if (is.null(token)) {
      return(div(class = "alert alert-danger",
                 icon("triangle-exclamation"),
                 " Enlace inválido o expirado. Solicita uno nuevo desde la pantalla de inicio."))
    }
    info <- tryCatch(sb_validar_reset_token(token), error = function(e) NULL)
    if (is.null(info)) {
      return(div(class = "alert alert-danger",
                 icon("triangle-exclamation"),
                 " Este enlace ha expirado o ya fue usado. Solicita uno nuevo."))
    }
    div(class = "alert alert-info mb-3",
        icon("user"),
        sprintf(" Restableciendo contraseña para el usuario: %s", info$username))
  })

  output$ui_reset_result <- renderUI({
    r <- reset_resultado()
    if (is.null(r)) return(NULL)
    switch(r,
      ok = div(class = "alert alert-success mt-3",
               icon("circle-check"),
               " ¡Contraseña actualizada! ",
               tags$a(href = "/", "Haz clic aquí para iniciar sesión.")),
      expirado = div(class = "alert alert-danger mt-3",
                     icon("triangle-exclamation"),
                     " Este enlace ha expirado o ya fue usado. Solicita uno nuevo."),
      div(class = "alert alert-danger mt-3",
          icon("triangle-exclamation"),
          " Error al guardar. Intenta nuevamente.")
    )
  })

  observeEvent(input$btn_set_nueva_pass, {
    reset_resultado(NULL)
    token <- reset_token_url()
    if (is.null(token)) { reset_resultado("expirado"); return() }

    pass1 <- input$nueva_pass
    pass2 <- input$nueva_pass2

    if (nchar(pass1) < 8) {
      showNotification("La contraseña debe tener al menos 8 caracteres.", type = "warning")
      return()
    }
    if (pass1 != pass2) {
      showNotification("Las contraseñas no coinciden.", type = "warning")
      return()
    }

    info <- tryCatch(sb_validar_reset_token(token), error = function(e) NULL)
    if (is.null(info)) { reset_resultado("expirado"); return() }

    resultado <- tryCatch({
      sb_cambiar_password(info$username, pass1)
      sb_consumir_reset_token(token)
      sb_log(info$username, "ok", "reset_password")
      "ok"
    }, error = function(e) { message("[RESET_PASS] ", e$message); "error" })

    reset_resultado(resultado)
  })

  # ── Logout ────────────────────────────────────────────────────────────────
  output$ui_btn_logout <- renderUI({
    req(auth_ok())
    info <- user_info_r()
    tagList(
      # Badge de rol con color
      {
        rol_label <- switch(rol_usuario(info),
          gratis   = tagList(span(class="badge bg-secondary", "Gratis")),
          estandar = tagList(span(class="badge bg-primary",   "Estándar")),
          pro      = tagList(span(class="badge bg-success",   "Pro")),
          admin    = tagList(span(class="badge bg-danger",    "Admin")),
          span(class="badge bg-secondary", info$rol)
        )
        p(style = "font-size:.78rem; color:#666;",
          icon("user"), " ", strong(info$nombre %||% info$user), " ", rol_label)
      },
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

  # ── Helpers de rol ────────────────────────────────────────────────────────
  rol_actual   <- reactive({ rol_usuario(info_usuario()) })
  es_gratis    <- reactive({ rol_actual() == "gratis" })

  # Contador de compilaciones del mes actual (para rol estandar)
  compilaciones_mes <- reactiveVal(0L)

  # Reiniciar contador al iniciar sesión
  observeEvent(auth_ok(), {
    if (auth_ok()) compilaciones_mes(0L)
  })

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
                          choices = c("gratis" = "gratis", "Estándar" = "estandar", "Pro" = "pro", "Administrador" = "admin")),
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
                           class = "btn btn-warning w-100")
            )
          ),
          br(),
          card(
            card_header(div(class = "card-header-dga", "Editar rol de usuario")),
            card_body(
              textInput("er_user", "Nombre de usuario"),
              selectInput("er_rol", "Nuevo rol",
                          choices = c("Gratis"        = "gratis",
                                      "Estándar" = "estandar",
                                      "Pro"           = "pro",
                                      "Administrador" = "admin")),
              actionButton("btn_cambiar_rol", "Actualizar rol",
                           class = "btn btn-info w-100 text-white")
            )
          )
        )
      ),
      br(),
      fluidRow(
        column(12,
          card(
            card_header(div(class = "card-header-dga", "Usuarios registrados")),
            card_body(padding="0",
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

  # ── Editar rol de usuario ─────────────────────────────────────────────────
  observeEvent(input$btn_cambiar_rol, {
    req(es_admin(), nchar(trimws(input$er_user)) > 0, nchar(input$er_rol) > 0)
    tryCatch({
      sb_cambiar_rol(input$er_user, input$er_rol)
      sb_log(info_usuario()$user, "ok", "change_rol")
      showNotification(
        paste("✅ Rol de", input$er_user, "actualizado a", input$er_rol),
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

    rol    <- rol_actual()
    lim_n  <- LIMITE_ARCHIVOS[[rol]]
    lim_kb <- LIMITE_KB_ARCHIVO[[rol]]
    n      <- nrow(df)
    msgs   <- list()

    if (is.finite(lim_n) && n > lim_n) {
      msgs[[length(msgs)+1]] <- div(
        class="alert alert-danger py-1 px-2 mb-1", style="font-size:.78rem;",
        icon("triangle-exclamation"),
        sprintf(" Plan %s: máx. %d archivo%s. Solo se procesarán los primeros.",
                toupper(rol), lim_n, if (lim_n==1) "" else "s"))
    }
    if (is.finite(lim_kb)) {
      grandes <- df$name[df$size > lim_kb * 1024]
      if (length(grandes) > 0)
        msgs[[length(msgs)+1]] <- div(
          class="alert alert-danger py-1 px-2 mb-1", style="font-size:.78rem;",
          icon("triangle-exclamation"),
          sprintf(" %d archivo%s supera%s el límite de %d KB y será%s ignorado%s.",
                  length(grandes), if(length(grandes)==1)""else"s",
                  if(length(grandes)==1)""else"n", lim_kb,
                  if(length(grandes)==1)""else"n", if(length(grandes)==1)""else"s"))
    }

    mb      <- round(sum(df$size)/1024^2, 2)
    resumen <- div(
      style="font-size:.78rem; color:#2c7fb8; margin-top:-8px; margin-bottom:4px;",
      icon("circle-check"),
      sprintf("  %d archivo%s · %.2f MB cargados", n, if (n==1) "" else "s", mb))

    if (is.finite(lim_n)) {
      hay_error <- n > lim_n || (is.finite(lim_kb) && any(df$size > lim_kb*1024))
      resumen <- tagList(resumen,
        div(style=sprintf("font-size:.72rem; color:%s; margin-bottom:4px;",
                          if (hay_error) "#dc3545" else "#6c757d"),
            icon("circle-info"),
            sprintf(" Plan %s: máx. %d archivo%s · %d KB c/u",
                    toupper(rol), lim_n, if (lim_n==1) "" else "s", lim_kb)))
    }
    tagList(msgs, resumen)
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

    rol  <- rol_actual()

    # ── Validar límite de compilaciones mensuales (rol estandar) ─────────────
    lim_mes <- LIMITE_COMPILACIONES_MES[[rol]]
    if (is.finite(lim_mes) && compilaciones_mes() >= lim_mes) {
      showNotification(
        sprintf("\u26a0 Alcanzaste el l\u00edmite de %d compilaciones este mes (plan Est\u00e1ndar).", lim_mes),
        type = "warning", duration = 8)
      return()
    }

    # ── Filtrar archivos según límites del rol ────────────────────────────────
    lim_n  <- LIMITE_ARCHIVOS[[rol]]
    lim_kb <- LIMITE_KB_ARCHIVO[[rol]]

    # Filtrar por tamaño primero
    if (is.finite(lim_kb)) {
      grandes <- df_files$size > lim_kb * 1024
      if (any(grandes)) {
        nombres_grandes <- df_files$name[grandes]
        df_files <- df_files[!grandes, ]
        showNotification(
          paste0("\u26a0 Archivos ignorados por superar ", lim_kb, " KB: ",
                 paste(nombres_grandes, collapse=", ")),
          type = "warning", duration = 8)
      }
    }

    # Limitar cantidad de archivos
    if (is.finite(lim_n) && nrow(df_files) > lim_n) {
      showNotification(
        sprintf("\u26a0 Solo se procesar\u00e1n los primeros %d archivo%s (l\u00edmite plan %s).",
                lim_n, if (lim_n==1) "" else "s", toupper(rol)),
        type = "warning", duration = 8)
      df_files <- df_files[seq_len(lim_n), ]
    }

    if (nrow(df_files) == 0) {
      showNotification("\u274c Ning\u00fan archivo v\u00e1lido para procesar con tu plan actual.",
                       type = "error", duration = 8)
      return()
    }

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

        compilaciones_mes(compilaciones_mes() + 1L)
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


  # ── Botones de descarga dinámicos (según rol) ─────────────────────────────
  output$ui_botones_descarga <- renderUI({
    req(auth_ok())
    info <- info_usuario()

    btn_bloqueado <- function(label) {
      div(style = "position:relative; margin-bottom:8px;",
        tags$button(label, disabled = "disabled",
                    class = "btn btn-outline-secondary btn-sm w-100",
                    style = "opacity:0.5; cursor:not-allowed;"),
        div(style = "position:absolute; right:8px; top:50%; transform:translateY(-50%);
                     font-size:.7rem; color:#999;",
            icon("lock"), " Solo plan Estándar+"))
    }

    if (puede(info, "dl_mensual")) {
      btn_mensual <- tagList(
        downloadButton("dl_mensual", "Caudal medio mensual",
                       class = "btn btn-outline-primary btn-sm w-100 mb-2"), br())
    } else {
      btn_mensual <- btn_bloqueado("Caudal medio mensual")
    }

    if (puede(info, "dl_anual")) {
      btn_anual <- downloadButton("dl_anual", "Caudal medio anual",
                                  class = "btn btn-outline-primary btn-sm w-100")
    } else {
      btn_anual <- btn_bloqueado("Caudal medio anual")
    }

    tagList(
      downloadButton("dl_diario", "Caudal diario",
                     class = "btn btn-outline-primary btn-sm w-100 mb-2"),
      br(),
      btn_mensual,
      btn_anual
    )
  })

  # ── Tab gráfico dinámico (bloqueado para rol gratis) ──────────────────────
  output$ui_tab_grafico <- renderUI({
    req(auth_ok())
    if (!puede(info_usuario(), "grafico")) {
      return(div(
        class = "alert alert-warning m-4 text-center",
        style = "border-left: 5px solid #f0ad4e;",
        icon("lock"), tags$strong(" Función exclusiva de plan Estándar o superior"),
        br(), br(),
        tags$span(style = "font-size:.88rem; color:#666;",
          "Actualiza tu plan para visualizar series de tiempo,
           comparar estaciones y exportar gráficos interactivos."),
        br(), br(),
        tags$span(class = "badge bg-warning text-dark",
                  style = "font-size:.8rem;",
                  icon("arrow-up"), " Mejora tu plan")
      ))
    }

    fluidRow(
      column(3,
        div(
          h6(tags$b("Seleccionar estaciones"),
             tags$small(class = "text-muted ms-1", "(máx. 5)")),
          div(class = "est-check-group", uiOutput("ui_check_estaciones")),
          hr(),
          h6(tags$b("Agregación temporal")),
          selectInput("agr_tipo", label = NULL,
                      choices  = c("Diaria" = "dia",
                                   "Media mensual" = "mes",
                                   "Media anual"   = "anio"),
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
    info <- info_usuario()
    rol  <- rol_usuario(info)
    # Mostrar contador de compilaciones para rol estándar
    extra_stat <- if (rol == "estandar") {
      lim <- LIMITE_COMPILACIONES_MES[["estandar"]]
      restantes <- max(0L, lim - compilaciones_mes())
      sb(sprintf("%d / %d", compilaciones_mes(), lim),
         "Compilaciones este mes")
    } else NULL

    col_periodo <- if (!is.null(extra_stat)) column(3, extra_stat) else
                  column(3, sb(rango, "Período"))
    card(card_body(fluidRow(
      column(3, sb(format(nrow(df), big.mark="."), "Registros totales")),
      column(3, sb(n_distinct(df$Estacion),        "Estaciones")),
      column(3, sb(n_distinct(df$Cuenca),          "Cuencas")),
      col_periodo
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
