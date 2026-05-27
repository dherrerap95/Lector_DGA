# supabase_utils.R
# ─────────────────────────────────────────────────────────────────────────────
# CORRECCIONES APLICADAS:
#   [FIX-SB-1] SB_URL y SB_KEY se leen en tiempo de llamada (no en carga),
#              para que funcionen correctamente en shinyapps.io / Posit Connect
#              donde las variables de entorno se setean DESPUÉS de cargar el paquete.
#   [FIX-SB-2] sb_req() valida que URL y KEY no estén vacíos antes de construir
#              el request, con mensaje de error accionable.
#   [FIX-SB-3] sb_get_usuarios() maneja errores HTTP (4xx/5xx) explícitamente,
#              devuelve data.frame vacío en lugar de lanzar excepción sin contexto.
#   [FIX-SB-4] Todas las funciones de escritura retornan invisible(TRUE/FALSE)
#              para que app.R pueda verificar éxito sin capturar la respuesta raw.
#   [FIX-SB-5] Se agrega req_error(is_error = \(r) FALSE) para que httr2 NO lance
#              error automáticamente; así podemos inspeccionar el status nosotros.
#   [FIX-SB-6] sb_log() es silencioso por diseño (no debe romper el flujo auth).
# ─────────────────────────────────────────────────────────────────────────────

library(httr2)
library(jsonlite)
library(digest)   # necesario para digest::digest()

# ── Helper: leer credenciales en tiempo de ejecución ─────────────────────────
.sb_creds <- function() {
  url <- Sys.getenv("SUPABASE_URL")
  key <- Sys.getenv("SUPABASE_KEY")

  if (nchar(url) == 0 || nchar(key) == 0) {
    stop(
      "Variables de entorno SUPABASE_URL y/o SUPABASE_KEY no definidas.\n",
      "  Agrega en .Renviron (local) o en el panel de entorno de shinyapps.io:\n",
      "    SUPABASE_URL=https://<tu-proyecto>.supabase.co\n",
      "    SUPABASE_KEY=<tu-anon-key>\n",
      call. = FALSE
    )
  }
  list(url = url, key = key)
}

# ── Helper base: construye un request autenticado ─────────────────────────────
sb_req <- function(tabla) {
  creds <- .sb_creds()   # [FIX-SB-1] lectura diferida
  request(paste0(creds$url, "/rest/v1/", tabla)) |>
    req_headers(
      "apikey"        = creds$key,
      "Authorization" = paste("Bearer", creds$key),
      "Content-Type"  = "application/json",
      "Prefer"        = "return=representation"
    ) |>
    req_error(is_error = \(resp) FALSE)  # [FIX-SB-5] control manual de errores HTTP
}

# ── Helper: verificar status HTTP y lanzar error legible ─────────────────────
.sb_check_resp <- function(resp, contexto = "Supabase") {
  status <- resp_status(resp)
  if (status >= 400) {
    body <- tryCatch(resp_body_string(resp), error = function(e) "(sin cuerpo)")
    stop(sprintf("[%s] HTTP %d: %s", contexto, status, body), call. = FALSE)
  }
  invisible(resp)
}

# ── Obtener todos los usuarios (para check_credentials) ──────────────────────
sb_get_usuarios <- function() {
  resp <- tryCatch({
    sb_req("usuarios") |>
      req_url_query(
        activo = "eq.true",
        select = "username,password_hash,nombre,email,rol,created_at"
      ) |>
      req_perform()
  }, error = function(e) {
    warning("[sb_get_usuarios] Error de red: ", e$message)
    return(NULL)
  })

  if (is.null(resp)) return(data.frame())  # [FIX-SB-3] fallo silencioso → df vacío

  tryCatch({
    .sb_check_resp(resp, "sb_get_usuarios")
    resultado <- fromJSON(resp_body_string(resp))
    # fromJSON puede devolver lista vacía si no hay filas
    if (is.null(resultado) || length(resultado) == 0) return(data.frame())
    as.data.frame(resultado)
  }, error = function(e) {
    warning("[sb_get_usuarios] ", e$message)
    data.frame()
  })
}

# ── Crear usuario ──────────────────────────────────────────────────────────────
sb_crear_usuario <- function(user, password, nombre, email, rol = "consultor") {
  if (nchar(trimws(user)) == 0)     stop("El nombre de usuario no puede estar vacío.", call. = FALSE)
  if (nchar(password) < 8)          stop("La contraseña debe tener al menos 8 caracteres.", call. = FALSE)

  hash <- digest::digest(password, algo = "sha256")
  body <- list(username = user, password_hash = hash,
               nombre = nombre, email = email, rol = rol, activo = TRUE)

  resp <- sb_req("usuarios") |>
    req_body_json(body) |>
    req_perform()

  .sb_check_resp(resp, "sb_crear_usuario")
  invisible(TRUE)  # [FIX-SB-4]
}

# ── Cambiar contraseña ─────────────────────────────────────────────────────────
sb_cambiar_password <- function(user, nueva_password) {
  if (nchar(nueva_password) < 8)
    stop("La nueva contraseña debe tener al menos 8 caracteres.", call. = FALSE)

  hash <- digest::digest(nueva_password, algo = "sha256")

  resp <- sb_req("usuarios") |>
    req_url_query(username = paste0("eq.", user)) |>
    req_method("PATCH") |>
    req_body_json(list(
      password_hash = hash,
      updated_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )) |>
    req_perform()

  .sb_check_resp(resp, "sb_cambiar_password")
  invisible(TRUE)
}

# ── Desactivar usuario ─────────────────────────────────────────────────────────
sb_desactivar_usuario <- function(user) {
  resp <- sb_req("usuarios") |>
    req_url_query(username = paste0("eq.", user)) |>
    req_method("PATCH") |>
    req_body_json(list(activo = FALSE)) |>
    req_perform()

  .sb_check_resp(resp, "sb_desactivar_usuario")
  invisible(TRUE)
}

# ── Buscar usuario por email ─────────────────────────────────────────────
sb_buscar_por_email <- function(email) {
  resp <- tryCatch(
    sb_req("usuarios") |>
      req_url_query(
        email  = paste0("eq.", tolower(trimws(email))),
        activo = "eq.true",
        select = "username,nombre,email"
      ) |>
      req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NULL)
  tryCatch({
    .sb_check_resp(resp, "sb_buscar_por_email")
    res <- fromJSON(resp_body_string(resp))
    if (is.null(res) || length(res) == 0) return(NULL)
    df <- as.data.frame(res)
    if (nrow(df) == 0) return(NULL)
    as.list(df[1, ])
  }, error = function(e) NULL)
}

# ── Crear token de reset (SHA-256 aleatorio + expiración 2 h) ───────────────────
sb_crear_reset_token <- function(username) {
  token   <- digest(paste0(username, as.numeric(Sys.time()), runif(1)), algo = "sha256")
  expires <- format(Sys.time() + 2 * 3600, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  # Borrar tokens anteriores del mismo usuario
  try(silent = TRUE, {
    sb_req("reset_tokens") |>
      req_url_query(username = paste0("eq.", username)) |>
      req_method("DELETE") |>
      req_perform()
  })

  resp <- sb_req("reset_tokens") |>
    req_body_json(list(
      username   = username,
      token      = token,
      expires_at = expires,
      usado      = FALSE
    )) |>
    req_perform()

  .sb_check_resp(resp, "sb_crear_reset_token")
  token
}

# ── Validar token de reset ────────────────────────────────────────────────────
# Devuelve list(username) si válido, NULL si expirado/inexistente/ya usado
sb_validar_reset_token <- function(token) {
  if (nchar(trimws(token)) == 0) return(NULL)
  resp <- tryCatch(
    sb_req("reset_tokens") |>
      req_url_query(
        token  = paste0("eq.", token),
        usado  = "eq.false",
        select = "username,expires_at"
      ) |>
      req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NULL)
  tryCatch({
    .sb_check_resp(resp, "sb_validar_reset_token")
    res <- fromJSON(resp_body_string(resp))
    if (is.null(res) || length(res) == 0) return(NULL)
    df <- as.data.frame(res)
    if (nrow(df) == 0) return(NULL)
    expires <- as.POSIXct(df$expires_at[1], format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    if (is.na(expires) || Sys.time() > expires) return(NULL)
    list(username = as.character(df$username[1]))
  }, error = function(e) NULL)
}

# ── Marcar token como usado ───────────────────────────────────────────────────────
sb_consumir_reset_token <- function(token) {
  resp <- sb_req("reset_tokens") |>
    req_url_query(token = paste0("eq.", token)) |>
    req_method("PATCH") |>
    req_body_json(list(usado = TRUE)) |>
    req_perform()
  .sb_check_resp(resp, "sb_consumir_reset_token")
  invisible(TRUE)
}

# ── Enviar email de recuperación vía Resend API ──────────────────────────────────
# Variables de entorno requeridas:
#   RESEND_API_KEY  — API key de resend.com
#   APP_URL         — URL pública de la app (ej: https://usuario.shinyapps.io/app)
#   EMAIL_FROM      — (opcional) remitente verificado en Resend.
#                     Si no se define, usa onboarding@resend.dev (sólo envía a tu propio correo en plan free).
#
# Devuelve: invisible(TRUE) si el envio fue exitoso, invisible(FALSE) si falló.
# En caso de fallo imprime el motivo con message() para que aparezca en el log de la app.
sb_enviar_email_reset <- function(nombre, email_destino, username, token) {

  # ── 1. Leer y validar configuración ──────────────────────────────────────────
  api_key <- Sys.getenv("RESEND_API_KEY")
  app_url <- Sys.getenv("APP_URL",    unset = "http://localhost:3838")
  from_addr <- Sys.getenv("EMAIL_FROM", unset = "")

  # Remitente por defecto: dominio sandbox de Resend (funciona sin verificar dominio)
  if (nchar(from_addr) == 0) {
    from_addr <- "onboarding@resend.dev"
  }

  if (nchar(api_key) == 0) {
    message("[EMAIL] ERROR: variable RESEND_API_KEY no está configurada.",
            " Agárgala en .Renviron o en el panel de shinyapps.io.")
    return(invisible(FALSE))
  }

  # ── 2. Construir enlace y cuerpo HTML ─────────────────────────────────────
  link <- paste0(app_url, "?reset_token=", token)

  html_body <- paste0(
    "<!DOCTYPE html><html lang='es'><head><meta charset='UTF-8'></head>",
    "<body style='font-family:Arial,sans-serif;max-width:560px;margin:auto;",
    "padding:24px;color:#333;'>",
    "<div style='background:#0d2b45;border-radius:8px 8px 0 0;padding:20px 24px;'>",
    "<h2 style='color:#fff;margin:0;font-size:1.1rem;'>",
    "HIDROCOMP-CL — Recuperación de contraseña</h2></div>",
    "<div style='border:1px solid #ddd;border-top:none;",
    "border-radius:0 0 8px 8px;padding:24px;'>",
    "<p>Hola <strong>", htmltools::htmlEscape(nombre), "</strong>,</p>",
    "<p>Recibimos una solicitud para restablecer la contraseña de tu cuenta.</p>",
    "<p>ℹ️ Tu nombre de usuario es: ",
    "<strong style='color:#2c7fb8;'>", htmltools::htmlEscape(username), "</strong></p>",
    "<p>Haz clic en el botón para crear una nueva contraseña:</p>",
    "<div style='text-align:center;margin:28px 0;'>",
    "<a href='", link, "' style='background:#2c7fb8;color:#fff;padding:12px 32px;",
    "border-radius:6px;text-decoration:none;font-weight:bold;font-size:1rem;'>",
    "Restablecer contraseña</a></div>",
    "<p style='font-size:.82rem;color:#888;'>",
    "Este enlace es válido por <strong>2 horas</strong>. ",
    "Si no solicitaste este cambio, ignora este correo con tranquilidad.</p>",
    "<p style='font-size:.82rem;color:#888;'>",
    "Si el botón no funciona, copia y pega este enlace en tu navegador:<br>",
    "<span style='color:#2c7fb8;word-break:break-all;'>", link, "</span></p>",
    "<hr style='border:none;border-top:1px solid #eee;margin:20px 0;'>",
    "<p style='font-size:.75rem;color:#bbb;text-align:center;'>",
    "HIDROCOMP-CL — Plataforma de Caudales DGA Chile</p>",
    "</div></body></html>"
  )

  # ── 3. Llamar a la API de Resend ──────────────────────────────────────────
  message(sprintf("[EMAIL] Enviando a %s via %s ...", email_destino, from_addr))

  resp <- tryCatch(
    request("https://api.resend.com/emails") |>
      req_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type"  = "application/json"
      ) |>
      req_body_json(list(
        from    = from_addr,
        to      = list(email_destino),
        subject = "HIDROCOMP-CL — Recupera tu contraseña",
        html    = html_body
      )) |>
      req_error(is_error = function(r) FALSE) |>
      req_timeout(10) |>
      req_perform(),
    error = function(e) {
      message("[EMAIL] Error de red: ", e$message)
      NULL
    }
  )

  # ── 4. Verificar resultado ────────────────────────────────────────────────
  if (is.null(resp)) {
    message("[EMAIL] Fallo: no se recibió respuesta de Resend.")
    return(invisible(FALSE))
  }

  status <- resp_status(resp)
  if (status >= 400) {
    body <- tryCatch(resp_body_string(resp), error = function(e) "(sin cuerpo)")
    message(sprintf(
      "[EMAIL] Fallo HTTP %d. Causas comunes:
",
      status,
      "  401 -> RESEND_API_KEY incorrecta o expirada
",
      "  403 -> dominio del remitente no verificado en resend.com
",
      "       Usa EMAIL_FROM=onboarding@resend.dev mientras tanto
",
      "  422 -> formato de email inválido
",
      "  Respuesta de Resend: ", body
    ))
    return(invisible(FALSE))
  }

  message(sprintf("[EMAIL] OK (HTTP %d) — correo enviado a %s", status, email_destino))
  invisible(TRUE)
}

# ── Registrar log de acceso ────────────────────────────────────────────────────
# [FIX-SB-6] Completamente silencioso: nunca debe interrumpir el flujo de auth.
sb_log <- function(user, resultado, accion = "login") {
  try({
    resp <- sb_req("log_accesos") |>
      req_body_json(list(
        username  = user,
        resultado = resultado,
        accion    = accion,
        timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      )) |>
      req_perform()
    # No llamamos .sb_check_resp para que un error de log no propague
  }, silent = TRUE)
  invisible(NULL)
}

# ── Cambiar rol de usuario ─────────────────────────────────────────────────────
sb_cambiar_rol <- function(user, nuevo_rol) {
  roles_validos <- c("gratis", "estandar", "pro", "admin")
  if (!nuevo_rol %in% roles_validos)
    stop(sprintf("Rol inválido: '%s'. Debe ser uno de: %s",
                 nuevo_rol, paste(roles_validos, collapse=", ")), call.=FALSE)

  resp <- sb_req("usuarios") |>
    req_url_query(username = paste0("eq.", user)) |>
    req_method("PATCH") |>
    req_body_json(list(
      rol        = nuevo_rol,
      updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )) |>
    req_perform()

  .sb_check_resp(resp, "sb_cambiar_rol")
  invisible(TRUE)
}
