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

# ── Crear token de reset ───────────────────────────────────────────────────────
sb_crear_reset_token <- function(user) {
  resp <- sb_req("reset_tokens") |>
    req_body_json(list(username = user)) |>
    req_perform()

  .sb_check_resp(resp, "sb_crear_reset_token")
  resp_body_json(resp)
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
