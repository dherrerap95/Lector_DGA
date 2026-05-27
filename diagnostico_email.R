# =============================================================================
# diagnostico_email.R
# Ejecuta este script en tu consola de R para diagnosticar el problema de email.
# NO lo subas a shinyapps.io — es solo para pruebas locales.
# =============================================================================

library(httr2)
library(jsonlite)

cat("=== DIAGNÓSTICO DE EMAIL HIDROCOMP-CL ===\n\n")

# ── 1. Variables de entorno ───────────────────────────────────────────────────
api_key   <- Sys.getenv("RESEND_API_KEY")
app_url   <- Sys.getenv("APP_URL",    unset = "http://localhost:3838")
from_addr <- Sys.getenv("EMAIL_FROM", unset = "")

cat("1. Variables de entorno:\n")
cat(sprintf("   RESEND_API_KEY : %s\n",
    if (nchar(api_key) == 0) "❌ NO DEFINIDA" else paste0("✅ ", strrep("*", nchar(api_key)-4), substr(api_key, nchar(api_key)-3, nchar(api_key)))))
cat(sprintf("   APP_URL        : %s\n",
    if (nchar(app_url) == 0) "❌ NO DEFINIDA" else paste0("✅ ", app_url)))
cat(sprintf("   EMAIL_FROM     : %s\n\n",
    if (nchar(from_addr) == 0) "⚠ No definida (se usará onboarding@resend.dev)" else paste0("✅ ", from_addr)))

if (nchar(api_key) == 0) {
  cat("❌ DETENIDO: RESEND_API_KEY no está definida.\n")
  cat("   Agrega en tu .Renviron:\n")
  cat("     RESEND_API_KEY=re_xxxxxxxxxxxxxxxxxxxx\n")
  cat("   Luego reinicia R y vuelve a correr este script.\n")
  stop("Falta RESEND_API_KEY", call. = FALSE)
}

if (nchar(from_addr) == 0) {
  from_addr <- "onboarding@resend.dev"
  cat("⚠  EMAIL_FROM no definido. Usando sandbox: onboarding@resend.dev\n")
  cat("   (Con este remitente solo puedes enviar al correo de tu cuenta Resend)\n\n")
}

# ── 2. Correo de prueba — CAMBIA ESTA LÍNEA por tu correo real ─────────────────
email_prueba <- "dherrerapino@gmail.com"   # <── CAMBIA ESTO

if (email_prueba == "TU_CORREO@gmail.com") {
  cat("⚠  Cambia 'email_prueba' por tu correo real antes de continuar.\n")
  stop("Edita la variable email_prueba", call. = FALSE)
}

# ── 3. Llamada directa a Resend ───────────────────────────────────────────────
cat(sprintf("2. Intentando enviar correo de prueba a: %s\n", email_prueba))
cat(sprintf("   Desde: %s\n\n", from_addr))

resp <- tryCatch(
  request("https://api.resend.com/emails") |>
    req_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type"  = "application/json"
    ) |>
    req_body_json(list(
      from    = from_addr,
      to      = list(email_prueba),
      subject = "HIDROCOMP-CL — Prueba de conexión",
      html    = "<p>Si recibes este correo, la configuración es correcta. ✅</p>"
    )) |>
    req_error(is_error = function(r) FALSE) |>
    req_timeout(15) |>
    req_perform(),
  error = function(e) {
    cat(sprintf("❌ Error de red: %s\n", e$message))
    cat("   Posibles causas: sin internet, firewall corporativo, proxy.\n")
    NULL
  }
)

if (is.null(resp)) stop("Sin respuesta", call. = FALSE)

status <- resp_status(resp)
body   <- tryCatch(resp_body_string(resp), error = function(e) "(no se pudo leer)")

cat(sprintf("3. Respuesta de Resend:\n"))
cat(sprintf("   HTTP Status : %d\n", status))
cat(sprintf("   Body        : %s\n\n", body))

if (status == 200) {
  cat("✅ ÉXITO — El correo fue enviado. Revisa tu bandeja (y spam).\n")
  cat("   Si usaste onboarding@resend.dev, solo llega al correo de tu cuenta Resend.\n")

} else if (status == 401) {
  cat("❌ ERROR 401 — API Key incorrecta o expirada.\n")
  cat("   Solución: ve a https://resend.com/api-keys y genera una nueva key.\n")
  cat("   Luego actualiza RESEND_API_KEY en tu .Renviron.\n")

} else if (status == 403) {
  cat("❌ ERROR 403 — Dominio del remitente no verificado.\n")
  cat("   El remitente usado fue: ", from_addr, "\n")
  if (grepl("resend.dev", from_addr)) {
    cat("   Aunque usas onboarding@resend.dev, este error indica que tu\n")
    cat("   cuenta Resend NO está verificada o estás en un plan sin acceso.\n")
    cat("   Solución: ve a https://resend.com y verifica tu cuenta.\n")
  } else {
    cat("   Solución A (rápida): define EMAIL_FROM=onboarding@resend.dev\n")
    cat("   Solución B (producción): verifica tu dominio en https://resend.com/domains\n")
  }

} else if (status == 422) {
  cat("❌ ERROR 422 — Datos inválidos.\n")
  cat("   Revisa que el email de destino tenga formato correcto.\n")
  cat("   Detalle: ", body, "\n")

} else {
  cat(sprintf("⚠  Respuesta inesperada (HTTP %d). Detalle: %s\n", status, body))
}

cat("\n=== FIN DEL DIAGNÓSTICO ===\n")
