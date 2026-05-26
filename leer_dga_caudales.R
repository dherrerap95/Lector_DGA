# =============================================================================
# leer_dga_caudales.R
# Lectura de archivos XLS de Caudales Medios Diarios - DGA Chile
# Extrae metadatos completos: estación, BNA, cuenca, subcuenca,
# altitud, latitud, longitud, UTM Norte, UTM Este, área de drenaje.
# =============================================================================

leer_dga_caudales <- function(file, log_fn = message) {
  
  meses <- c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN",
             "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")
  
  # ---------------------------------------------------------------------------
  # Helpers de extracción de metadatos
  # Patrón: la celda con la etiqueta y la siguiente celda no-vacía = valor
  # ---------------------------------------------------------------------------
  extraer_meta <- function(raw, patron) {
    nc  <- ncol(raw)
    for (r in seq_len(nrow(raw))) {
      fila_chr <- replace_na(as.character(unlist(raw[r, ])), "")
      idx_lab  <- which(str_detect(fila_chr, regex(patron, ignore_case = TRUE)))
      if (length(idx_lab) == 0) next
      # Buscar primer valor no-vacío a la derecha de la etiqueta
      for (pos in idx_lab) {
        candidatos <- fila_chr[(pos + 1):min(pos + 6, nc)]
        val <- candidatos[candidatos != "" & !str_detect(candidatos, regex(patron, ignore_case = TRUE))][1]
        if (!is.na(val) && val != "") return(str_trim(val))
      }
    }
    return(NA_character_)
  }
  
  # Convierte coordenada DMS "32° 46' 41''" → decimal negativo (hemisferio S/W)
  dms_a_decimal <- function(dms_str, negativo = TRUE) {
    if (is.na(dms_str) || dms_str == "") return(NA_real_)
    nums <- suppressWarnings(as.numeric(str_extract_all(dms_str, "\\d+\\.?\\d*")[[1]]))
    if (length(nums) < 1) return(NA_real_)
    grados  <- nums[1]
    minutos <- ifelse(length(nums) >= 2, nums[2], 0)
    segundos <- ifelse(length(nums) >= 3, nums[3], 0)
    dec <- grados + minutos / 60 + segundos / 3600
    if (negativo) dec <- -dec
    return(dec)
  }
  
  hojas      <- excel_sheets(file)
  resultados <- list()
  
  for (hoja in hojas) {
    
    # ── 1. Leer hoja completa como texto ──────────────────────────────────────
    raw <- tryCatch(
      read_excel(file,
                 sheet        = hoja,
                 col_names    = FALSE,
                 col_types    = "text",
                 .name_repair = "unique"),
      error = function(e) {
        log_fn(sprintf("  [AVISO] No se pudo leer la hoja '%s': %s", hoja, e$message))
        return(NULL)
      }
    )
    if (is.null(raw)) next
    nc <- ncol(raw)
    
    # Vector de texto combinado (primeras 5 cols) para búsquedas de filas
    texto_filas <- apply(
      raw[, 1:min(5, nc), drop = FALSE], 1,
      function(x) paste(replace_na(as.character(x), ""), collapse = " ")
    )
    
    # ── 2. Extraer metadatos ──────────────────────────────────────────────────
    estacion    <- extraer_meta(raw, "Estaci[oó]n")
    if (is.na(estacion)) estacion <- str_trim(hoja)
    
    codigo_bna  <- extraer_meta(raw, "C[oó]digo\\s*BNA")
    cuenca      <- extraer_meta(raw, "^Cuenca")
    subcuenca   <- extraer_meta(raw, "SubCuenca")
    altitud_str <- extraer_meta(raw, "Altitud")
    lat_str     <- extraer_meta(raw, "Latitud")
    lon_str     <- extraer_meta(raw, "Longitud")
    utm_norte   <- extraer_meta(raw, "UTM\\s*Norte")
    utm_este    <- extraer_meta(raw, "UTM\\s*Este")
    area_str    <- extraer_meta(raw, "[Áa]rea\\s*de\\s*Drenaje")
    
    altitud     <- suppressWarnings(as.numeric(altitud_str))
    latitud     <- dms_a_decimal(lat_str, negativo = TRUE)
    longitud    <- dms_a_decimal(lon_str, negativo = TRUE)
    utm_norte_n <- suppressWarnings(as.numeric(utm_norte))
    utm_este_n  <- suppressWarnings(as.numeric(utm_este))
    area_km2    <- suppressWarnings(as.numeric(area_str))
    
    # ── 3. Detectar bloques anuales por "AÑO" ────────────────────────────────
    idx_anios <- which(str_detect(texto_filas, regex("A[ÑN]O", ignore_case = TRUE)))
    
    if (length(idx_anios) == 0) {
      log_fn(sprintf("  [AVISO] Hoja '%s': no se encontraron bloques anuales.", hoja))
      next
    }
    
    for (j in seq_along(idx_anios)) {
      
      fila_anio <- idx_anios[j]
      fila_fin  <- if (j < length(idx_anios)) idx_anios[j + 1] - 1L else nrow(raw)
      
      anio <- str_extract(texto_filas[fila_anio], "\\d{4}")
      if (is.na(anio)) next
      anio <- as.integer(anio)
      
      sub <- raw[(fila_anio + 1L):fila_fin, , drop = FALSE]
      
      # ── 4. Localizar cabecera "DIA" ─────────────────────────────────────────
      col_dia    <- NA_integer_
      header_idx <- NA_integer_
      
      for (c_idx in 1:min(5, ncol(sub))) {
        h_idx <- which(str_trim(str_to_upper(replace_na(sub[[c_idx]], ""))) == "DIA")[1]
        if (!is.na(h_idx)) {
          header_idx <- h_idx
          col_dia    <- c_idx
          break
        }
      }
      
      if (is.na(header_idx)) {
        log_fn(sprintf("  [AVISO] Hoja '%s', año %d: no se encontró cabecera 'DIA'.", hoja, anio))
        next
      }
      
      header    <- sub[header_idx, ]
      datos_raw <- sub[(header_idx + 1L):nrow(sub), , drop = FALSE]
      
      # ── 5. Eliminar fila INDICADORES y filtrar días válidos ─────────────────
      datos_raw <- datos_raw %>%
        filter(
          !str_detect(coalesce(.[[col_dia]], ""), regex("^INDICADORES", ignore_case = TRUE)),
          !is.na(.[[col_dia]]),
          str_detect(str_trim(.[[col_dia]]), "^\\d+$")
        )
      
      if (nrow(datos_raw) == 0) next
      
      # ── 6. Identificar columnas de meses ────────────────────────────────────
      header_vals <- str_trim(str_to_upper(replace_na(unlist(header), "")))
      cols_meses  <- which(header_vals %in% meses)
      
      if (length(cols_meses) == 0) next
      nombres_meses <- header_vals[cols_meses]
      
      # ── 7. Extraer DIA + valores ─────────────────────────────────────────────
      cols_sel    <- c(col_dia, cols_meses)
      nombres_sel <- c("dia", nombres_meses)
      
      datos_sel <- datos_raw[, cols_sel, drop = FALSE]
      names(datos_sel) <- nombres_sel
      
      # ── 8. Extraer indicadores (columna siguiente a cada mes) ────────────────
      cols_ind     <- cols_meses + 1L
      mask_ind     <- cols_ind <= nc
      cols_ind_ok  <- cols_ind[mask_ind]
      meses_ind_ok <- nombres_meses[mask_ind]
      
      datos_ind <- datos_raw[, c(col_dia, cols_ind_ok), drop = FALSE]
      names(datos_ind) <- c("dia", paste0(meses_ind_ok, "_ind"))
      
      datos_sel <- datos_sel %>%
        mutate(dia = suppressWarnings(as.integer(dia))) %>%
        filter(!is.na(dia), dia >= 1L, dia <= 31L)
      
      datos_ind <- datos_ind %>%
        mutate(dia = suppressWarnings(as.integer(dia))) %>%
        filter(!is.na(dia), dia >= 1L, dia <= 31L)
      
      if (nrow(datos_sel) == 0) next
      
      # ── 9. Pivotear e integrar ───────────────────────────────────────────────
      long_vals <- datos_sel %>%
        pivot_longer(-dia, names_to = "mes_str", values_to = "valor_raw") %>%
        mutate(Valor = suppressWarnings(as.numeric(valor_raw)))
      
      if (ncol(datos_ind) > 1) {
        long_ind <- datos_ind %>%
          pivot_longer(-dia, names_to = "mes_ind", values_to = "Indicador") %>%
          mutate(
            mes_str   = str_remove(mes_ind, "_ind$"),
            Indicador = str_trim(coalesce(Indicador, " "))
          ) %>%
          select(dia, mes_str, Indicador)
      } else {
        long_ind <- long_vals %>% select(dia, mes_str) %>% mutate(Indicador = NA_character_)
      }
      
      long_final <- long_vals %>%
        left_join(long_ind, by = c("dia", "mes_str")) %>%
        filter(!is.na(Valor)) %>%
        mutate(
          mes_num = match(mes_str, meses),
          Fecha   = tryCatch(
            make_date(anio, mes_num, dia),
            warning = function(w) as.Date(NA),
            error   = function(e) as.Date(NA)
          )
        ) %>%
        filter(!is.na(Fecha)) %>%
        mutate(
          Estacion     = estacion,
          CodigoBNA    = codigo_bna,
          Cuenca       = cuenca,
          SubCuenca    = subcuenca,
          Altitud_msnm = altitud,
          Latitud_S    = latitud,
          Longitud_W   = longitud,
          UTM_Norte    = utm_norte_n,
          UTM_Este     = utm_este_n,
          AreaDrenaje_km2 = area_km2,
          Anio         = anio,
          Indicador    = if_else(Indicador %in% c("*", "<", ">"), Indicador, NA_character_)
        ) %>%
        select(
          Estacion, CodigoBNA, Cuenca, SubCuenca,
          Altitud_msnm, Latitud_S, Longitud_W, UTM_Norte, UTM_Este, AreaDrenaje_km2,
          Fecha, Valor, Indicador
        )
      
      resultados[[length(resultados) + 1L]] <- long_final
    }
  }
  
  if (length(resultados) == 0) {
    warning(sprintf("No se extrajo ningún dato del archivo: %s", basename(file)))
    return(tibble(
      Estacion = character(), CodigoBNA = character(),
      Cuenca = character(), SubCuenca = character(),
      Altitud_msnm = numeric(), Latitud_S = numeric(), Longitud_W = numeric(),
      UTM_Norte = numeric(), UTM_Este = numeric(), AreaDrenaje_km2 = numeric(),
      Fecha = as.Date(character()), Valor = numeric(), Indicador = character()
    ))
  }
  
  bind_rows(resultados) %>% arrange(Estacion, Fecha)
}