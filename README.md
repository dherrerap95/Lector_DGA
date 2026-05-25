# 📊 Compilador de Caudales Medios Diarios — DGA Chile

Aplicación **Shiny** para procesar y compilar masivamente archivos XLS de **Caudales Medios Diarios** descargados desde la plataforma de la Dirección General de Aguas (DGA) de Chile.

---

## ✨ Funcionalidades

| Función | Descripción |
|---|---|
| **Lectura masiva** | Procesa todos los XLS de una carpeta en un solo clic |
| **Metadatos completos** | Extrae estación, BNA, cuenca, subcuenca, altitud, latitud, longitud, UTM Norte/Este, área de drenaje |
| **Indicadores DGA** | Preserva indicadores de calidad (`*`, `<`, `>`) |
| **Exportación** | Guarda CSV en carpeta seleccionada + descarga directa desde el navegador |
| **Previsualización** | Tabla interactiva con filtros, paginación y barras de valor |
| **Log de proceso** | Consola en tiempo real con conteo de registros por archivo |

---

## 📁 Estructura del proyecto

```
dga-caudales-compiler/
├── app.R                    # Aplicación Shiny principal
├── R/
│   └── leer_dga_caudales.R  # Función de lectura y parseo de XLS DGA
├── README.md
└── .gitignore
```

---

## 🚀 Instalación y ejecución

### Prerrequisitos

R ≥ 4.2 con los siguientes paquetes:

```r
install.packages(c(
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
))
```

### Ejecutar localmente

```r
shiny::runApp("dga-caudales-compiler")
```

O desde RStudio: abrir `app.R` y presionar **Run App**.

### Despliegue en shinyapps.io

```r
library(rsconnect)
rsconnect::deployApp("dga-caudales-compiler")
```

---

## 📋 Formato del CSV de salida

| Columna | Tipo | Descripción |
|---|---|---|
| `Estacion` | chr | Nombre de la estación fluviométrica |
| `CodigoBNA` | chr | Código BNA (Banco Nacional de Aguas) |
| `Cuenca` | chr | Cuenca hidrográfica principal |
| `SubCuenca` | chr | Subcuenca hidrográfica |
| `Altitud_msnm` | num | Altitud de la estación (msnm) |
| `Latitud_S` | num | Latitud decimal sur (negativa) |
| `Longitud_W` | num | Longitud decimal oeste (negativa) |
| `UTM_Norte` | num | Coordenada UTM Norte (m) |
| `UTM_Este` | num | Coordenada UTM Este (m) |
| `AreaDrenaje_km2` | num | Área de drenaje (km²) |
| `Fecha` | date | Fecha del registro (YYYY-MM-DD) |
| `Valor` | num | Caudal medio diario (m³/s) |
| `Indicador` | chr | Indicador de calidad: `*` estimado, `<` menor, `>` mayor |
| `ArchivoFuente` | chr | Nombre del archivo XLS de origen |

---

## 🗺️ Coordenadas

- **Latitud / Longitud**: convertidas automáticamente de DMS (`°` `'` `''`) a grados decimales (negativo para hemisferio S/W).
- **UTM**: Sistema de referencia PSAD56 o WGS84 según publicación DGA (verificar datum con cada descarga).

---

## 🐛 Reporte de bugs y mejoras

Abrir un *Issue* en GitHub con:
1. Mensaje de error del log
2. Fragmento de la fila del XLS problemática (sin datos sensibles)
3. Versión de R y SO

---

## 📄 Licencia

MIT — uso libre para proyectos de recursos hídricos, académicos e institucionales.

---

*Desarrollado para análisis hidrológico de cuencas en Chile.*
