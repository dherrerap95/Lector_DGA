# 📊 Compilador de Caudales Medios Diarios — DGA Chile

Aplicación **Shiny** para procesar y compilar masivamente archivos XLS de **Caudales Medios Diarios** descargados desde la plataforma de la Dirección General de Aguas (DGA) de Chile.

---

## ✨ Funcionalidades

| Función | Descripción |
|---|---|
| **Lectura masiva** | Procesa todos los XLS/XLSX de una carpeta en un solo clic |
| **Metadatos completos** | Extrae estación, BNA, cuenca, subcuenca, altitud, latitud, longitud, UTM Norte/Este, área de drenaje |
| **Indicadores DGA** | Preserva indicadores de calidad (`*`, `<`, `>`) — activable/desactivable |
| **Exportación múltiple** | Descarga directa en CSV: caudal diario, medio mensual y medio anual |
| **Gráfico interactivo** | Visualización Plotly con selección de hasta 5 estaciones, agregación temporal (diaria/mensual/anual) y escala logarítmica |
| **Tabla de resumen** | Resumen por estación: período, número de registros, Q medio/máx/mín |
| **Datos completos** | Tabla interactiva con filtros por columna, paginación y barras de valor |
| **Log de proceso** | Consola en tiempo real con conteo de registros y errores por archivo |

---

## 📁 Estructura del proyecto

```
dga-caudales-compiler/
├── app.R                    # Aplicación Shiny principal
├── leer_dga_caudales.R      # Función de lectura y parseo de XLS DGA
├── install_packages.R       # Script de instalación de dependencias
├── logo.png                 # Logo de la aplicación (esquina superior derecha)
└── README.md
```

---

## 🚀 Instalación y ejecución

### Prerrequisitos

R ≥ 4.2 con los siguientes paquetes:

```r
source("install_packages.R")
```

O manualmente:

```r
install.packages(c(
  "shiny", "shinyFiles", "shinyjs",
  "readxl", "dplyr", "tidyr", "stringr", "lubridate",
  "DT", "shinycssloaders", "bslib", "htmltools",
  "ggplot2", "plotly", "scales"
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

> ⚠️ Asegúrate de incluir `logo.png` y `leer_dga_caudales.R` en el directorio de despliegue.

---

## 📋 Formato del CSV de salida

### Caudal diario

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

### Caudal medio mensual (agregado)

Incluye las mismas columnas de metadatos más: `Anio`, `Mes`, `Q_medio_mensual`, `N_dias`.

### Caudal medio anual (agregado)

Incluye las mismas columnas de metadatos más: `Anio`, `Q_medio_anual`, `Q_max_anual`, `Q_min_anual`, `N_dias`.

---

## 🗺️ Coordenadas

- **Latitud / Longitud**: convertidas automáticamente de DMS (`°` `'` `''`) a grados decimales (negativo para hemisferio S/W).
- **UTM**: Sistema de referencia PSAD56 o WGS84 según publicación DGA (verificar datum con cada descarga).

---

## 🖥️ Interfaz

- **Panel izquierdo**: selección de carpeta de entrada, opciones de filtrado y botones de descarga.
- **Stat boxes**: resumen rápido post-procesamiento (archivos procesados, registros totales, estaciones, rango temporal).
- **Tab Resumen**: tabla por estación con período, recuento y estadísticas básicas de caudal.
- **Tab Gráfico**: selección múltiple de estaciones (máx. 5), agregación diaria/mensual/anual, escala logarítmica opcional.
- **Tab Datos completos**: tabla completa con filtros, paginación y botones de exportación CSV/Excel.

---

## 🐛 Reporte de bugs y mejoras

Abrir un *Issue* en GitHub con:
1. Mensaje de error del log
2. Fragmento de la fila del XLS problemática (sin datos sensibles)
3. Versión de R y sistema operativo

---

## 📄 Licencia

**GNU General Public License v3.0** (GPL-3.0)

Este programa es software libre: puedes redistribuirlo y/o modificarlo bajo los términos de la Licencia Pública General GNU publicada por la Free Software Foundation, ya sea la versión 3 de la Licencia o (a tu elección) cualquier versión posterior.

Este programa se distribuye con la esperanza de que sea útil, pero **SIN NINGUNA GARANTÍA**; incluso sin la garantía implícita de **COMERCIALIZACIÓN** o **APTITUD PARA UN PROPÓSITO PARTICULAR**. Consulta la Licencia Pública General GNU para más detalles.

Deberías haber recibido una copia de la GNU GPL junto con este programa. Si no, visita <https://www.gnu.org/licenses/>.

---

*Desarrollado para análisis hidrológico de cuencas en Chile.*  
*Por Diego Herrera Pino.*
