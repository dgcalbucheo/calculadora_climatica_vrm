# calculadora_climatica_vrm

> **Versión 1 · prototipo de aprendizaje y portafolio.** No apto para decisiones
> agronómicas sin contrastar con datos locales. Limitaciones principales:
>
> - Cada resultado es el de una **celda de ~26 km²** (~5,6 × 4,7 km), no el de un predio.
>   No resuelve el **drenaje de aire frío** (fondo de valle vs. ladera), que es el proceso
>   que más pesa en las heladas.
> - **CR2MET es un producto modelado y calibrado con estaciones**: compararlo con esas
>   mismas estaciones no es una validación independiente.
> - Los datos **terminan el 2021-12-31**.

Prototipo de **calculadora de riesgo climático por punto** (heladas, temperaturas y
precipitación) acotada a la **Región de Valparaíso y la Región Metropolitana**.
Reutiliza el motor de análisis de `paltos_quilpue`, un estudio de factibilidad agronómica
previo (repositorio privado), del que se extraen y generalizan funciones.

App de prueba en línea: <https://dgcalbucheo.shinyapps.io/calculadora-clima-vrm/>
(ver «Ejecutar online»).

## Estado

| Paso del pipeline | Estado |
|---|---|
| 1. Alcance geográfico fijo (Valparaíso + RM) | Hecho |
| 2. Estrategia de datos: descargar una vez y recortar | Hecho (recorte en `data/raw/cr2met`); se valida en `analisis/00_validacion_recorte.qmd` |
| 3. Modularizar funciones de `paltos_quilpue` (parámetros: coordenada, fechas) | Hecho (motor en `R/`, catálogo de salidas y exportación) |
| 4. Prototipo Shiny mínimo | Hecho (`app.R`, ejecución local) |
| 5. Validar con un punto conocido (Colliguay) | Hecho: `tests/control_colliguay.R` y `analisis/01_motor_por_celda.qmd` |
| 6. Salida descargable | Hecho: zip con PNG (300 dpi) + Excel + LEAME desde la app, e informe HTML por punto con Quarto paramétrico (`analisis/02_informe_punto.qmd`, se ejecuta desde el computador) |
| 7. Despliegue de prueba | Hecho (shinyapps.io, plan gratuito; ver «Ejecutar online») |

## Estructura

```
app.R              app Shiny (solo orquesta; la lógica está en R/)
parametros.yml     parámetros compartidos (punto de control, períodos, umbrales, rutas)
R/                 funciones reutilizables (una por archivo; el archivo se llama como la función)
analisis/          análisis narrado en Quarto, numerado (00_validacion_recorte.qmd,
                   01_motor_por_celda.qmd, 02_informe_punto.qmd: informe paramétrico por punto)
tests/             control_colliguay.R: control de regresión (se corre tras cambiar el motor)
LICENSE            licencia MIT (código)
data/raw/          datos originales (NO versionado): recorte de CR2MET
data/processed/    capas y datos derivados (celdas/: NO versionado; cobertura_vrm.gpkg: sí, ~5 MB)
output/            tablas y figuras finales
```

## Datos

**CR2MET v2.5** — Boisier, J.P. (2023). *CR2MET: A high-resolution precipitation and
temperature dataset for the period 1960-2021 in continental Chile.* Centro de Ciencia
del Clima y la Resiliencia (CR2), Universidad de Chile. DOI
[10.5281/zenodo.7529682](https://doi.org/10.5281/zenodo.7529682) · CC BY 4.0.

- Precipitación diaria, Tmin y Tmax diarias, 1960-01-01 a 2021-12-31, grilla de 0,05° (~5 km).
- Descarga: <https://www.cr2.cl/datos-productos-grillados/> (requiere formulario) o
  [Zenodo](https://zenodo.org/records/7529681): `CR2MET_pr_v2.5.zip` y `CR2MET_txn_v2.5.zip`.
- **Recorte usado:** regiones de Valparaíso (5) y Metropolitana (13), parte continental
  (sin Isla de Pascua ni Juan Fernández), con margen de 0,1° y ajuste a la grilla original:
  lon −71,95 a −69,65, lat −34,40 a −31,90 (46 × 50 celdas). Se conserva un netCDF por
  mes, con el mismo nombre y subcarpeta que el original (`CR2MET_pr_v2.5/pr/`,
  `CR2MET_txn_v2.5/txn/`); se omite la máscara `cl_mask`. El original nacional (~32 GB)
  no se conserva.
- Límites de la fuente: producto modelado y calibrado con estaciones (no medición
  directa); a ~5 km no resuelve microtopografía; termina en 2021-12-31.

**Límites comunales** — la máscara de cobertura (`data/processed/cobertura_vrm.gpkg`, 88
comunas de las regiones 5 y 13) se construyó a partir de los mapas vectoriales de la
**Biblioteca del Congreso Nacional de Chile**, de uso libre citando la fuente. Es un material
referencial, sin precisión geodésica: aquí solo decide qué puntos acepta la calculadora.

**Regenerar `data/raw`** (el raw no se versiona): descargar los dos .zip de CR2MET,
descomprimirlos en una carpeta y ejecutar:

```r
library(terra)
invisible(lapply(list.files("R", "\\.R$", full.names = TRUE), source))
par <- yaml::read_yaml("parametros.yml", fileEncoding = "UTF-8")
comunas_shp <- "<ruta al shapefile de comunas de Chile>"   # no se incluye en el repo

e <- extent_regiones_chile(comunas_shp,
                           cod_regiones = par$recorte$cod_regiones,
                           margen = par$recorte$margen_grados)
recortar_cr2met_mensual(src = "<carpeta con los netCDF originales>",
                        dst = file.path(par$rutas$raw, "cr2met"),
                        ext_wgs84 = e,
                        n_esperado = 2 * par$recorte$n_archivos_esperado)
```

## Cómo se reconstruye todo desde cero

De `data/` solo se versiona la máscara `cobertura_vrm.gpkg` (el paso 2 es opcional si la
tienes). Para reproducir la app en otro equipo, en este orden:

```r
renv::restore()

# 1) data/raw/cr2met: descargar CR2MET y recortar (bloque de arriba)

# 2) Máscara de cobertura (comunas de las regiones 5 y 13, continente)
invisible(lapply(list.files("R", "\\.R$", full.names = TRUE), source))
par <- yaml::read_yaml("parametros.yml", fileEncoding = "UTF-8")
comunas_shp <- "<ruta al shapefile de comunas de Chile>"   # el mismo de arriba
crear_mascara_cobertura(comunas_shp, par$rutas$mascara)

# 3) Un archivo por celda (~1.300 celdas, ~120 MB, ~13 min)
consolidar_cr2met_por_celda("data/raw/cr2met", "data/processed/celdas", par$rutas$mascara)

# 4) Control: debe terminar en «CONTROL COLLIGUAY: OK»
source("tests/control_colliguay.R")
```

`consolidar_cr2met_por_celda()` guarda Tmin, Tmax y pr como enteros ×1000 (diferencia
máxima con la lectura directa: 0,0005). La app solo lee esos archivos, sin `terra`.
Detalle y limitaciones: `analisis/01_motor_por_celda.qmd`.

## Ejecutar la app (local)

Abrir `calculadora_climatica_vrm.Rproj` (para que renv y las rutas relativas queden activos) y:

```r
shiny::runApp()      # o abrir app.R y pulsar «Run App»
```

Se abre en el navegador (`127.0.0.1`), solo visible desde este equipo. Se elige un punto
(clic en el mapa o coordenadas WGS84), un período (1960-2021), umbrales y salidas del
catálogo, y «Generar» entrega una vista previa y un zip con PNG, `tablas.xlsx` y `LEAME.txt`.
Requiere `data/processed/celdas/` (ver arriba). Para el mapa: paquete `leaflet`.

**Alcance:** el valor es el de una celda de ~26 km², no el de un predio (no resuelve el
drenaje de aire frío). Climatologías y totales usan solo años completos.

## Ejecutar online (shinyapps.io)

App de prueba y aprendizaje, publicada en el plan gratuito de shinyapps.io:
<https://dgcalbucheo.shinyapps.io/calculadora-clima-vrm/> (se duerme tras 15 min sin uso;
la primera carga tarda unos segundos). Plan gratuito: 5 apps, 25 h activas/mes, bundle ≤ 1 GB
(el nuestro pesa ~120 MB).

Para publicar o actualizar (una vez registrada la cuenta con `rsconnect::setAccountInfo()`;
el token es una contraseña: no va en el repo ni en el chat):

```r
source("R/preparar_deploy.R")
preparar_deploy()      # crea deploy_app/ sin las funciones que usan terra/sf
shiny::runApp("deploy_app")   # probar antes de subir
rsconnect::deployApp("deploy_app", appName = "calculadora-clima-vrm")
```

Notas aprendidas:

- `preparar_deploy()` excluye las funciones que usan `terra`/`sf`, pero el servidor igual
  compila `terra` y `sf` en el primer despliegue, porque `leaflet` los arrastra como
  dependencias indirectas (`raster`). Es lento la primera vez (decenas de minutos); los
  despliegues siguientes reutilizan lo instalado.
- Interrumpir `deployApp()` en R no detiene el trabajo del servidor: mientras siga en curso,
  otro despliegue de la misma app responde con error 409. Solución usada: publicar con otro
  `appName` y archivar la app vieja después.
- La app lleva un pie con el aviso de versión 1 y la cita de CR2MET (CC BY 4.0); la fuente
  también va en el `LEAME.txt` del zip.
- Alternativa sin servidor (shinylive/WebAssembly): sin probar.

## Puesta en marcha

Abrir `calculadora_climatica_vrm.Rproj` en RStudio y:

```r
renv::restore()    # si ya existe renv.lock
# primera vez: renv::init(); renv::snapshot()
```

Paquetes usados: `terra`, `sf` (solo construcción de datos), `ggplot2`, `yaml`, `here`,
`knitr`, `quarto`, y para la app `shiny`, `leaflet`, `openxlsx`, `zip`.
Después, renderizar `analisis/00_validacion_recorte.qmd` y `analisis/01_motor_por_celda.qmd`.

## Informe por punto (Quarto paramétrico)

`analisis/02_informe_punto.qmd` genera un HTML estático de un punto (mismo motor que la app,
con mapa de ubicación, tablas y gráficos, fuente y limitaciones). Requiere Quarto y los datos
de `data/processed/celdas/`. Parámetros: `nombre`, `lon`, `lat` (WGS84), `fecha_ini`,
`fecha_fin`, `umbrales_helada`, `umbrales_calor` y `notas` (texto interpretativo opcional).
Sin parámetros usa el punto de control y el período de validación de `parametros.yml`.

```r
quarto::quarto_render(
  "analisis/02_informe_punto.qmd",
  output_file = "informe_mi_predio.html",
  execute_params = list(nombre = "Mi predio", lon = -71.0, lat = -33.0,
                        fecha_ini = "2002-01-01", fecha_fin = "2021-12-31",
                        umbrales_helada = c(0, -2, -4), umbrales_calor = c(30, 35),
                        notas = "Texto interpretativo para el cliente (opcional).")
)
```

El HTML queda en `analisis/` (ignorado por git); moverlo a `output/informes/` (también
ignorado) para entregarlo. Los informes de clientes no se versionan.

## Licencia y atribución

- **Código:** licencia MIT (ver [`LICENSE`](LICENSE)).
- **Datos:** CR2MET v2.5 se distribuye con licencia CC BY 4.0; cualquier uso o publicación
  de resultados derivados debe citar la fuente (ver «Datos»). Los datos de CR2MET no se
  incluyen en este repositorio.
- **Límites comunales:** Biblioteca del Congreso Nacional de Chile (mapas vectoriales, uso
  libre señalando la fuente; material referencial).

## Funciones (R/)

| Función | Qué hace |
|---|---|
| `extent_regiones_chile()` | Extent WGS84 de regiones (con margen) a partir de un shapefile comunal |
| `recortar_cr2met_mensual()` | Recorta los netCDF mensuales de CR2MET a un extent, conservando nombres |
| `leer_cr2met()` | Lee el recorte de una variable (pr, tmin, tmax) y período como un SpatRaster diario |
| `serie_punto()` | Serie temporal de un raster en un punto (de `paltos_quilpue`) |
| `dias_helada()` | Días con Tmin ≤ umbral, formato largo (de `paltos_quilpue`) |
| `dias_sobre_umbral()` | Días con valor > umbral (o ≥) (de `paltos_quilpue`) |
| `totales_periodo()` | Totales por año o año-mes (de `paltos_quilpue`) |
| `tema_informe()` | Tema ggplot común (de `paltos_quilpue`) |
| `serie_clima_celda()` | Serie diaria (tmin, tmax, pr) de un punto desde los archivos por celda (R base) |
| `serie_clima_punto()` | Lo mismo leyendo los netCDF con `terra` (ruta de laboratorio) |
| `validar_consulta_punto()` | Valida coordenada y fechas de una consulta |
| `consolidar_cr2met_por_celda()` | Genera un `.rds` por celda, `indice.csv` y `grilla.rds` |
| `crear_mascara_cobertura()` / `punto_en_cobertura()` | Máscara de cobertura (regiones 5 y 13) y prueba de pertenencia |
| `metricas_punto()`, `resumen_por_umbral()`, `tablas_salidas()` | Métricas y tablas de salida (años completos) |
| `catalogo_salidas()`, `exportar_salidas()`, `leame_exportacion()` | Catálogo de salidas y exportación (PNG, Excel, LEAME) |
| `grafico_*()` | Un gráfico por salida del catálogo (heladas, temporada, temperatura, calor, precipitación) |
