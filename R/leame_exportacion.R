#' Texto del LEAME que acompaña toda exportación (fuente, definiciones, limitaciones)
#'
#' @param res Salida de `tablas_salidas()`.
#' @param ids Ids de las salidas exportadas.
#' @param punto Lista con `lon` y `lat` pedidos (o NULL).
#' @return Vector de líneas de texto.
leame_exportacion <- function(res, ids = NULL, punto = NULL) {
  m <- res$meta
  fmt <- function(x) paste(x, collapse = ", ")
  c("CALCULADORA CLIMÁTICA VALPARAÍSO + RM — LEAME",
    paste("Generado:", format(Sys.Date(), "%Y-%m-%d")),
    "",
    "PUNTO Y PERÍODO",
    if (!is.null(punto)) sprintf("Coordenada pedida (WGS84): lon %.5f, lat %.5f", punto$lon, punto$lat),
    if (!is.null(m$celda)) sprintf("Centro de la celda CR2MET usada: lon %.3f, lat %.3f", m$celda[["lon"]], m$celda[["lat"]]),
    paste("Años completos usados:", m$periodo_txt),
    if (length(m$anios_descartados))
      paste("Años parciales descartados (no cubren enero a diciembre):", fmt(m$anios_descartados)),
    "",
    "FUENTE",
    "CR2MET v2.5 — Boisier, J. P. (2023). CR2MET: A high-resolution precipitation and",
    "temperature dataset for the period 1960-2021 in continental Chile. Centro de Ciencia",
    "del Clima y la Resiliencia (CR2), Universidad de Chile. DOI 10.5281/zenodo.7529682. CC BY 4.0.",
    "Grilla de 0,05° (~5 km), datos diarios de precipitación, Tmin y Tmax, 1960-2021.",
    "",
    "DEFINICIONES",
    paste0("Día de helada: Tmin <= umbral (", fmt(m$umbrales_helada), " °C)."),
    paste0("Día cálido: Tmax > umbral (", fmt(m$umbrales_calor), " °C)."),
    paste0("Día con lluvia: precipitación >= ", m$dia_lluvia_mm, " mm."),
    "Tmed = (Tmin + Tmax) / 2 (aproximación; no es la media de 24 h).",
    "Climatologías y totales anuales: solo años completos.",
    "",
    "LIMITACIONES",
    "- Producto grillado construido con modelos estadísticos calibrados con estaciones;",
    "  no es medición directa.",
    "- A ~5 km cada celda promedia el relieve: un valor de celda no equivale a un predio.",
    "  En fondos de valle o sitios con inversión térmica tiende a subestimar heladas y a",
    "  moderar extremos.",
    "- El período termina el 2021-12-31: no incluye años recientes.",
    "- Con pocos años, medias y tendencias son descriptivas, no concluyentes.",
    "",
    if (length(ids)) c("SALIDAS INCLUIDAS", paste("-", ids)))
}
