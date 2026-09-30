#' Primera y última helada de cada año
#'
#' Para cada año y umbral: fecha de la primera y última helada, y número de
#' días de helada. En Chile central la temporada de heladas cae dentro del
#' año calendario (≈ abril–noviembre), por lo que se usa el año calendario.
#'
#' @param heladas Salida de dias_helada().
#' @param grupos Columnas de agrupación adicionales (p. ej. "fuente").
#' @return data.frame: grupos, umbral, anio, primera, ultima, dia_juliano de
#'   cada una, n_dias. Años sin heladas quedan con NA.
fechas_helada_anual <- function(heladas, grupos = NULL) {
  heladas$anio <- as.integer(format(heladas$fecha, "%Y"))
  claves <- c(grupos, "umbral", "anio")
  partes <- split(heladas, heladas[, claves], drop = TRUE)
  out <- do.call(rbind, lapply(partes, function(d) {
    f <- d$fecha[d$helada]
    data.frame(d[1, claves, drop = FALSE],
               primera = if (length(f)) min(f) else as.Date(NA),
               ultima  = if (length(f)) max(f) else as.Date(NA),
               n_dias  = length(f))
  }))
  out$doy_primera <- as.integer(format(out$primera, "%j"))
  out$doy_ultima  <- as.integer(format(out$ultima, "%j"))
  rownames(out) <- NULL
  out[do.call(order, out[, claves, drop = FALSE]), ]
}
