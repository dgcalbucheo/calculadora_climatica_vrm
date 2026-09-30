#' Resumen (media, mínimo, máximo por año) de una tabla anual por umbral
#'
#' @param x Tabla anual con columnas `umbral`, `anio_completo` y la columna `col`
#'   (salida de `metricas_punto()`).
#' @param col Columna a resumir ("helada" o "supera").
#' @param solo_completos Si TRUE, usa solo años con todos sus días.
#' @return data.frame: umbral, media, min, max, n_anios.
resumen_por_umbral <- function(x, col, solo_completos = TRUE) {
  if (solo_completos) x <- x[x$anio_completo, ]
  if (nrow(x) == 0) {
    return(data.frame(umbral = numeric(), media = numeric(), min = numeric(),
                      max = numeric(), n_anios = integer()))
  }
  a <- stats::aggregate(x[[col]], list(umbral = x$umbral), function(v)
    c(media = mean(v), min = min(v), max = max(v), n = length(v)))
  data.frame(umbral = a$umbral, media = a$x[, "media"], min = a$x[, "min"],
             max = a$x[, "max"], n_anios = as.integer(a$x[, "n"]), row.names = NULL)
}
