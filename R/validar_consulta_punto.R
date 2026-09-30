#' Validar coordenada y fechas de una consulta puntual
#'
#' Chequeos comunes a `serie_clima_punto()` y `serie_clima_celda()`. Los errores
#' son mensajes en español sin traza (`call. = FALSE`), pensados para mostrarse
#' tal cual en la interfaz.
#'
#' @param lon,lat Coordenadas WGS84 (un solo punto).
#' @param fecha_ini,fecha_fin Fechas ("YYYY-MM-DD" o Date), ambas inclusive.
#' @param periodo_disponible Vector de dos fechas (inicio, fin) que cubre la base
#'   (NULL = no validar contra el período disponible).
#' @return Lista con `ini` y `fin` (Date).
validar_consulta_punto <- function(lon, lat, fecha_ini, fecha_fin,
                                   periodo_disponible = NULL) {
  if (!is.numeric(lon) || !is.numeric(lat) || length(lon) != 1 || length(lat) != 1 ||
      anyNA(c(lon, lat)) || !all(is.finite(c(lon, lat)))) {
    stop("La coordenada debe ser un lon/lat numérico", call. = FALSE)
  }
  ini <- tryCatch(as.Date(fecha_ini), error = function(e) NA)
  fin <- tryCatch(as.Date(fecha_fin), error = function(e) NA)
  if (length(ini) != 1 || length(fin) != 1 || anyNA(c(ini, fin))) {
    stop("Fechas no válidas (formato AAAA-MM-DD)", call. = FALSE)
  }
  if (ini > fin) stop("La fecha de inicio es posterior a la de término", call. = FALSE)
  if (!is.null(periodo_disponible)) {
    lim <- as.Date(periodo_disponible)
    if (ini < lim[1] || fin > lim[2]) {
      stop(sprintf("El período pedido (%s a %s) sale del rango disponible (%s a %s)",
                   ini, fin, lim[1], lim[2]), call. = FALSE)
    }
  }
  list(ini = ini, fin = fin)
}
