#' Clasificar días de helada por umbral
#'
#' Un día es de helada para un umbral si Tmin ≤ umbral. Devuelve formato
#' largo (un registro por día y umbral), listo para resumir por mes o año.
#'
#' @param diario data.frame con columna `fecha` y la columna de Tmin.
#' @param umbrales Umbrales en °C.
#' @param col Nombre de la columna de Tmin.
#' @param grupos Columnas adicionales a conservar (p. ej. "fuente").
#' @return data.frame: grupos, fecha, umbral, helada (lógico), tmin.
dias_helada <- function(diario, umbrales = c(0, -1, -2, -3, -4),
                        col = "tmin", grupos = NULL) {
  base <- diario[!is.na(diario[[col]]), c(grupos, "fecha", col), drop = FALSE]
  do.call(rbind, lapply(umbrales, function(u) {
    data.frame(base[, c(grupos, "fecha"), drop = FALSE],
               umbral = u,
               helada = base[[col]] <= u,
               tmin = base[[col]])
  }))
}
