#' Clasificar días sobre un umbral (p. ej. días cálidos por Tmax)
#'
#' Un día supera el umbral si el valor es estrictamente mayor que el umbral
#' (Tmax > 30 °C), o mayor o igual si `inclusivo = TRUE` (precipitación
#' ≥ 1 mm). Formato largo (un registro por día y umbral), compatible con
#' totales_periodo() y climatologia_mensual().
#'
#' @param diario data.frame con `fecha` y la columna a evaluar.
#' @param umbrales Umbrales (misma unidad que `col`).
#' @param col Columna a evaluar (por defecto "tmax").
#' @param grupos Columnas adicionales a conservar (p. ej. "fuente").
#' @param inclusivo Si TRUE usa ≥ en vez de >.
#' @return data.frame: grupos, fecha, umbral, supera (lógico), valor.
dias_sobre_umbral <- function(diario, umbrales = c(25, 30, 33, 35, 36),
                              col = "tmax", grupos = NULL, inclusivo = FALSE) {
  base <- diario[!is.na(diario[[col]]), c(grupos, "fecha", col), drop = FALSE]
  do.call(rbind, lapply(umbrales, function(u) {
    data.frame(base[, c(grupos, "fecha"), drop = FALSE],
               umbral = u,
               supera = if (inclusivo) base[[col]] >= u else base[[col]] > u,
               valor = base[[col]])
  }))
}
