#' Etiqueta de umbral para leyendas
#' @param u Umbral numérico.
etiqueta_umbral <- function(u) {
  factor(paste0("≤ ", u, " °C"),
         levels = paste0("≤ ", sort(unique(u), decreasing = TRUE), " °C"))
}
