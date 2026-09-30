#' Etiqueta de umbral de calor para tablas y leyendas
#' @param u Umbral numérico.
#' @return Factor con niveles ordenados de menor a mayor ("> 25 °C", ...).
etiqueta_umbral_calor <- function(u) {
  factor(paste0("> ", u, " °C"),
         levels = paste0("> ", sort(unique(u)), " °C"))
}
