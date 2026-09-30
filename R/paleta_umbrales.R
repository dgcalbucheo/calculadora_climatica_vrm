#' Umbrales de helada: una sola tonalidad (azul) de claro a oscuro, porque
#' son magnitudes ordenadas (≤0 claro … ≤−4 oscuro).
#'
#' Colores para umbrales de helada (secuencial azul)
#' @param umbrales Vector de umbrales (se ordenan de mayor a menor).
#' @return Vector nombrado de colores, nombres "≤ 0 °C", "≤ -1 °C", ...
paleta_umbrales <- function(umbrales = c(0, -1, -2, -3, -4)) {
  rampa <- c("#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#0d366b",
             "#082a55", "#051f40")
  u <- sort(umbrales, decreasing = TRUE)
  stats::setNames(rampa[seq_along(u)], etiqueta_umbral(u))
}
