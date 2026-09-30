#' Colores para umbrales de calor (secuencial naranja)
#'
#' Una sola tonalidad de claro a oscuro: umbral más alto = más oscuro.
#' Rampa ordinal validada (monótona en luminosidad, extremo claro ≥ 2:1 de
#' contraste sobre fondo blanco).
#' @param umbrales Vector de umbrales (se ordenan de menor a mayor).
#' @return Vector nombrado de colores, nombres "> 25 °C", "> 30 °C", ...
paleta_umbrales_calor <- function(umbrales = c(25, 30, 33, 35, 36)) {
  rampa <- c("#f2a07a", "#ec7f4c", "#d9602a", "#b0461a", "#7d2f10",
             "#5c220b", "#3f1707")
  u <- sort(umbrales)
  stats::setNames(rampa[seq_along(u)], levels(etiqueta_umbral_calor(u)))
}
