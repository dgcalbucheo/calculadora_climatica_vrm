#' Guardar una figura ggplot en output/figuras (PNG, 300 dpi, fondo blanco)
#'
#' @param p Objeto ggplot/patchwork.
#' @param nombre Nombre del archivo (p. ej. "01_heladas_mensual.png").
#' @param ancho,alto Tamaño en pulgadas.
#' @param dir Carpeta de salida.
#' @return Invisible: el gráfico (para poder imprimirlo en el mismo chunk).
guardar_figura <- function(p, nombre, ancho = 9, alto = 5,
                           dir = here::here("output", "figuras")) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(file.path(dir, nombre), p, width = ancho, height = alto,
                  dpi = 300, bg = "white")
  p
}
