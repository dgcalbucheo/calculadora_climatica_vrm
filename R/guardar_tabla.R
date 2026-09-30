#' Guardar una tabla de resultados en output/tablas (CSV, UTF-8)
#'
#' @param x data.frame.
#' @param nombre Nombre del archivo (p. ej. "01_heladas_mensual.csv").
#' @param dir Carpeta de salida.
#' @return Invisible: ruta del archivo.
guardar_tabla <- function(x, nombre, dir = here::here("output", "tablas")) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  ruta <- file.path(dir, nombre)
  utils::write.csv(x, ruta, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(ruta)
}
