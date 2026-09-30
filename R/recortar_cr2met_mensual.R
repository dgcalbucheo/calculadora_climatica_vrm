#' Recortar CR2MET a un área conservando los archivos mensuales
#'
#' CR2MET v2.5 viene en un netCDF por mes para todo Chile. Esta función recorta
#' cada archivo a `ext_wgs84` (ajustado hacia afuera a la grilla original) y lo
#' guarda con el mismo nombre y subcarpeta en `dst`. No une meses: así el recorte
#' se puede comparar 1:1 con el original y leer con `leer_cr2met()`.
#' Se omite la máscara 2D `cl_mask` (sin eje de tiempo). Si el archivo de salida
#' ya existe se salta, de modo que se puede reanudar tras una interrupción.
#'
#' @param src Carpeta con los netCDF originales (busca recursivamente).
#' @param dst Carpeta de salida (se crea).
#' @param ext_wgs84 SpatExtent, p. ej. de `extent_regiones_chile()`.
#' @param n_esperado Si no es NULL, detiene la función si el número de
#'   archivos de entrada es distinto.
#' @param cada Cada cuántos archivos informar avance.
#' @return Rutas de salida (invisible).
recortar_cr2met_mensual <- function(src, dst, ext_wgs84, n_esperado = NULL,
                                    cada = 50) {
  archivos <- list.files(src, pattern = "^CR2MET.*\\.nc$", recursive = TRUE,
                         full.names = TRUE)
  if (!is.null(n_esperado) && length(archivos) != n_esperado) {
    stop("Se esperaban ", n_esperado, " archivos y hay ", length(archivos))
  }
  salidas <- file.path(dst, substring(archivos, nchar(src) + 2))
  for (i in seq_along(archivos)) {
    if (file.exists(salidas[i])) next
    dir.create(dirname(salidas[i]), recursive = TRUE, showWarnings = FALSE)
    tmp <- file.path(dirname(salidas[i]), paste0("~", basename(salidas[i])))
    s <- terra::sds(archivos[i])
    s <- s[which(names(s) != "cl_mask")]
    terra::writeCDF(terra::crop(s, ext_wgs84, snap = "out"), tmp,
                    overwrite = TRUE, compression = 4)
    file.rename(tmp, salidas[i])
    if (i %% cada == 0) message(i, "/", length(archivos))
  }
  invisible(salidas)
}
