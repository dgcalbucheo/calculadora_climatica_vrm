#' Leer CR2MET recortado (archivos mensuales) como un SpatRaster diario
#'
#' Busca en `dir` los netCDF mensuales de una variable, selecciona los meses del
#' período según el nombre (`_AAAA_MM_`), los une y recorta por fecha exacta.
#' Los archivos de precipitación contienen `pr`; los de temperatura, `tmin` y
#' `tmax` (`CR2MET_tmin_tmax_...`).
#'
#' @param dir Carpeta con el recorte (p. ej. `data/raw/cr2met`).
#' @param variable "pr", "tmin" o "tmax".
#' @param inicio,fin Fechas "YYYY-MM-DD" (NULL = todo lo disponible).
#' @return SpatRaster con `terra::time()` diario.
leer_cr2met <- function(dir, variable = c("pr", "tmin", "tmax"),
                        inicio = NULL, fin = NULL) {
  variable <- match.arg(variable)
  marca <- if (variable == "pr") "_pr_" else "_tmin_tmax_"
  archivos <- list.files(dir, pattern = "^CR2MET.*\\.nc$", recursive = TRUE,
                         full.names = TRUE)
  archivos <- sort(archivos[grepl(marca, basename(archivos), fixed = TRUE)])
  if (length(archivos) == 0) stop("Sin archivos CR2MET para '", variable, "' en ", dir)

  f_mes <- as.Date(sub(".*_day_([0-9]{4})_([0-9]{2})_.*", "\\1-\\2-01", basename(archivos)))
  if (anyNA(f_mes)) stop("No pude leer año-mes en los nombres de archivo")
  sel <- rep(TRUE, length(archivos))
  if (!is.null(inicio)) sel <- sel & f_mes >= as.Date(format(as.Date(inicio), "%Y-%m-01"))
  if (!is.null(fin))    sel <- sel & f_mes <= as.Date(fin)
  archivos <- archivos[sel]
  if (length(archivos) == 0) stop("Sin archivos CR2MET en el período pedido")

  r <- do.call(c, lapply(archivos, terra::rast, subds = variable))
  t <- as.Date(terra::time(r))
  if (anyNA(t)) stop("Algún archivo no trae eje de tiempo")
  keep <- rep(TRUE, length(t))
  if (!is.null(inicio)) keep <- keep & t >= as.Date(inicio)
  if (!is.null(fin))    keep <- keep & t <= as.Date(fin)
  r[[which(keep)]]
}
