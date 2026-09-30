#' Serie temporal de un SpatRaster multitemporal en un punto
#'
#' @param r SpatRaster con terra::time() definido.
#' @param lon,lat Coordenadas WGS84 (se reproyectan si hace falta).
#' @param nombre Nombre de la columna de valores.
#' @return data.frame con fecha y valor.
serie_punto <- function(r, lon, lat, nombre = "valor") {
  p <- terra::vect(cbind(lon, lat), crs = "EPSG:4326")
  if (!terra::same.crs(r, p)) p <- terra::project(p, terra::crs(r))
  v <- as.numeric(terra::extract(r, p, ID = FALSE)[1, ])
  out <- data.frame(fecha = as.Date(terra::time(r)), v)
  names(out)[2] <- nombre
  out
}
