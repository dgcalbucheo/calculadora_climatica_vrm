#' ¿El punto cae dentro del área de cobertura (regiones de Valparaíso y RM)?
#'
#' @param lon,lat Coordenadas WGS84.
#' @param mascara Ruta al .gpkg creado con `crear_mascara_cobertura()`, o un
#'   SpatVector ya leído (conviene leerlo una vez y reutilizarlo en la app).
#' @return TRUE/FALSE.
punto_en_cobertura <- function(lon, lat, mascara) {
  m <- if (inherits(mascara, "SpatVector")) mascara else terra::vect(mascara)
  p <- terra::vect(cbind(lon, lat), crs = "EPSG:4326")
  if (!terra::same.crs(m, p)) p <- terra::project(p, terra::crs(m))
  any(as.vector(terra::relate(p, m, "intersects")))
}
