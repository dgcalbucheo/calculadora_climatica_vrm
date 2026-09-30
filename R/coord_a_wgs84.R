#' Convertir una coordenada a WGS84 (lon/lat)
#'
#' Pensada para la capa de interfaz: el núcleo (`serie_clima_punto()`) recibe
#' siempre lon/lat en WGS84, y esta función convierte desde otro CRS antes de llamarlo
#' (p. ej. UTM 19S, "EPSG:32719").
#'
#' @param x,y Coordenadas en el CRS de origen (un solo punto).
#' @param crs CRS de origen: código EPSG ("EPSG:32719" o 32719) o cualquier
#'   definición que entienda `terra`. Por defecto WGS84 (x = lon, y = lat).
#' @return Vector con nombres: `c(lon = , lat = )`.
coord_a_wgs84 <- function(x, y, crs = "EPSG:4326") {
  if (!is.numeric(x) || !is.numeric(y) || length(x) != 1 || length(y) != 1 ||
      anyNA(c(x, y)) || !all(is.finite(c(x, y)))) {
    stop("x e y deben ser números únicos y finitos", call. = FALSE)
  }
  if (is.numeric(crs)) crs <- paste0("EPSG:", crs)
  if (identical(toupper(crs), "EPSG:4326")) return(c(lon = x, lat = y))
  p <- terra::project(terra::vect(cbind(x, y), crs = crs), "EPSG:4326")
  xy <- terra::crds(p)
  c(lon = unname(xy[1, 1]), lat = unname(xy[1, 2]))
}
