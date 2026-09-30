#' Extent (WGS84) de una o más regiones de Chile continental
#'
#' Lee un shapefile comunal con una columna de código de región, deja las
#' regiones pedidas, descarta las comunas insulares (Isla de Pascua, Juan
#' Fernández: xmin < `lon_min`), reproyecta a WGS84 y devuelve el bbox con
#' margen.
#'
#' @param shp Ruta al shapefile comunal (cualquier CRS).
#' @param cod_regiones Códigos de región (5 = Valparaíso, 13 = Metropolitana).
#' @param margen Margen en grados que se suma a cada lado.
#' @param col_region Nombre de la columna con el código de región.
#' @param lon_min Longitud bajo la cual una comuna se considera insular.
#' @return SpatExtent (terra) en WGS84.
extent_regiones_chile <- function(shp, cod_regiones = c(5, 13), margen = 0.1,
                                  col_region = "codregion", lon_min = -75) {
  com <- sf::st_read(shp, quiet = TRUE)
  com <- com[com[[col_region]] %in% cod_regiones, ]
  com <- sf::st_transform(com, 4326)
  xmin_com <- vapply(sf::st_geometry(com),
                     function(g) sf::st_bbox(g)[["xmin"]], numeric(1))
  com <- com[xmin_com > lon_min, ]
  if (nrow(com) == 0) stop("Sin comunas para las regiones pedidas")
  bb <- sf::st_bbox(com)
  terra::ext(bb[["xmin"]] - margen, bb[["xmax"]] + margen,
             bb[["ymin"]] - margen, bb[["ymax"]] + margen)
}
