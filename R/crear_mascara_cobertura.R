#' Crear la máscara de cobertura (polígono único) desde un shapefile comunal
#'
#' Se ejecuta **una sola vez** (necesita `sf` y el shapefile comunal). Deja las
#' regiones pedidas, descarta las comunas insulares (Isla de Pascua, Juan Fernández),
#' une todo en un solo polígono, lo reproyecta a WGS84 y lo guarda como GeoPackage.
#' Después `punto_en_cobertura()` lo lee solo con `terra`.
#'
#' @param shp Ruta al shapefile comunal (cualquier CRS).
#' @param salida Ruta del .gpkg de salida (se sobrescribe).
#' @param cod_regiones Códigos de región (5 = Valparaíso, 13 = Metropolitana).
#' @param col_region Nombre de la columna con el código de región.
#' @param lon_min Longitud bajo la cual una comuna se considera insular.
#' @return Invisible: ruta de `salida`.
crear_mascara_cobertura <- function(shp, salida, cod_regiones = c(5, 13),
                                    col_region = "codregion", lon_min = -75) {
  com <- sf::st_read(shp, quiet = TRUE)
  com <- com[com[[col_region]] %in% cod_regiones, ]
  com <- sf::st_transform(sf::st_make_valid(com), 4326)
  xmin_com <- vapply(sf::st_geometry(com),
                     function(g) sf::st_bbox(g)[["xmin"]], numeric(1))
  com <- com[xmin_com > lon_min, ]
  if (nrow(com) == 0) stop("Sin comunas para las regiones pedidas")
  area <- sf::st_sf(nombre = "cobertura_vrm",
                    geometry = sf::st_make_valid(sf::st_union(sf::st_geometry(com))))
  dir.create(dirname(salida), showWarnings = FALSE, recursive = TRUE)
  sf::st_write(area, salida, delete_dsn = TRUE, quiet = TRUE)
  message("Máscara guardada: ", salida, " (", nrow(com), " comunas)")
  invisible(salida)
}
