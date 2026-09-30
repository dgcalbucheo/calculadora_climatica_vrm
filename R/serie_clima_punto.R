#' Serie diaria de Tmin, Tmax y precipitación (CR2MET) en un punto, desde los netCDF
#'
#' Ruta "de laboratorio": lee el recorte CR2MET (netCDF mensuales) con `terra`.
#' Sirve para validar y para construir los archivos por celda; la app usa
#' `serie_clima_celda()`, que no necesita `terra`.
#'
#' @param lon,lat Coordenadas WGS84 (grados decimales). Otro CRS: convertir antes
#'   con `coord_a_wgs84()`.
#' @param fecha_ini,fecha_fin Fechas ("YYYY-MM-DD" o Date), ambas inclusive.
#' @param dir_cr2met Carpeta con el recorte (p. ej. `data/raw/cr2met`).
#' @param periodo_disponible Vector de dos fechas (inicio, fin) del período que
#'   cubre la base (`parametros.yml: periodo$completo`). NULL = no validar.
#' @param mascara Ruta al .gpkg de cobertura (o SpatVector) creado con
#'   `crear_mascara_cobertura()`. Si se entrega, se rechazan los puntos fuera del
#'   área (mar, Argentina, otras regiones), que CR2MET igual trae con valores.
#' @return data.frame diario: fecha, tmin, tmax, pr. Con atributo `celda`
#'   (`c(lon =, lat =)`: centro de la celda CR2MET usada).
serie_clima_punto <- function(lon, lat, fecha_ini, fecha_fin, dir_cr2met,
                              periodo_disponible = NULL, mascara = NULL) {
  q <- validar_consulta_punto(lon, lat, fecha_ini, fecha_fin, periodo_disponible)
  ini <- q$ini; fin <- q$fin

  tmin <- leer_cr2met(dir_cr2met, "tmin", ini, fin)
  if (lon < terra::xmin(tmin) || lon > terra::xmax(tmin) ||
      lat < terra::ymin(tmin) || lat > terra::ymax(tmin)) {
    stop("El punto está fuera del área cubierta (Regiones de Valparaíso y Metropolitana)",
         call. = FALSE)
  }
  if (!is.null(mascara) && !punto_en_cobertura(lon, lat, mascara)) {
    stop("El punto está fuera de la Región de Valparaíso y la Región Metropolitana ",
         "(continente): puede caer en el mar, en otro país o en otra región", call. = FALSE)
  }
  tmax <- leer_cr2met(dir_cr2met, "tmax", ini, fin)
  pr   <- leer_cr2met(dir_cr2met, "pr",   ini, fin)

  d <- Reduce(function(a, b) merge(a, b, by = "fecha"),
              list(serie_punto(tmin, lon, lat, "tmin"),
                   serie_punto(tmax, lon, lat, "tmax"),
                   serie_punto(pr,   lon, lat, "pr")))
  if (all(is.na(d$tmin)) && all(is.na(d$tmax)) && all(is.na(d$pr))) {
    stop("Sin datos CR2MET en ese punto (mar o fuera de Chile continental)", call. = FALSE)
  }
  d <- d[order(d$fecha), ]
  rownames(d) <- NULL
  ce <- terra::xyFromCell(tmin, terra::cellFromXY(tmin, cbind(lon, lat)))
  attr(d, "celda") <- c(lon = unname(ce[1, 1]), lat = unname(ce[1, 2]))
  d
}
