#' Serie diaria de Tmin, Tmax y precipitación en un punto, desde los archivos por celda
#'
#' Ruta de la aplicación: usa solo R base (sin `terra`). Calcula qué celda de la
#' grilla contiene el punto, lee su archivo y recorta por fechas. Devuelve lo mismo
#' que `serie_clima_punto()`, así que `tablas_salidas()` funciona igual con ambas.
#' La "máscara" es el conjunto de celdas del índice (las que tocan el polígono de
#' cobertura): una celda costera que solo toca el polígono en parte se acepta.
#'
#' @param lon,lat Coordenadas WGS84.
#' @param fecha_ini,fecha_fin Fechas ("YYYY-MM-DD" o Date), ambas inclusive.
#' @param dir_celdas Carpeta creada por `consolidar_cr2met_por_celda()`.
#' @param periodo_disponible Vector de dos fechas del período disponible; NULL usa el
#'   período guardado en `grilla.rds`.
#' @return data.frame diario: fecha, tmin, tmax, pr, con atributo `celda`.
serie_clima_celda <- function(lon, lat, fecha_ini, fecha_fin, dir_celdas,
                              periodo_disponible = NULL) {
  g <- readRDS(file.path(dir_celdas, "grilla.rds"))
  if (is.null(periodo_disponible)) periodo_disponible <- c(g$fecha_ini, g$fecha_fin)
  q <- validar_consulta_punto(lon, lat, fecha_ini, fecha_fin, periodo_disponible)

  if (lon < g$xmin || lon > g$xmax || lat < g$ymin || lat > g$ymax) {
    stop("El punto está fuera del área cubierta (Regiones de Valparaíso y Metropolitana)",
         call. = FALSE)
  }
  col <- min(floor((lon - g$xmin) / g$res + 1e-9) + 1, g$ncol)
  fil <- min(floor((g$ymax - lat) / g$res + 1e-9) + 1, g$nrow)
  celda <- (fil - 1) * g$ncol + col
  archivo <- file.path(dir_celdas, sprintf("celda_%06d.rds", celda))
  if (!file.exists(archivo)) {
    stop("El punto está fuera de la Región de Valparaíso y la Región Metropolitana ",
         "(continente): puede caer en el mar, en otro país o en otra región", call. = FALSE)
  }
  x <- readRDS(archivo)
  n <- length(x$tmin)
  d <- data.frame(fecha = x$fecha_ini + seq_len(n) - 1,
                  tmin = x$tmin / x$escala, tmax = x$tmax / x$escala, pr = x$pr / x$escala)
  d <- d[d$fecha >= q$ini & d$fecha <= q$fin, ]
  if (nrow(d) == 0 || (all(is.na(d$tmin)) && all(is.na(d$tmax)) && all(is.na(d$pr)))) {
    stop("Sin datos CR2MET en ese punto (mar o fuera de Chile continental)", call. = FALSE)
  }
  rownames(d) <- NULL
  attr(d, "celda") <- c(lon = as.numeric(x$lon), lat = as.numeric(x$lat))
  d
}
