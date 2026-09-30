#' Consolidar el recorte CR2MET en un archivo pequeño por celda
#'
#' Se ejecuta **una vez** (necesita `terra`). Lee los netCDF mensuales año por año,
#' se queda con las celdas que tocan el polígono de cobertura y escribe un `.rds`
#' por celda con la serie diaria completa (Tmin, Tmax y pr como enteros x `escala`,
#' comprimidos). Deja también `indice.csv` (celdas disponibles) y `grilla.rds`
#' (metadatos de la grilla), con lo que `serie_clima_celda()` funciona sin `terra`.
#'
#' Valores guardados con 3 decimales (`escala = 1000`): coincide con la precisión
#' útil de CR2MET; una diferencia de <0,0005 solo importaría si un valor cayera
#' justo sobre un umbral.
#'
#' @param dir_cr2met Carpeta con el recorte (netCDF mensuales).
#' @param dir_salida Carpeta de salida (se crea; por ejemplo `data/processed/celdas`).
#' @param mascara Ruta al .gpkg de cobertura (`crear_mascara_cobertura()`).
#' @param anios Años a incluir (NULL = todos los disponibles).
#' @param celdas_max Solo para pruebas: usar solo las primeras N celdas cubiertas.
#' @param escala Factor de conversión a entero.
#' @param bloque Celdas procesadas a la vez en la segunda fase (controla memoria).
#' @return Invisible: data.frame del índice de celdas.
consolidar_cr2met_por_celda <- function(dir_cr2met, dir_salida, mascara,
                                        anios = NULL, celdas_max = NULL,
                                        escala = 1000, bloque = 300) {
  t0 <- Sys.time()
  dir.create(dir_salida, recursive = TRUE, showWarnings = FALSE)
  tmp <- file.path(dir_salida, "_tmp")
  dir.create(tmp, showWarnings = FALSE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  # años disponibles (por nombre de archivo, mismo patrón que leer_cr2met)
  arch <- list.files(dir_cr2met, "^CR2MET.*\\.nc$", recursive = TRUE)
  disp <- sort(unique(as.integer(sub(".*_day_([0-9]{4})_[0-9]{2}_.*", "\\1", basename(arch)))))
  if (is.null(anios)) anios <- disp
  if (!all(anios %in% disp)) stop("Hay años pedidos sin archivos en ", dir_cr2met)
  anios <- sort(anios)

  # grilla y celdas cubiertas
  r0 <- leer_cr2met(dir_cr2met, "tmin", sprintf("%d-01-01", anios[1]), sprintf("%d-01-31", anios[1]))[[1]]
  rc <- terra::rasterize(terra::vect(mascara), r0, field = 1, touches = TRUE, background = 0)
  celdas <- which(terra::values(rc)[, 1] == 1)
  if (!is.null(celdas_max)) celdas <- utils::head(celdas, celdas_max)
  if (length(celdas) == 0) stop("Ninguna celda toca el polígono de cobertura")
  xy <- terra::xyFromCell(r0, celdas)
  message(length(celdas), " celdas cubiertas · ", length(anios), " años")

  # fase 1: un archivo temporal por año (días x celdas, enteros)
  a_int <- function(r) {
    v <- terra::values(r)[celdas, , drop = FALSE]
    storage.mode(v) <- "double"
    t(round(v * escala))
  }
  for (y in anios) {
    ini <- sprintf("%d-01-01", y); fin <- sprintf("%d-12-31", y)
    rt <- leer_cr2met(dir_cr2met, "tmin", ini, fin)
    fechas <- as.Date(terra::time(rt))
    tmx <- leer_cr2met(dir_cr2met, "tmax", ini, fin)
    prr <- leer_cr2met(dir_cr2met, "pr", ini, fin)
    if (!identical(fechas, as.Date(terra::time(tmx))) || !identical(fechas, as.Date(terra::time(prr)))) {
      stop("Fechas distintas entre variables en ", y)
    }
    saveRDS(list(fechas = fechas, tmin = a_int(rt), tmax = a_int(tmx), pr = a_int(prr)),
            file.path(tmp, sprintf("anio_%d.rds", y)))
    message("  año ", y, " listo")
  }

  # fase 2: por bloques de celdas, unir años y escribir un archivo por celda
  idx <- split(seq_along(celdas), ceiling(seq_along(celdas) / bloque))
  fechas_all <- NULL
  for (b in idx) {
    partes <- lapply(anios, function(y) {
      x <- readRDS(file.path(tmp, sprintf("anio_%d.rds", y)))
      list(f = x$fechas, tmin = x$tmin[, b, drop = FALSE], tmax = x$tmax[, b, drop = FALSE],
           pr = x$pr[, b, drop = FALSE])
    })
    f <- do.call(c, lapply(partes, `[[`, "f"))
    if (any(diff(f) != 1)) stop("La serie diaria no es continua entre años")
    fechas_all <- f
    tmin <- do.call(rbind, lapply(partes, `[[`, "tmin"))
    tmax <- do.call(rbind, lapply(partes, `[[`, "tmax"))
    pr   <- do.call(rbind, lapply(partes, `[[`, "pr"))
    for (j in seq_along(b)) {
      i <- b[j]
      saveRDS(list(celda = celdas[i], lon = xy[i, 1], lat = xy[i, 2],
                   fecha_ini = f[1], escala = escala,
                   tmin = as.integer(tmin[, j]), tmax = as.integer(tmax[, j]),
                   pr = as.integer(pr[, j])),
              file.path(dir_salida, sprintf("celda_%06d.rds", celdas[i])), compress = "xz")
    }
    message("  celdas ", min(b), "-", max(b), " escritas")
  }

  indice <- data.frame(celda = celdas, lon = xy[, 1], lat = xy[, 2],
                       archivo = sprintf("celda_%06d.rds", celdas))
  utils::write.csv(indice, file.path(dir_salida, "indice.csv"), row.names = FALSE)
  saveRDS(list(xmin = terra::xmin(r0), xmax = terra::xmax(r0), ymin = terra::ymin(r0),
               ymax = terra::ymax(r0), res = terra::res(r0)[1],
               nrow = terra::nrow(r0), ncol = terra::ncol(r0), escala = escala,
               fecha_ini = fechas_all[1], fecha_fin = fechas_all[length(fechas_all)],
               n_celdas = length(celdas),
               fuente = "CR2MET v2.5 (Boisier 2023), doi:10.5281/zenodo.7529682, CC BY 4.0"),
          file.path(dir_salida, "grilla.rds"))
  mb <- sum(file.size(file.path(dir_salida, indice$archivo))) / 1e6
  message(sprintf("Listo: %d celdas, %.1f MB en total (%.0f KB por celda), %.1f min",
                  nrow(indice), mb, 1000 * mb / nrow(indice),
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  invisible(indice)
}
