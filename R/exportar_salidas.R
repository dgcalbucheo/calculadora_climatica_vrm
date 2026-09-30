#' Exportar las salidas pedidas: figuras PNG, tablas (un Excel o CSV) y LEAME
#'
#' Fase final del flujo: recibe las tablas ya calculadas y genera, solo para las
#' salidas pedidas, sus figuras (300 dpi), sus tablas y el LEAME con fuente,
#' definiciones y limitaciones. Deja todo en `dir` (`figuras/`, `tablas.xlsx` o
#' `tablas/`, `LEAME.txt`); comprimir en un zip es tarea de la interfaz.
#'
#' @param res Salida de `tablas_salidas()`.
#' @param ids Ids del `catalogo_salidas()` a exportar (por defecto, todas).
#' @param dir Carpeta de salida (se crea).
#' @param parametros Lista nombrada por id con los argumentos de cada gráfico, p. ej.
#'   `list(heladas_anual = list(umbrales = c(0, -2)), heladas_mensual = list(meses = 5:10))`.
#' @param punto Lista con `lon` y `lat` pedidos, para el LEAME (opcional).
#' @param formato_tablas "xlsx" (un libro con una hoja por tabla, más una hoja LEAME)
#'   o "csv" (un archivo por tabla).
#' @return Invisible: vector con las rutas de los archivos escritos (relativas a `dir`).
exportar_salidas <- function(res, ids = catalogo_salidas()$id, dir,
                             parametros = list(), punto = NULL,
                             formato_tablas = c("xlsx", "csv")) {
  formato_tablas <- match.arg(formato_tablas)
  cat_ <- catalogo_salidas()
  desconocidos <- setdiff(ids, cat_$id)
  if (length(desconocidos)) {
    stop("Salidas que no existen en el catálogo: ", paste(desconocidos, collapse = ", "),
         call. = FALSE)
  }
  if (length(ids) == 0) stop("No se pidió ninguna salida", call. = FALSE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  archivos <- character()
  tablas <- list()
  leame <- leame_exportacion(res, ids, punto)

  for (id in ids) {
    e <- cat_[cat_$id == id, ]
    p <- do.call(match.fun(e$grafico), c(list(res), parametros[[id]]))
    guardar_figura(p, paste0(id, ".png"), ancho = e$ancho, alto = e$alto,
                   dir = file.path(dir, "figuras"))
    archivos <- c(archivos, file.path("figuras", paste0(id, ".png")))
    for (t in setdiff(strsplit(e$tablas, ",")[[1]], names(tablas))) tablas[[t]] <- res[[t]]
  }

  if (formato_tablas == "xlsx") {
    hojas <- stats::setNames(tablas, vapply(names(tablas), function(t) {
      x <- gsub("_", " ", t); paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
    }, character(1)))
    tablas_a_excel(hojas, file.path(dir, "tablas.xlsx"),
                   hojas_info = list(LEAME = data.frame(Texto = leame)))
    archivos <- c(archivos, "tablas.xlsx")
  } else {
    for (t in names(tablas)) {
      guardar_tabla(tablas[[t]], paste0(t, ".csv"), dir = file.path(dir, "tablas"))
      archivos <- c(archivos, file.path("tablas", paste0(t, ".csv")))
    }
  }
  writeLines(enc2utf8(leame), file.path(dir, "LEAME.txt"), useBytes = TRUE)
  invisible(c(archivos, "LEAME.txt"))
}
