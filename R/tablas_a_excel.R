#' Escribir un conjunto de tablas en un libro Excel con formato
#'
#' Una hoja por tabla, con encabezado en negrita, primera fila fija, ancho de
#' columnas según el contenido y números redondeados. Opcionalmente antepone hojas
#' informativas (p. ej. el LEAME). Requiere el paquete `openxlsx`.
#'
#' @param tablas Lista nombrada de data.frames (nombre = nombre de hoja, se recorta
#'   a 31 caracteres).
#' @param archivo Ruta del .xlsx de salida.
#' @param hojas_info Lista nombrada de data.frames que van primero.
#' @param decimales Decimales para redondear columnas numéricas.
#' @param etiquetar Si TRUE aplica `etiquetar_columnas()` a `tablas`.
#' @return Invisible: ruta del archivo.
tablas_a_excel <- function(tablas, archivo, hojas_info = list(), decimales = 2,
                           etiquetar = TRUE) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Falta el paquete 'openxlsx' (renv::install(\"openxlsx\"))", call. = FALSE)
  }
  wb <- openxlsx::createWorkbook()
  estilo_enc <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#E1E0D9",
                                      border = "bottom", wrapText = TRUE,
                                      valign = "center")
  agregar <- function(nombre, df) {
    hoja <- substr(gsub("[\\[\\]\\*\\?/\\\\:]", "-", nombre), 1, 31)
    num <- vapply(df, is.numeric, logical(1))
    df[num] <- lapply(df[num], function(x) round(x, decimales))
    openxlsx::addWorksheet(wb, hoja)
    openxlsx::writeData(wb, hoja, df, headerStyle = estilo_enc)
    openxlsx::freezePane(wb, hoja, firstRow = TRUE)
    contenido <- vapply(df, function(x)
      suppressWarnings(max(nchar(as.character(utils::head(x, 100))), 0, na.rm = TRUE)),
      numeric(1))
    anchos <- pmin(pmax(nchar(names(df)), contenido, 10), 60)
    openxlsx::setColWidths(wb, hoja, cols = seq_along(df), widths = anchos)
  }
  for (n in names(hojas_info)) agregar(n, hojas_info[[n]])
  for (n in names(tablas)) {
    df <- tablas[[n]]
    if (etiquetar) df <- etiquetar_columnas(df)
    agregar(n, df)
  }
  dir.create(dirname(archivo), showWarnings = FALSE, recursive = TRUE)
  openxlsx::saveWorkbook(wb, archivo, overwrite = TRUE)
  invisible(archivo)
}
