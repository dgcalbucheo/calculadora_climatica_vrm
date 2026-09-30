#' Preparar una carpeta autocontenida para publicar la app (sin terra ni sf)
#'
#' Copia `app.R`, las funciones de `R/` que la app necesita y los datos por celda a
#' una carpeta nueva, que es la que se publica (p. ej. con `rsconnect::deployApp()`).
#' Se excluyen las funciones que usan `terra`/`sf` (solo sirven para construir los
#' datos), para que el servidor no tenga que instalar esas dependencias.
#'
#' @param dir Carpeta de salida (se borra y se vuelve a crear).
#' @param dir_celdas Carpeta con los datos por celda (`consolidar_cr2met_por_celda()`).
#' @param excluir Funciones de `R/` que no van al despliegue.
#' @return Invisible: ruta de `dir`. Informa archivos y tamaño.
preparar_deploy <- function(dir = "deploy_app",
                            dir_celdas = "data/processed/celdas",
                            excluir = c("leer_cr2met", "recortar_cr2met_mensual",
                                        "extent_regiones_chile", "serie_punto",
                                        "serie_clima_punto", "crear_mascara_cobertura",
                                        "punto_en_cobertura", "consolidar_cr2met_por_celda",
                                        "coord_a_wgs84",
                                        "preparar_deploy")) {
  if (!file.exists("app.R")) stop("Ejecutar desde la raíz del proyecto (no encuentro app.R)", call. = FALSE)
  if (!file.exists(file.path(dir_celdas, "grilla.rds")))
    stop("No encuentro ", dir_celdas, "/grilla.rds: consolidar los datos primero", call. = FALSE)
  unlink(dir, recursive = TRUE)
  dir.create(file.path(dir, "R"), recursive = TRUE)
  dir.create(file.path(dir, "data", "processed"), recursive = TRUE)

  file.copy("app.R", file.path(dir, "app.R"))
  fun <- list.files("R", "\\.R$", full.names = TRUE)
  fun <- fun[!sub("\\.R$", "", basename(fun)) %in% excluir]
  file.copy(fun, file.path(dir, "R"))
  file.copy(dir_celdas, file.path(dir, "data", "processed"), recursive = TRUE)

  # Aviso si quedó algo que dependa de terra/sf: haría que el servidor los compile
  cod <- unlist(lapply(list.files(file.path(dir, "R"), full.names = TRUE), readLines, warn = FALSE))
  cod <- c(cod, readLines(file.path(dir, "app.R"), warn = FALSE))
  cod <- cod[!grepl("^\\s*#", cod)]
  if (any(grepl("(terra|sf)::|library\\((terra|sf)\\)", cod)))
    warning("Quedó código que usa terra o sf en ", dir, ": revisar `excluir`", call. = FALSE)

  todo <- list.files(dir, recursive = TRUE, full.names = TRUE)
  message(sprintf("%s: %d archivos, %.0f MB (%d funciones de R/)", dir, length(todo),
                  sum(file.size(todo)) / 1e6, length(fun)))
  invisible(dir)
}
