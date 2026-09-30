#' Métricas anuales de heladas, calor y precipitación de una serie diaria
#'
#' Reúne `dias_helada()`, `dias_sobre_umbral()` y `totales_periodo()` sobre la
#' salida de `serie_clima_punto()`. Cada tabla anual trae `n_dias` (días con dato
#' en ese año) y `anio_completo`: si el rango pedido parte o termina a mitad de
#' año, esos años salen marcados y no deben leerse como año completo.
#'
#' @param serie data.frame con fecha, tmin, tmax, pr.
#' @param umbrales_helada Umbrales de helada en °C (Tmin <= umbral).
#' @param umbrales_calor Umbrales de calor en °C (Tmax > umbral).
#' @param dia_lluvia_mm Un día es de lluvia si pr >= este valor (mm).
#' @return Lista con `helada` (umbral, anio, helada, n_dias, anio_completo),
#'   `calor` (umbral, anio, supera, ...) y `precip` (anio, pr_mm, dias_lluvia, ...).
metricas_punto <- function(serie, umbrales_helada = c(0, -1, -2, -3, -4),
                           umbrales_calor = c(25, 30, 33, 35, 36),
                           dia_lluvia_mm = 1) {
  anio <- as.integer(format(serie$fecha, "%Y"))
  n <- stats::aggregate(list(n_dias = rep(1L, length(anio))), list(anio = anio), sum)
  bisiesto <- (n$anio %% 4 == 0 & n$anio %% 100 != 0) | n$anio %% 400 == 0
  n$anio_completo <- n$n_dias == ifelse(bisiesto, 366L, 365L)
  con_n <- function(x) {
    x <- merge(x, n, by = "anio")
    x[do.call(order, x[, intersect(c("umbral", "anio"), names(x)), drop = FALSE]), ]
  }

  helada <- totales_periodo(dias_helada(serie, umbrales_helada),
                            "helada", grupos = "umbral", por_mes = FALSE)
  calor <- totales_periodo(dias_sobre_umbral(serie, umbrales_calor),
                           "supera", grupos = "umbral", por_mes = FALSE)
  pr_mm <- totales_periodo(serie, "pr", por_mes = FALSE)
  names(pr_mm)[names(pr_mm) == "pr"] <- "pr_mm"
  llu <- totales_periodo(dias_sobre_umbral(serie, dia_lluvia_mm, col = "pr", inclusivo = TRUE),
                         "supera", por_mes = FALSE)
  names(llu)[names(llu) == "supera"] <- "dias_lluvia"

  list(helada = con_n(helada),
       calor  = con_n(calor),
       precip = con_n(merge(pr_mm, llu, by = "anio")))
}
