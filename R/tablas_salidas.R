#' Todas las tablas que alimentan las salidas (gráficos y CSV) de la calculadora
#'
#' Calcula, a partir de la serie diaria de `serie_clima_punto()`, las tablas de la
#' versión 1: heladas, temperatura, días cálidos y precipitación. Solo usa **años
#' completos** (enero a diciembre): así las climatologías mensuales no se sesgan por
#' meses ausentes. Los años parciales del rango pedido se descartan y se informan en
#' `meta`.
#'
#' @param serie data.frame diario: fecha, tmin, tmax, pr.
#' @param umbrales_helada,umbrales_calor,dia_lluvia_mm Ver `metricas_punto()`.
#' @return Lista con las tablas `heladas_anual`, `heladas_resumen`,
#'   `heladas_mensual`, `temporada_heladas`, `temperatura_mensual`, `calor_anual`,
#'   `calor_resumen`, `precip_mensual`, `precip_anual`, y `meta` (años usados y
#'   descartados, texto del período, umbrales, celda CR2MET).
tablas_salidas <- function(serie, umbrales_helada = c(0, -1, -2, -3, -4),
                           umbrales_calor = c(25, 30, 33, 35, 36),
                           dia_lluvia_mm = 1) {
  celda <- attr(serie, "celda")
  m <- metricas_punto(serie, umbrales_helada, umbrales_calor, dia_lluvia_mm)

  todos  <- m$precip$anio
  anios  <- todos[m$precip$anio_completo]
  if (length(anios) == 0) {
    stop("El período pedido no incluye ningún año completo (enero a diciembre); ",
         "amplíalo para calcular climatologías y totales anuales", call. = FALSE)
  }
  solo <- function(x) { x <- x[x$anio %in% anios, ]; rownames(x) <- NULL; x }
  serie <- serie[as.integer(format(serie$fecha, "%Y")) %in% anios, ]
  serie$mes <- as.integer(format(serie$fecha, "%m"))

  # Heladas
  hel <- dias_helada(serie, umbrales_helada)
  hel_mes <- totales_periodo(hel, "helada", grupos = "umbral")
  heladas_mensual <- climatologia_mensual(hel_mes, "helada", grupos = "umbral")
  temporada <- fechas_helada_anual(hel)

  # Temperatura mensual
  serie$tmed <- (serie$tmin + serie$tmax) / 2
  q <- function(x, p) unname(stats::quantile(x, p, na.rm = TRUE))
  temperatura_mensual <- do.call(rbind, lapply(split(serie, serie$mes), function(d) {
    data.frame(mes = d$mes[1],
               tmin_media = mean(d$tmin, na.rm = TRUE), tmin_p10 = q(d$tmin, 0.10),
               tmed_media = mean(d$tmed, na.rm = TRUE),
               tmax_media = mean(d$tmax, na.rm = TRUE), tmax_p90 = q(d$tmax, 0.90))
  }))

  # Precipitación
  pr_mes <- totales_periodo(serie, "pr")
  serie$lluvia <- serie$pr >= dia_lluvia_mm
  ll_mes <- totales_periodo(serie, "lluvia")
  precip_mensual <- do.call(rbind, lapply(split(pr_mes, pr_mes$mes), function(d) {
    data.frame(mes = d$mes[1], media = mean(d$pr), mediana = stats::median(d$pr),
               p25 = q(d$pr, 0.25), p75 = q(d$pr, 0.75), max = max(d$pr))
  }))
  precip_mensual$dias_lluvia <- vapply(precip_mensual$mes, function(k)
    mean(ll_mes$lluvia[ll_mes$mes == k]), numeric(1))

  pa <- solo(m$precip)
  pa$media_periodo <- mean(pa$pr_mm)
  pa$anomalia_pct <- 100 * (pa$pr_mm - pa$media_periodo) / pa$media_periodo
  pa$clase <- factor(rep(NA_character_, nrow(pa)), levels = c("Seco", "Normal", "Húmedo"))
  if (nrow(pa) >= 3) {
    pa$clase <- tryCatch(
      cut(pa$pr_mm, stats::quantile(pa$pr_mm, c(0, 1/3, 2/3, 1)),
          labels = c("Seco", "Normal", "Húmedo"), include.lowest = TRUE),
      error = function(e) pa$clase)
  }

  ha <- solo(m$helada)
  ca <- solo(m$calor)
  rango <- range(anios)
  list(
    heladas_anual = ha,
    heladas_resumen = resumen_por_umbral(ha, "helada"),
    heladas_mensual = heladas_mensual,
    temporada_heladas = temporada,
    temperatura_mensual = temperatura_mensual,
    calor_anual = ca,
    calor_resumen = resumen_por_umbral(ca, "supera"),
    precip_mensual = precip_mensual,
    precip_anual = pa,
    meta = list(
      anios = anios, anios_descartados = setdiff(todos, anios),
      periodo_txt = if (rango[1] == rango[2]) as.character(rango[1])
                    else sprintf("%d–%d (%d años)", rango[1], rango[2], length(anios)),
      umbrales_helada = umbrales_helada, umbrales_calor = umbrales_calor,
      dia_lluvia_mm = dia_lluvia_mm, celda = celda)
  )
}
