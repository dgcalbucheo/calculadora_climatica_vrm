#' Gráfico: frecuencia mensual de heladas por umbral (promedio de días por mes)
#'
#' @param res Salida de `tablas_salidas()`.
#' @param meses Meses a mostrar (1 a 12). Por defecto abril–noviembre.
#' @param umbrales Umbrales a mostrar (NULL = todos).
#' @return Objeto ggplot.
grafico_heladas_mensual <- function(res, meses = 4:11, umbrales = NULL) {
  x <- res$heladas_mensual
  if (!is.null(umbrales)) x <- x[x$umbral %in% umbrales, ]
  x <- x[x$mes %in% meses, ]
  if (nrow(x) == 0) stop("Sin datos para los meses o umbrales pedidos", call. = FALSE)
  meses <- sort(unique(meses))
  nm <- nombres_meses()
  x$umbral <- etiqueta_umbral(x$umbral)
  x$mes <- factor(x$mes, levels = meses, labels = nm[meses])
  rango <- if (length(meses) > 1 && all(diff(meses) == 1))
    paste0(nm[meses[1]], "–", nm[meses[length(meses)]]) else "meses seleccionados"
  ggplot2::ggplot(x, ggplot2::aes(mes, media, fill = umbral)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.85), width = 0.8) +
    ggplot2::scale_fill_manual(values = paleta_umbrales(res$meta$umbrales_helada),
                               name = NULL) +
    ggplot2::labs(x = NULL, y = "Días de helada / mes (promedio)",
                  title = "Frecuencia mensual de heladas por umbral",
                  subtitle = paste0(rango, ", período ", res$meta$periodo_txt)) +
    tema_informe()
}
