#' Gráfico: días cálidos (Tmax sobre umbral) por año y umbral
#'
#' @param res Salida de `tablas_salidas()`.
#' @param umbrales Umbrales a mostrar (NULL = todos los calculados).
#' @return Objeto ggplot.
grafico_dias_calidos_anual <- function(res, umbrales = NULL) {
  x <- res$calor_anual
  if (!is.null(umbrales)) x <- x[x$umbral %in% umbrales, ]
  if (nrow(x) == 0) stop("Ningún umbral pedido coincide con los calculados", call. = FALSE)
  x$umbral <- etiqueta_umbral_calor(x$umbral)
  ggplot2::ggplot(x, ggplot2::aes(anio, supera, colour = umbral)) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::scale_colour_manual(values = paleta_umbrales_calor(res$meta$umbrales_calor),
                                 name = NULL) +
    ggplot2::scale_x_continuous(breaks = function(l) { b <- pretty(l); b[b == round(b)] }) +
    ggplot2::labs(x = NULL, y = "Días / año",
                  title = "Días cálidos por año y umbral",
                  subtitle = paste("Período", res$meta$periodo_txt)) +
    tema_informe()
}
