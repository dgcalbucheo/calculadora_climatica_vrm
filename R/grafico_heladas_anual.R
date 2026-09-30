#' Gráfico: días de helada por año y umbral
#'
#' @param res Salida de `tablas_salidas()`.
#' @param umbrales Umbrales a mostrar (NULL = todos los calculados).
#' @return Objeto ggplot.
grafico_heladas_anual <- function(res, umbrales = NULL) {
  x <- res$heladas_anual
  if (!is.null(umbrales)) x <- x[x$umbral %in% umbrales, ]
  if (nrow(x) == 0) stop("Ningún umbral pedido coincide con los calculados", call. = FALSE)
  x$umbral <- etiqueta_umbral(x$umbral)
  ggplot2::ggplot(x, ggplot2::aes(anio, helada, colour = umbral)) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::scale_colour_manual(values = paleta_umbrales(res$meta$umbrales_helada),
                                 name = NULL) +
    ggplot2::scale_x_continuous(breaks = function(l) { b <- pretty(l); b[b == round(b)] }) +
    ggplot2::labs(x = NULL, y = "Días de helada / año",
                  title = "Días de helada por año y umbral",
                  subtitle = paste("Período", res$meta$periodo_txt)) +
    tema_informe()
}
