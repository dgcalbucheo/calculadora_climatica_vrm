#' Gráfico: temperatura media mensual (Tmin, Tmed, Tmax)
#'
#' Tmed se aproxima como (Tmin + Tmax) / 2.
#'
#' @param res Salida de `tablas_salidas()`.
#' @return Objeto ggplot.
grafico_temperatura_mensual <- function(res) {
  t <- res$temperatura_mensual
  x <- rbind(data.frame(mes = t$mes, variable = "Tmin", valor = t$tmin_media),
             data.frame(mes = t$mes, variable = "Tmed", valor = t$tmed_media),
             data.frame(mes = t$mes, variable = "Tmax", valor = t$tmax_media))
  x$variable <- factor(x$variable, c("Tmin", "Tmed", "Tmax"))
  col <- paleta_fuentes[["CR2MET"]]
  ggplot2::ggplot(x, ggplot2::aes(mes, valor)) +
    ggplot2::geom_line(colour = col, linewidth = 0.7) +
    ggplot2::geom_point(colour = col, size = 1.8) +
    ggplot2::facet_wrap(~variable, nrow = 1) +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = substr(nombres_meses(), 1, 1)) +
    ggplot2::labs(x = NULL, y = "°C", title = "Temperatura media mensual",
                  subtitle = paste("Período", res$meta$periodo_txt)) +
    tema_informe()
}
