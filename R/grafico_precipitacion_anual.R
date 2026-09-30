#' Gráfico: precipitación anual y media del período
#'
#' @param res Salida de `tablas_salidas()`.
#' @return Objeto ggplot.
grafico_precipitacion_anual <- function(res) {
  x <- res$precip_anual
  col <- paleta_fuentes[["CR2MET"]]
  ggplot2::ggplot(x, ggplot2::aes(anio, pr_mm)) +
    ggplot2::geom_col(fill = col, width = 0.8) +
    ggplot2::geom_hline(yintercept = x$media_periodo[1], colour = col,
                        linetype = 2, linewidth = 0.5) +
    ggplot2::scale_x_continuous(breaks = function(l) { b <- pretty(l); b[b == round(b)] }) +
    ggplot2::labs(x = NULL, y = "mm / año", title = "Precipitación anual",
                  subtitle = paste0("Línea discontinua: media ", res$meta$periodo_txt)) +
    tema_informe()
}
