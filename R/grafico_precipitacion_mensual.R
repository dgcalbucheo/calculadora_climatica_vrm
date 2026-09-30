#' Gráfico: precipitación mensual (media y rango intercuartil entre años)
#'
#' @param res Salida de `tablas_salidas()`.
#' @return Objeto ggplot.
grafico_precipitacion_mensual <- function(res) {
  x <- res$precip_mensual
  x$mes <- factor(x$mes, levels = 1:12, labels = nombres_meses())
  ggplot2::ggplot(x, ggplot2::aes(mes, media)) +
    ggplot2::geom_col(fill = paleta_fuentes[["CR2MET"]], width = 0.8) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = p25, ymax = p75), width = 0.25,
                           colour = "#52514e", linewidth = 0.4) +
    ggplot2::labs(x = NULL, y = "mm / mes", title = "Precipitación mensual",
                  subtitle = paste0("Barra: media ", res$meta$periodo_txt,
                                    "; línea: rango intercuartil entre años")) +
    tema_informe()
}
