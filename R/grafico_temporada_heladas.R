#' Gráfico: temporada de heladas (primera a última helada de cada año)
#'
#' @param res Salida de `tablas_salidas()`.
#' @param umbral Umbral de helada (°C). NULL = el más alto calculado (p. ej. 0 °C).
#' @param meses Meses que se rotulan en el eje (NULL = automático: desde un mes antes
#'   de la primera helada hasta un mes después de la última observadas).
#' @return Objeto ggplot. Si no hubo heladas con ese umbral, un gráfico con el aviso.
grafico_temporada_heladas <- function(res, umbral = NULL, meses = NULL) {
  if (is.null(umbral)) umbral <- max(res$meta$umbrales_helada)
  et <- levels(etiqueta_umbral(umbral))
  ttl <- paste0("Temporada de heladas (Tmin ", et, ")")
  x <- res$temporada_heladas
  x <- x[x$umbral == umbral & x$n_dias > 0, ]
  if (nrow(x) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0, y = 0, colour = "#52514e",
                        label = paste("Sin heladas", et, "en el período")) +
      ggplot2::labs(x = NULL, y = NULL, title = ttl,
                    subtitle = paste("Período", res$meta$periodo_txt)) +
      tema_informe() +
      ggplot2::theme(axis.text = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank()))
  }
  if (is.null(meses)) {
    m_ini <- min(as.integer(format(x$primera, "%m")))
    m_fin <- max(as.integer(format(x$ultima, "%m")))
    meses <- max(1, m_ini - 1):min(12, m_fin + 1)
  }
  nm <- nombres_meses()
  col <- paleta_fuentes[["CR2MET"]]
  ggplot2::ggplot(x, ggplot2::aes(y = anio)) +
    ggplot2::geom_segment(ggplot2::aes(x = doy_primera, xend = doy_ultima, yend = anio),
                          colour = col, linewidth = 0.8) +
    ggplot2::geom_point(ggplot2::aes(x = doy_ultima), colour = col, size = 2) +
    ggplot2::scale_x_continuous(
      breaks = as.integer(format(as.Date(paste0("2001-", meses, "-01")), "%j")),
      labels = nm[meses]) +
    ggplot2::scale_y_reverse(breaks = function(l) { b <- pretty(l); b[b == round(b)] }) +
    ggplot2::labs(x = NULL, y = NULL, title = ttl,
                  subtitle = paste0("Línea: primera a última helada del año; punto: última helada · ",
                                    res$meta$periodo_txt)) +
    tema_informe()
}
