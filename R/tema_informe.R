#' Tema ggplot sobrio para informe
#' @param base_size Tamaño base de texto.
tema_informe <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#e1e0d9", linewidth = 0.3),
      axis.text = ggplot2::element_text(colour = "#52514e"),
      axis.title = ggplot2::element_text(colour = "#52514e"),
      plot.title = ggplot2::element_text(colour = "#0b0b0b", face = "bold"),
      plot.subtitle = ggplot2::element_text(colour = "#52514e"),
      legend.position = "top", legend.justification = "left",
      strip.text = ggplot2::element_text(face = "bold", hjust = 0)
    )
}
