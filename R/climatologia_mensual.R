#' Climatología mensual a partir de totales año-mes
#'
#' Promedia entre años los totales mensuales. Incluye los meses sin eventos
#' (valor 0) para que el promedio no quede sesgado.
#'
#' @param tot Salida de totales_periodo(por_mes = TRUE).
#' @param valor Columna de valores.
#' @param grupos Columnas de agrupación.
#' @return data.frame: grupos, mes, media, sd, min, max, prop_anios (fracción
#'   de años con valor > 0), n_anios.
climatologia_mensual <- function(tot, valor, grupos = NULL) {
  # completar combinaciones grupo × año × mes faltantes con 0
  anios <- sort(unique(tot$anio))
  comb <- unique(tot[, grupos, drop = FALSE])
  comb <- merge(comb, expand.grid(anio = anios, mes = 1:12))
  tot <- merge(comb, tot, all.x = TRUE)
  tot[[valor]][is.na(tot[[valor]])] <- 0

  f <- function(x) c(media = mean(x), sd = stats::sd(x), min = min(x),
                     max = max(x), prop_anios = mean(x > 0), n_anios = length(x))
  x <- stats::aggregate(list(v = tot[[valor]]), tot[, c(grupos, "mes"), drop = FALSE], f)
  out <- cbind(x[, c(grupos, "mes"), drop = FALSE], as.data.frame(x$v))
  out[do.call(order, out[, c(grupos, "mes"), drop = FALSE]), ]
}
