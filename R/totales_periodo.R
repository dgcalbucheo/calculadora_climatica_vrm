#' Totales por año (y opcionalmente por mes) de una variable diaria
#'
#' Suma (o aplica `fun`) una variable diaria por año-mes o por año, dentro de
#' grupos (fuente, umbral...). Útil para días de helada, horas bajo umbral,
#' días de calor, etc.
#'
#' @param df data.frame con `fecha` y la columna `valor`.
#' @param valor Nombre de la columna a agregar (lógico o numérico).
#' @param grupos Columnas de agrupación.
#' @param por_mes Si TRUE, agrega por año y mes; si FALSE, por año.
#' @param meses Meses a incluir (NULL = todos), p. ej. 9:11 para sep-oct-nov.
#' @param fun Función de agregación.
#' @return data.frame: grupos, anio, [mes], valor.
totales_periodo <- function(df, valor, grupos = NULL, por_mes = TRUE,
                            meses = NULL, fun = sum) {
  df$anio <- as.integer(format(df$fecha, "%Y"))
  df$mes  <- as.integer(format(df$fecha, "%m"))
  if (!is.null(meses)) df <- df[df$mes %in% meses, ]
  claves <- c(grupos, "anio", if (por_mes) "mes")
  x <- stats::aggregate(list(valor = as.numeric(df[[valor]])),
                        df[, claves, drop = FALSE], fun)
  names(x)[names(x) == "valor"] <- valor
  x[do.call(order, x[, claves, drop = FALSE]), ]
}
