#' Renombrar columnas técnicas con etiquetas legibles
#'
#' @param df data.frame.
#' @param diccionario Vector nombrado (nombre técnico -> etiqueta);
#'   por defecto diccionario_columnas().
#' @return data.frame con columnas renombradas (las desconocidas se mantienen;
#'   si dos columnas quedan con la misma etiqueta se conserva el nombre original
#'   entre paréntesis).
etiquetar_columnas <- function(df, diccionario = diccionario_columnas()) {
  nuevos <- ifelse(names(df) %in% names(diccionario), diccionario[names(df)], names(df))
  dup <- duplicated(nuevos) | duplicated(nuevos, fromLast = TRUE)
  nuevos[dup] <- paste0(nuevos[dup], " (", names(df)[dup], ")")
  names(df) <- unname(nuevos)
  df
}
