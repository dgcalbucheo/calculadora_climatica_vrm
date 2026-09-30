#' Catálogo de salidas que se pueden pedir (versión 1: solo CR2MET)
#'
#' Una fila por salida. La interfaz lee este catálogo para armar sus opciones, y
#' `exportar_salidas()` lo usa para generar los archivos. Para sumar una salida:
#' escribir su `grafico_*()` en R/, agregar su tabla a `tablas_salidas()` y una
#' fila aquí.
#'
#' @return data.frame con: id, grupo, titulo, grafico (nombre de la función),
#'   tablas (nombres en `tablas_salidas()`, separados por coma), parametros
#'   (argumentos configurables de la función, separados por coma), ancho, alto.
catalogo_salidas <- function() {
  data.frame(
    id = c("heladas_anual", "heladas_mensual", "temporada_heladas",
           "temperatura_mensual", "dias_calidos_anual",
           "precipitacion_mensual", "precipitacion_anual"),
    grupo = c("Heladas", "Heladas", "Heladas", "Temperatura", "Temperatura",
              "Precipitación", "Precipitación"),
    titulo = c("Días de helada por año y umbral",
               "Frecuencia mensual de heladas por umbral",
               "Temporada de heladas (primera a última)",
               "Temperatura media mensual (Tmin, Tmed, Tmax)",
               "Días cálidos por año y umbral",
               "Precipitación mensual",
               "Precipitación anual"),
    grafico = c("grafico_heladas_anual", "grafico_heladas_mensual",
                "grafico_temporada_heladas", "grafico_temperatura_mensual",
                "grafico_dias_calidos_anual", "grafico_precipitacion_mensual",
                "grafico_precipitacion_anual"),
    tablas = c("heladas_anual,heladas_resumen", "heladas_mensual",
               "temporada_heladas", "temperatura_mensual",
               "calor_anual,calor_resumen", "precip_mensual", "precip_anual"),
    parametros = c("umbrales", "umbrales,meses", "umbral,meses", "",
                   "umbrales", "", ""),
    ancho = c(9, 9, 9, 9, 9, 9, 9),
    alto  = c(4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5),
    stringsAsFactors = FALSE
  )
}
