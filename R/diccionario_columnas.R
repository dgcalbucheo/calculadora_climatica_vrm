#' Diccionario de nombres de columnas para las tablas exportadas
#'
#' Traduce los nombres técnicos de las tablas de `tablas_salidas()` a etiquetas
#' legibles con unidades (usado por `tablas_a_excel()`). Las columnas que no están
#' aquí conservan su nombre. Versión reducida del diccionario de `paltos_quilpue`,
#' con solo lo que usa la calculadora.
#'
#' @return Vector nombrado: nombre técnico -> etiqueta.
diccionario_columnas <- function() {
  c(
    umbral = "Umbral (°C)", anio = "Año", mes = "Mes",
    n_anios = "N años", n_dias = "N° días", anio_completo = "Año completo",
    helada = "Días de helada", supera = "Días cálidos",
    media = "Media", mediana = "Mediana", min = "Mínimo", max = "Máximo",
    sd = "Desv. estándar", p25 = "Percentil 25", p75 = "Percentil 75",
    prop_anios = "Fracción de años con ocurrencia",
    primera = "Primera helada", ultima = "Última helada",
    doy_primera = "Día juliano primera", doy_ultima = "Día juliano última",
    tmin_media = "Tmin media (°C)", tmin_p10 = "Tmin percentil 10 (°C)",
    tmed_media = "Tmed media (°C)", tmax_media = "Tmax media (°C)",
    tmax_p90 = "Tmax percentil 90 (°C)",
    pr_mm = "Precipitación (mm)", media_periodo = "Media del período (mm)",
    anomalia_pct = "Anomalía (%)", clase = "Clase (terciles)",
    dias_lluvia = "Días con lluvia"
  )
}
