# Control de regresión: Colliguay
# Ejecutar desde la raíz del proyecto:  source("tests/control_colliguay.R")
# Compara las métricas de la calculadora con los valores validados contra
# paltos_quilpue (CR2MET, 2002-2021, celda -71.325 / -33.175).
# Falla (stop) si algún valor se desvía más de la tolerancia.

invisible(lapply(list.files("R", "\\.R$", full.names = TRUE), source))

dir_celdas <- "data/processed/celdas"   # consolidación completa
lon <- -71.325; lat <- -33.175          # centro de la celda de Colliguay (misma celda que paltos_quilpue)

d <- serie_clima_celda(lon, lat, "2002-01-01", "2021-12-31", dir_celdas)
m <- metricas_punto(d)

h  <- resumen_por_umbral(m$helada, "helada")
esp <- c(
  helada_0    = 2.15,
  helada_m1   = 0.35,
  pr_anual_mm = 364.0,
  dias_lluvia = 32.8
)
obs <- c(
  helada_0    = h$media[h$umbral == 0],
  helada_m1   = h$media[h$umbral == -1],
  pr_anual_mm = mean(m$precip$pr_mm),
  dias_lluvia = mean(m$precip$dias_lluvia)
)
tol <- c(helada_0 = 0.01, helada_m1 = 0.01, pr_anual_mm = 0.1, dias_lluvia = 0.05)

res <- data.frame(esperado = esp, observado = round(obs, 3),
                  dif = round(obs - esp, 3), tol = tol,
                  ok = abs(obs - esp) <= tol)
print(res)
celda <- unname(attr(d, "celda"))[1:2]   # (lon, lat) del centro de la celda
cat("Celda usada:", celda, "(esperada: -71.325 -33.175)\n")
stopifnot(isTRUE(all.equal(celda, c(-71.325, -33.175))))
if (!all(res$ok)) stop("CONTROL COLLIGUAY: hay valores fuera de tolerancia") else
  cat("CONTROL COLLIGUAY: OK\n")
