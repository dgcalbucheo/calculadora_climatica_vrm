# Calculadora climática Valparaíso + RM (prototipo local)
# Ejecutar desde la raíz del proyecto:  shiny::runApp()
# La app solo orquesta: la lógica vive en R/ (serie_clima_celda, tablas_salidas,
# exportar_salidas, catalogo_salidas). Datos: celdas consolidadas de CR2MET v2.5.

library(shiny)
invisible(lapply(list.files("R", "\\.R$", full.names = TRUE), source))

dir_celdas <- getOption("calc.dir_celdas", "data/processed/celdas")
grilla <- readRDS(file.path(dir_celdas, "grilla.rds"))
per_ini <- as.Date(grilla$fecha_ini); per_fin <- as.Date(grilla$fecha_fin)
catalogo <- catalogo_salidas()
hay_mapa <- requireNamespace("leaflet", quietly = TRUE)

# checkboxGroupInput no admite opciones agrupadas: lista plana "Grupo · título"
opts_ids <- stats::setNames(catalogo$id, paste0(catalogo$grupo, " · ", catalogo$titulo))

ui <- fluidPage(
  titlePanel("Calculadora climática · Valparaíso y Región Metropolitana"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("1. Punto"),
      if (hay_mapa) leaflet::leafletOutput("mapa", height = 260),
      if (hay_mapa) helpText("Clic en el mapa para fijar el punto, o escribe las coordenadas."),
      fluidRow(
        column(6, numericInput("lon", "Longitud (WGS84)", value = -71.31, step = 0.01)),
        column(6, numericInput("lat", "Latitud (WGS84)", value = -33.17, step = 0.01))
      ),
      h4("2. Período"),
      dateRangeInput("fechas", NULL, start = max(per_ini, as.Date("2002-01-01")),
                     end = min(per_fin, as.Date("2021-12-31")),
                     min = per_ini, max = per_fin, language = "es", separator = " a "),
      helpText(paste0("Disponible: ", per_ini, " a ", per_fin,
                      ". Los totales y climatologías usan solo años completos.")),
      h4("3. Umbrales"),
      checkboxGroupInput("umb_helada", "Helada (Tmin ≤ °C)", choices = c(0, -1, -2, -3, -4),
                         selected = c(0, -2, -4), inline = TRUE),
      checkboxGroupInput("umb_calor", "Día cálido (Tmax > °C)", choices = c(25, 30, 33, 35, 36),
                         selected = c(30, 35), inline = TRUE),
      h4("4. Salidas"),
      checkboxGroupInput("ids", NULL, choices = opts_ids, selected = catalogo$id),
      actionButton("generar", "Generar", class = "btn-primary")
    ),
    mainPanel(
      width = 8,
      uiOutput("estado"),
      conditionalPanel("output.listo",
        selectInput("prev", "Vista previa", choices = NULL),
        plotOutput("grafico", height = 380),
        downloadButton("descargar", "Descargar zip (PNG + Excel + LEAME)")
      )
    )
  ),
  tags$footer(
    style = "margin-top: 2rem; padding: 1rem 0; border-top: 1px solid #ddd; font-size: 0.85em; color: #666;",
    # Los hijos de una etiqueta se unen sin espacios: cada texto va en un solo string
    p(strong("Versión 1, prototipo. "),
      paste("Cada resultado corresponde a una celda de ~26 km², no a un predio, y no resuelve",
            "el drenaje de aire frío (fondo de valle vs. ladera). No usar para decisiones",
            "agronómicas sin contrastar con datos locales.")),
    p("Datos: CR2MET v2.5 — Boisier, J. P. (2023). ",
      em(paste("CR2MET: A high-resolution precipitation and temperature dataset for the",
               "period 1960-2021 in continental Chile.")),
      paste(" Centro de Ciencia del Clima y la Resiliencia (CR2), Universidad de Chile. DOI "),
      a(href = "https://doi.org/10.5281/zenodo.7529682", target = "_blank", rel = "noopener",
        "10.5281/zenodo.7529682"),
      ". Licencia ",
      a(href = "https://creativecommons.org/licenses/by/4.0/", target = "_blank", rel = "noopener",
        "CC BY 4.0"),
      paste(". Los datos se consolidaron por celda y de ellos se derivan los indicadores",
            "mostrados (días de helada, días cálidos, precipitación)."))
  )
)

server <- function(input, output, session) {
  r <- reactiveValues(res = NULL, punto = NULL, celda = NULL, ids = NULL)

  if (hay_mapa) {
    output$mapa <- leaflet::renderLeaflet({
      leaflet::leaflet() |> leaflet::addTiles() |>
        leaflet::setView(lng = -71, lat = -33.4, zoom = 7)
    })
    observeEvent(input$mapa_click, {
      updateNumericInput(session, "lon", value = round(input$mapa_click$lng, 4))
      updateNumericInput(session, "lat", value = round(input$mapa_click$lat, 4))
      leaflet::leafletProxy("mapa") |> leaflet::clearMarkers() |>
        leaflet::addMarkers(lng = input$mapa_click$lng, lat = input$mapa_click$lat)
    })
  }

  observeEvent(input$generar, {
    r$res <- NULL
    tryCatch({
      if (length(input$ids) == 0) stop("Elige al menos una salida", call. = FALSE)
      if (length(input$umb_helada) == 0 || length(input$umb_calor) == 0)
        stop("Elige al menos un umbral de helada y uno de día cálido", call. = FALSE)
      serie <- serie_clima_celda(input$lon, input$lat, input$fechas[1], input$fechas[2],
                                 dir_celdas,
                                 periodo_disponible = c(per_ini, per_fin))
      res <- tablas_salidas(serie,
                            umbrales_helada = sort(as.numeric(input$umb_helada), decreasing = TRUE),
                            umbrales_calor  = sort(as.numeric(input$umb_calor)))
      r$res <- res; r$celda <- attr(serie, "celda"); r$ids <- input$ids
      r$punto <- list(lon = input$lon, lat = input$lat)
      updateSelectInput(session, "prev", choices = stats::setNames(input$ids,
                          catalogo$titulo[match(input$ids, catalogo$id)]),
                        selected = input$ids[1])
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
  })

  output$listo <- reactive(!is.null(r$res))
  outputOptions(output, "listo", suspendWhenHidden = FALSE)

  output$estado <- renderUI({
    if (is.null(r$res)) return(helpText("Elige punto, período y salidas, y pulsa «Generar»."))
    m <- unname(r$celda)[1:2]
    helpText(sprintf("Celda CR2MET (~5 × 5 km) centrada en %.3f, %.3f. Años completos usados: %s.",
                     m[1], m[2], r$res$meta$periodo_txt))
  })

  output$grafico <- renderPlot({
    req(r$res, input$prev, input$prev %in% r$ids)
    e <- catalogo[catalogo$id == input$prev, ]
    do.call(match.fun(e$grafico), list(r$res))
  }, res = 110)

  output$descargar <- downloadHandler(
    filename = function() sprintf("clima_%.3f_%.3f.zip", r$punto$lon, r$punto$lat),
    content = function(file) {
      req(r$res)
      dir <- file.path(tempdir(), paste0("exp_", as.integer(Sys.time())))
      exportar_salidas(r$res, ids = r$ids, dir = dir, punto = r$punto,
                       formato_tablas = "xlsx")
      zip::zipr(file, files = list.files(dir), root = dir)
    }
  )
}

shinyApp(ui, server)
