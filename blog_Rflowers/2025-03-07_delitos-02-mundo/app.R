
# Cargar librerías necesarias
library(shiny)
library(leaflet)
library(dplyr)
library(htmltools)

# Definir la función para el mapa leaflet (adaptada para Shiny)
p_coro_leaflet_II <- function(df, fill_var, 
                              iso_col = "iso3_code", 
                              country_col = "country",
                              obs_label = "Nº de observaciones",
                              palette = "viridis",
                              reverse_palette = TRUE,
                              legend_title = "Nº de observaciones",
                              view_lng = 0,
                              view_lat = 30,
                              view_zoom = 2) {
  
  # Si fill_var ya es un string, usarlo directamente
  # Si no lo es, convertirlo a string
  if(is.character(fill_var)) {
    fill_var_name <- fill_var
  } else {
    fill_var_name <- as.character(substitute(fill_var))
    if (fill_var_name %in% names(df) == FALSE) {
      fill_var_name <- deparse(substitute(fill_var))
    }
  }
  
  # Crear la paleta de colores
  fill_values <- df[[fill_var_name]]
  pal <- colorNumeric(palette = palette, domain = fill_values, reverse = reverse_palette)
  
  # Crear las etiquetas para los popups con el texto personalizable
  etiquetas <- sprintf(
    "<strong>%s</strong><br/>País: %s<br/>%s: %g",
    df[[iso_col]], 
    df[[country_col]],
    obs_label,
    round(df[[fill_var_name]], 2)) %>% 
    lapply(htmltools::HTML)
  
  # Crear el mapa leaflet
  p <- leaflet(df) %>%
    addTiles() %>%  # Añadir mapa base
    addPolygons(
      fillColor = ~pal(get(fill_var_name)),  # Usando get() que funciona bien con fórmulas de leaflet
      weight = 0.9,
      opacity = 1,
      color = "white",
      dashArray = "2",
      fillOpacity = 1,
      highlightOptions = highlightOptions(
        weight = 1.4,
        color = "#999",
        dashArray = "",
        fillOpacity = 1,
        bringToFront = TRUE),
      label = etiquetas,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 7px"),
        textsize = "14px",
        direction = "auto")
    ) %>%
    addLegend(
      pal = pal, 
      values = ~get(fill_var_name),
      opacity = 0.7, 
      title = legend_title,
      position = "bottomright"
    ) %>% 
    setView(lng = view_lng, lat = view_lat, zoom = view_zoom) %>%
    setMaxBounds(lng1 = -100, lat1 = -85, lng2 = 180, lat2 = 85)
  
  return(p)
}
p
# Crear datos de ejemplo
crear_datos_ejemplo <- function() {
  # Lista de 10 países con sus códigos ISO3
  paises <- data.frame(
    iso3_code = c("USA", "CAN", "MEX", "BRA", "ARG", "ESP", "DEU", "GBR", "FRA", "ITA"),
    country = c("Estados Unidos", "Canadá", "México", "Brasil", "Argentina", 
                "España", "Alemania", "Reino Unido", "Francia", "Italia"),
    n = round(runif(10, 10, 100))
  )
  return(paises)
}

# UI de la aplicación
ui <- fluidPage(
  titlePanel("Mapa Leaflet con Función Personalizada"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("paleta", "Paleta de colores:", 
                  choices = c("viridis", "magma", "plasma", "inferno", "Blues", "Reds", "Greens"),
                  selected = "viridis"),
      
      checkboxInput("invertir", "Invertir paleta", value = TRUE),
      
      textInput("titulo_leyenda", "Título de la leyenda:", 
                value = "Nº de observaciones"),
      
      textInput("etiqueta_obs", "Etiqueta para observaciones:", 
                value = "Nº de observaciones"),
      
      numericInput("long_vista", "Longitud central:", value = 0),
      
      numericInput("lat_vista", "Latitud central:", value = 30),
      
      sliderInput("zoom_vista", "Nivel de zoom:", 
                  min = 1, max = 6, value = 2, step = 1),
      
      actionButton("generar", "Generar nuevo mapa")
    ),
    
    mainPanel(
      leafletOutput("mapa", height = "600px")
    )
  )
)

# Server de la aplicación
server <- function(input, output, session) {
  
  # Datos reactivos
  datos <- reactiveVal(crear_datos_ejemplo())
  
  # Regenerar datos cuando se pulsa el botón
  observeEvent(input$generar, {
    datos(crear_datos_ejemplo())
  })
  
  # Renderizar el mapa
  output$mapa <- renderLeaflet({
    p_coro_leaflet_II(
      df = datos(),
      fill_var = "n",
      palette = input$paleta,
      reverse_palette = input$invertir,
      legend_title = input$titulo_leyenda,
      obs_label = input$etiqueta_obs,
      view_lng = input$long_vista,
      view_lat = input$lat_vista,
      view_zoom = input$zoom_vista
    )
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)