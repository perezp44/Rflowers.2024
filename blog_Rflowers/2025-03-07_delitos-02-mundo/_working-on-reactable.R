# Cargar las librerías necesarias
library(reactable)
library(reactablefmtr)
library(htmltools)
library(viridis)

# Crear una tabla mejorada con múltiples características
reactable(
  data = iris,
  defaultPageSize = 36,
  filterable = TRUE,
  searchable = TRUE,
  striped = TRUE,
  highlight = TRUE,
  bordered = TRUE,
  
  # Personalizar el estilo de las columnas
  columns = list(
    Sepal.Length = colDef(
      name = "Longitud del Sépalo",
      format = colFormat(digits = 1),
      style = function(value) {
        color <- viridis_pal(option = "D")(10)[floor(value) - 4]
        list(background = color, color = "white")
      }
    ),
    Sepal.Width = colDef(
      name = "Ancho del Sépalo",
      format = colFormat(digits = 1),
      cell = data_bars(iris, 
                       fill_color = "#3fc1c9",
                       background = "#f5f5f5",
                       border_style = "solid",
                       text_position = "inside-end")
    ),
    Petal.Length = colDef(
      name = "Longitud del Pétalo",
      format = colFormat(digits = 1),
      cell = data_bars(iris, 
                       fill_color = "#fc5185",
                       background = "#f5f5f5",
                       text_position = "inside-end")
    ),
    Petal.Width = colDef(
      name = "Ancho del Pétalo",
      format = colFormat(digits = 1),
      cell = data_bars(iris, 
                       fill_color = "#5b8c85",
                       background = "#f5f5f5",
                       text_position = "inside-end")
    ),
    Species = colDef(
      name = "Especies",
      style = function(value) {
        color <- switch(value,
                        "setosa" = "#1a535c",
                        "versicolor" = "#4ecdc4",
                        "virginica" = "#ff6b6b")
        list(color = "white", background = color, fontWeight = "bold")
      }
    )
  ),
  
  # Estilos adicionales
  theme = reactablefmtr::fivethirtyeight(
    cell_padding = 4, 
    font_size = 13, 
    header_font_size = 14
  ),
  
  # Añadir resumen
  defaultColDef = colDef(
    footer = function(values) {
      if (is.numeric(values)) {
        sprintf("%.1f", mean(values))
      }
    }
  ),
  
  # Personalizar el título y pie de tabla
  defaultSorted = "Species",
  
  # Agregar opciones de paginación
  showPageSizeOptions = TRUE,
  pageSizeOptions = c(10, 25, 36, 50, 100),
  
  # Agregar herramientas para descargar datos
  elementId = "iris_tabla"
)

# Agregar título HTML y descripción
htmltools::tagList(
  htmltools::tags$h2("Conjunto de Datos Iris", 
                     style = "text-align: center; color: #333; font-family: 'Segoe UI', Arial, sans-serif;"),
  htmltools::tags$p("Características de longitud y ancho de sépalos y pétalos para tres especies de iris.", 
                    style = "text-align: center; font-style: italic; margin-bottom: 20px;"),
  reactable::getReactableState("iris_tabla")
)
tabla_iris
