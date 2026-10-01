# Cargar librerías
library(reactable)
library(dplyr)
library(htmltools)  # Añadimos esta librería

# Crear un conjunto de datos de ejemplo
datos <- data.frame(
  Nombre = c("Ana", "Carlos", "María", "Juan", "Laura"),
  Edad = c(28, 35, 42, 25, 31),
  Salario = c(45000, 62000, 55000, 38000, 52000),
  Departamento = c("Ventas", "TI", "Recursos Humanos", "Marketing", "Finanzas"),
  stringsAsFactors = FALSE
)

# Crear tabla reactable
reactable(datos, 
          # Permitir ordenamiento
          sortable = TRUE,
          
          # Bloquear la primera fila
          rowStyle = function(index) {
            if (index == 4) {  # Índice de Juan en el dataframe original
              list(fontWeight = "bold", backgroundColor = "#f0f0f0")
            }
          },
          
          # Personalizar estilos
          theme = reactableTheme(
            color = "hsl(233, 9%, 87%)",
            backgroundColor = "hsl(233, 9%, 19%)",
            borderColor = "hsl(233, 9%, 22%)",
            stripedColor = "hsl(233, 12%, 22%)",
            highlightColor = "hsl(233, 12%, 24%)"
          ),
          
          # Configuraciones adicionales
          defaultPageSize = 5,
          striped = TRUE,
          highlight = TRUE
)
