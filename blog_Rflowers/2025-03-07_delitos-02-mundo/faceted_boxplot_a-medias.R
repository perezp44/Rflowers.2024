crear_boxplot_interactivo <- function(datos, x_var, y_var, region_var) {
  # Verificar que las librerías necesarias estén instaladas
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Por favor instala el paquete 'ggplot2': install.packages('ggplot2')")
  }
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Por favor instala el paquete 'plotly': install.packages('plotly')")
  }
  
  # Cargar las librerías
  library(ggplot2)
  library(plotly)
  
  # Crear el gráfico base con ggplot2
  p <- ggplot(datos, aes_string(x = x_var, y = y_var)) +
    # Boxplot sin outliers
    geom_boxplot(outlier.shape = NA) +
    # Añadir puntos para cada observación
    geom_point(aes(text = paste("Valor Y:", round(get(y_var), 2))), 
               position = position_jitter(width = 0.2, seed = 123), 
               alpha = 0.6) +
    # Facet por región
    facet_wrap(as.formula(paste("~", region_var)), scales = "free_x") +
    # Mejoras estéticas
    theme_minimal() +
    labs(title = paste("Boxplot facetado por", region_var),
         x = x_var,
         y = y_var) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Convertir a plotly para la interactividad
  plotly_graph <- ggplotly(p, tooltip = "text")
  
  return(plotly_graph)
}

# Ejemplo de uso:
# Asumiendo que tienes un dataframe llamado 'mi_df' con las columnas 'grupo', 'valor' y 'pais'
crear_boxplot_interactivo(zzz, "region.x", "n", "region")
