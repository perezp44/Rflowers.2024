library(tidyverse)
library(sf)
library(rlang)


#- 1. COROPLETAS ------------------------------------------------------------------

##- f coropleta ggplot2 --------------------------------------------------------

# p <- ggplot(zz_m, aes(geometry = geometry, group = pais)) + 
#   geom_sf(aes(fill = n)) +
#   scale_fill_viridis_c(na.value = "lightgrey", name = "Nº obs.", direction = -1) +
#   theme_void() +
#   labs(title = "Nº de observaciones totales(1990-2023)",
#        subtitle = "(para los 27 tipos de delitos)")

p_coropleta_gg <- function(df, fill, group, 
                              name_leyenda =  "Nº obs.",
                              title = "Nº de observaciones totales(1990-2023)",
                              subtitle = "(para los 27 tipos de delitos)") {
  # Convertir los argumentos a expresiones simbólicas
  fill_var <- enquo(fill)
  group_var <- enquo(group)
  # Crear el gráfico usando tidyeval
  p <- ggplot(df, aes(geometry = geometry, group = !!group_var)) +
    geom_sf(aes(fill = !!fill_var)) +
    scale_fill_viridis_c(na.value = "lightgrey", name = name_leyenda, direction = 1) +
    theme_void() +
    labs(title = title,
         subtitle = subtitle)
  return(p)
}

# Ejemplo de uso:
#p <- p_coropleta_gg(zz_m, n, pais)

###- version quoted -------
#- ahora la misma función PERO quoted
#- Para modificar la función p_coropleta_gg 
#-para que acepte los nombres de las variables como strings (quoted), 
#- necesitamos cambiar cómo se manejan estos argumentos dentro de la función. 
#- En lugar de usar enquo(), utilizaremos sym() para convertir las cadenas de texto a símbolos.
#- La clave del cambio está en usar sym() en lugar de enquo(). 
#- Mientras que enquo() captura la expresión sin evaluar 
#- (lo que requiere nombres de variables sin comillas), 
#- sym() convierte un string en un símbolo, 
#- permitiendo pasar los nombres de las variables como strings con comillas.
p_coropleta_gg_q <- function(df, fill, group, 
                           name_leyenda = "Nº obs.",
                           title = "Nº de observaciones totales(1990-2023)",
                           subtitle = "(para los 27 tipos de delitos)") {
  # Convertir los argumentos de texto a símbolos
  fill_var <- sym(fill)
  group_var <- sym(group)
  
  # Crear el gráfico usando tidyeval
  p <- ggplot(df, aes(geometry = geometry, group = !!group_var)) +
    geom_sf(aes(fill = !!fill_var)) +
    scale_fill_viridis_c(na.value = "lightgrey", name = name_leyenda, direction = 1) +
    theme_void() +
    labs(title = title,
         subtitle = subtitle)
  
  return(p)
}


###- con R-base -------------
p_coropleta_gg_II <- function(df, fill, group, 
                                 name_leyenda = "Nº obs.",
                                 title = "Nº de observaciones totales(1990-2026)",
                                 subtitle = "(para los 27 tipos de delitos)") {
  
  # Crear la fórmula para aes() usando substitute() y paste() de base R
  mapping <- eval(parse(text = paste0("aes(geometry = geometry, group = ", group, ", fill = ", fill, ")")))
  
  # Crear el gráfico usando la fórmula generada
  p <- ggplot(df) +
    geom_sf(mapping = mapping) +
    scale_fill_viridis_c(na.value = "lightgrey", name = name_leyenda, direction = 1) +
    theme_void() +
    labs(title = title,
         subtitle = subtitle)
  
  return(p)
}

#p_coropleta_gg_II(zz_m, fill = "n", group = "pais")

###- R-base mejor versión ----------------

p_coropleta_gg_IIb <- function(df, fill, group, 
                                name_leyenda = "Nº obs.",
                                title = "Nº de observaciones totales(1990-2023)",
                                subtitle = "(para los 27 tipos de delitos)") {
  
  # Construir una expresión para aes() usando funciones de R base
  fill_expr <- as.name(fill)    # Convertir string a nombre de variable
  group_expr <- as.name(group)  # Convertir string a nombre de variable
  
  # Crear el gráfico usando las expresiones construidas
  p <- ggplot(df, aes(geometry = geometry, 
                      group = eval(group_expr, df), 
                      fill = eval(fill_expr, df))) +
    geom_sf() +
    scale_fill_viridis_c(na.value = "lightgrey", name = name_leyenda, direction = 1) +
    theme_void() +
    labs(title = title,
         subtitle = subtitle)
  
  return(p)
}
#p_coropleta_gg_IIb(zz_m, fill = "n", group = "pais")


##- f coropleta leaflet --------------------------------------------------------

# # Crear una paleta de colores basada en la vv. "n"
# bins <- base::pretty(min(zz_m$n, na.rm = TRUE):max(zz_m$n, na.rm = TRUE))
# pal <- colorBin("YlOrRd", domain = zz_m$n, bins = bins)
# 
# # Crear las etiquetas para los popups
# etiquetas <- sprintf(
#   "<strong>%s</strong><br/>País: %s<br/>Nº de observaciones: %g",
#   zz_m$iso3_code, zz_m$country, 
#   round(zz_m$n, 2)) %>% lapply(htmltools::HTML)
# 
# p <- leaflet(zz_m) %>%
#   addTiles() %>%  # Añadir mapa base
#   addPolygons(
#     fillColor = ~pal(n),
#     weight = 0.9,
#     opacity = 1,
#     color = "white",
#     dashArray = "2",
#     fillOpacity = 0.9,
#     highlightOptions = highlightOptions(
#       weight = 1.4,
#       color = "#999",
#       dashArray = "",
#       fillOpacity = 0.6,
#       bringToFront = TRUE),
#     label = etiquetas,
#     labelOptions = labelOptions(
#       style = list("font-weight" = "normal", padding = "3px 7px"),
#       textsize = "14px",
#       direction = "auto")
#   ) %>%
#   addLegend(
#     pal = pal, 
#     values = ~n, 
#     opacity = 0.7, 
#     title = "Nº de observaciones totales",
#     position = "bottomright"
#   ) %>% 
#   setView(lng = 0, lat = 30, zoom = 2) %>%
#   setMaxBounds(lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85)
# 
# p




#- usando tidyeval
library(leaflet)
library(htmltools)

p_coro_leaflet_tidy <- function(df, fill, 
                                iso_col = "iso3_code", 
                                country_col = "country",
                                palette = "viridis",
                                reverse_palette = TRUE,
                                legend_title = "Nº de observaciones totales",
                                view_lng = 0,
                                view_lat = 30,
                                view_zoom = 2) {

    # Convertir el argumento fill a string para poder usarlo después
  fill_name <- quo_name(enquo(fill))
  iso_name <- quo_name(ensym(iso_col))
  country_name <- quo_name(ensym(country_col))
  
  # Crear la paleta de colores
  fill_values <- df[[fill_name]]
  pal <- colorNumeric(palette = palette, domain = fill_values, reverse = reverse_palette)
  
  # Crear las etiquetas para los popups
  etiquetas <- sprintf(
    "<strong>%s</strong><br/>País: %s<br/>Nº de observaciones: %g",
    df[[iso_name]], 
    df[[country_name]],
    round(df[[fill_name]], 2)) %>% 
    lapply(htmltools::HTML)
  
  # Crear el mapa leaflet
  p <- leaflet(df) %>%
    addTiles() %>%  # Añadir mapa base
    addPolygons(
      fillColor = ~pal(df[[fill_name]]),
      weight = 0.9,
      opacity = 1,
      color = "white",
      dashArray = "2",
      fillOpacity = 0.9,
      highlightOptions = highlightOptions(
        weight = 1.4,
        color = "#999",
        dashArray = "",
        fillOpacity = 0.6,
        bringToFront = TRUE),
      label = etiquetas,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 7px"),
        textsize = "14px",
        direction = "auto")
    ) %>%
    addLegend(
      pal = pal, 
      values = df[[fill_name]], 
      opacity = 0.7, 
      title = legend_title,
      position = "bottomright"
    ) %>% 
    setView(lng = view_lng, lat = view_lat, zoom = view_zoom) %>%
    setMaxBounds(lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85)
  
  return(p)
}

# Ejemplo de uso:
# p_coro_leaflet_tidy(zz_m, n)

###- version quoted ----------

p_coro_leaflet_tidy_q <- function(df, fill, 
                                  iso_col = "iso3_code", 
                                  country_col = "country",
                                  obs_label = "Nº de observaciones",
                                  palette = "viridis",
                                  reverse_palette = TRUE,
                                  legend_title = "Nº de observaciones totales",
                                  view_lng = 0,
                                  view_lat = 30,
                                  view_zoom = 2) {
  
  # Usar directamente el nombre ya que viene como string
  fill_name <- fill
  iso_name <- iso_col
  country_name <- country_col
  
  # Crear la paleta de colores
  fill_values <- df[[fill_name]]
  pal <- colorNumeric(palette = palette, domain = fill_values, reverse = reverse_palette)
  
  # Crear las etiquetas para los popups usando el nuevo parámetro obs_label
  etiquetas <- sprintf(
    "<strong>%s</strong><br/>País: %s<br/>%s: %g",
    df[[iso_name]], 
    df[[country_name]],
    obs_label,
    round(df[[fill_name]], 2)) %>% 
    lapply(htmltools::HTML)
  
  # Crear el mapa leaflet
  p <- leaflet(df) %>%
    addTiles() %>%  # Añadir mapa base
    addPolygons(
      fillColor = ~pal(df[[fill_name]]),
      weight = 0.9,
      opacity = 1,
      color = "white",
      dashArray = "2",
      fillOpacity = 0.9,
      highlightOptions = highlightOptions(
        weight = 1.4,
        color = "#999",
        dashArray = "",
        fillOpacity = 0.6,
        bringToFront = TRUE),
      label = etiquetas,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 7px"),
        textsize = "14px",
        direction = "auto")
    ) %>%
    addLegend(
      pal = pal, 
      values = df[[fill_name]], 
      opacity = 0.7, 
      title = legend_title,
      position = "bottomright"
    ) %>% 
    setView(lng = view_lng, lat = view_lat, zoom = view_zoom) %>%
    setMaxBounds(lng1 = -180, lat1 = -85, lng2 = 180, lat2 = 85)
  
  return(p)
}

##- f coropleta leaflet --------------------------------------------------------
#- SIN usar tidyeval


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
      fillOpacity = 1, #- aqui
      highlightOptions = highlightOptions(
        weight = 1.4,
        color = "#999",
        dashArray = "",
        fillOpacity = 1,  #- aqui
        bringToFront = TRUE),
      label = etiquetas,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 7px"),
        textsize = "14px",
        direction = "auto")
    ) %>%
    addLegend(
      pal = pal, 
      values = ~get(fill_var_name),  # También usando get() aquí para consistencia
      opacity = 0.7, 
      title = legend_title,
      position = "bottomright"
    ) %>% 
    setView(lng = view_lng, lat = view_lat, zoom = view_zoom) %>%
    setMaxBounds(lng1 = -100, lat1 = -85, lng2 = 180, lat2 = 85)
  
  return(p)
}

# Ejemplo de uso con el nombre de la variable como símbolo:
# p_coro_leaflet_II(zz_m, n)

# Ejemplo de uso con el nombre de la variable como string:
# p_coro_leaflet_II(zz_m, "n")


##- f coropleta echarts4r ------------------------------------------------------


# zz_mm %>% 
#   e_charts(pais.iso) |> 
#   e_map(n) |> 
#   e_visual_map(n) 

# p <- zz_mm %>% 
#   e_charts(pais.iso) |> 
#   e_map(n) |> 
#   e_visual_map(n, min_ = min(zz_mm$n, na.rm = TRUE),
#                max_ = max(zz_mm$n, na.rm = TRUE),
#                inRange = list(color = c("#e0f3db", "#a8ddb5", "#43a2ca")),
#                text = c("Más obvs.", "Menos"),  calculable = TRUE) %>% 
#   e_title("Nº de observaciones por país") %>%
#   e_tooltip(trigger = "item", formatter = htmlwidgets::JS("function(params){ return params.name + ': ' + params.value;}"))

# p |> e_theme("vintage")
# 
# p  %>%  e_datazoom() |> 
#   e_zoom(
#     dataZoomIndex = 0,
#     start = 20,
#     end = 40,
#     btn = "BUTTON"
#   ) |> 
#   e_button(
#     id = "BUTTON", 
#     htmltools::tags$i(class = "fa fa-search"), # passed to the button
#     class = "btn btn-default",
#     "Zoom in"
#   )


library(echarts4r)
library(dplyr)

create_echarts_map <- function(data, 
                               iso_col = "pais.iso", 
                               value_col = "n",
                               title = "Nº de observaciones por país",
                               colors = c("#e0f3db", "#a8ddb5", "#43a2ca"),
                               text_labels = c("Más obvs.", "Menos")) {
  
  # Obtener valores mínimo y máximo para la escala
  min_value <- min(data[[value_col]], na.rm = TRUE)
  max_value <- max(data[[value_col]], na.rm = TRUE)
  
  # Crear una copia del dataframe con nombres de columnas estandarizados
  # para evitar problemas con la notación de columnas en echarts4r
  temp_data <- data
  names(temp_data)[names(temp_data) == iso_col] <- "iso_code"
  names(temp_data)[names(temp_data) == value_col] <- "value"
  
  # Crear el mapa echarts usando los nombres estandarizados
  p <- temp_data %>% 
    e_charts(iso_code) %>% 
    e_map(value) %>% 
    e_visual_map(value, 
                 min_ = min_value,
                 max_ = max_value,
                 inRange = list(color = colors),
                 text = text_labels,  
                 calculable = TRUE) %>% 
    e_title(title) %>%
    e_tooltip(trigger = "item", 
              formatter = htmlwidgets::JS("function(params){ return params.name + ': ' + params.value;}"))
  
  return(p)
}

# Ejemplo de uso:
# p <- create_echarts_map(zz_mm)
# p

# create_echarts_map(
#   zz_mm,
#   iso_col = "pais.iso",
#   value_col = "n",
#   title = "Delitos registrados por país",
#   colors = c("#ffffcc", "#a1dab4", "#41b6c4", "#225ea8")
# )


#p |> e_theme("vintage")



##- f coropleta highcharter ----------------------------------------------------


# p <- hcmap(
#   "custom/world-robinson-lowres",
#   data = zz_mm,
#   name = "Nº de observaciones",
#   value = "n",
#   borderWidth = 0,
#   nullColor = "#d3d3d3",
#   joinBy = c("iso-a3", "iso3_code")) |>
#   hc_colorAxis(
#     stops = color_stops(colors = viridisLite::inferno(10, begin = 0.1, direction = -1)),
#     type = "logarithmic")


library(highcharter)
library(viridisLite)

create_highcharter_map <- function(data, 
                                   value_col = "n",
                                   iso_col = "iso3_code",
                                   map_name = "custom/world-robinson-lowres",
                                   title = "Nº de observaciones",
                                   color_palette = viridisLite::inferno(10, begin = 0.1, direction = -1),
                                   use_log_scale = TRUE,
                                   null_color = "#d3d3d3",
                                   border_width = 0) {
  
  # Verificar que las columnas existen en los datos
  if (!value_col %in% names(data)) {
    stop(paste("La columna", value_col, "no existe en los datos"))
  }
  
  if (!iso_col %in% names(data)) {
    stop(paste("La columna", iso_col, "no existe en los datos"))
  }
  
  # Crear el mapa con highcharter
  p <- hcmap(
    map_name,
    data = data,
    name = title,
    value = value_col,
    borderWidth = border_width,
    nullColor = null_color,
    joinBy = c("iso-a3", iso_col)
  )
  
  # Aplicar la escala de colores
  if (use_log_scale) {
    p <- p |>
      hc_colorAxis(
        stops = color_stops(colors = color_palette),
        type = "logarithmic"
      )
  } else {
    p <- p |>
      hc_colorAxis(
        stops = color_stops(colors = color_palette),
        type = "linear"
      )
  }
  
  # Añadir título si se especifica
  if (!is.null(title) && title != "") {
    p <- p |> hc_title(text = title)
  }
  
  return(p)
}

# Ejemplo de uso básico:
# create_highcharter_map(zz_mm)

# create_highcharter_map(
#   zz_mm,
#   value_col = "n",
#   title = "Distribución global de delitos",
#   color_palette = viridisLite::plasma(10),
#   use_log_scale = FALSE
# )


##- f coropleta tmap - versión original con tooltip/popup corregido ----------
p_coro_tmap <- function(df, fill_var,
                        iso_col = "iso3_code",
                        country_col = "country",
                        obs_label = "Nº de observaciones",
                        palette = "viridis",
                        reverse_palette = TRUE,
                        legend_title = "Nº de observaciones",
                        view_lng = 0,
                        view_lat = 30,
                        view_zoom = 2,
                        basemap = "OpenStreetMap") {
  
  # Cargar las librerías requeridas (opcional si ya están cargadas)
  # require(tmap)
  
  # --- Manejo del nombre de la variable de relleno ---
  # (Mantenemos tu lógica original para determinar fill_var_name)
  if(is.character(fill_var)) {
    fill_var_name <- fill_var
  } else {
    # Usamos deparse(substitute()) que es más estándar para capturar el nombre
    fill_var_name <- deparse(substitute(fill_var))
    # # Tu comprobación original (puede ser útil si el nombre es complejo)
    # if (fill_var_name %in% names(df) == FALSE) {
    #   fill_var_name <- deparse(substitute(fill_var)) # Asegura que sea string
    # }
  }
  
  # Verificar que las columnas necesarias existen
  # (Buena práctica añadir esta verificación)
  required_cols <- c(iso_col, country_col, fill_var_name)
  if (!all(required_cols %in% names(df))) {
    stop(paste("Una o más columnas requeridas no se encuentran en df:",
               paste(required_cols[!required_cols %in% names(df)], collapse=", ")))
  }
  # Asegurar que fill_var_name es una columna numérica si df no es NULL
  if (!is.null(df) && !is.numeric(df[[fill_var_name]])) {
    warning(paste("La columna '", fill_var_name, "' no es numérica. La escala de color puede no funcionar como se espera."))
  }
  
  
  # Configurar tmap en modo "view"
  tmap_mode("view")
  
  # Configurar la paleta
  palette_values <- if(reverse_palette) paste0("-", palette) else palette
  
  # --- ELIMINADO ---
  # Ya no creamos la columna 'tooltip_text' manualmente.
  # df$tooltip_text <- paste0(...)
  
  # Crear el mapa con la sintaxis original donde sea posible
  tm <- tm_shape(df) +
    tm_polygons(
      # Mapeo del color de relleno (usando 'fill' como en tu original)
      fill = fill_var_name,
      
      # Configuración de la escala y leyenda (usando tu estructura original)
      fill.scale = tm_scale_continuous(
        values = palette_values,
        value.na = "lightgray", # Color para NA
        n = 7 # Número de cortes en la leyenda (ajusta según necesites)
      ),
      fill.legend = tm_legend(
        title = legend_title # Título de la leyenda
      ),
      
      # Otros parámetros estéticos de tu código original
      fill_alpha = 1,
      col = "white", # Color del borde del polígono
      col_alpha = 0.9, # Transparencia del borde
      lwd = 0.9, # Grosor del borde
      
      # ID del polígono (se usa para identificarlo, a menudo aparece en hover)
      id = iso_col, # Mantenemos el id como iso_col según tu original
      
      # --- CORRECCIÓN PRINCIPAL PARA EL POPUP ---
      # Especificamos las columnas a mostrar en el popup al hacer clic.
      # Usamos un vector nombrado para etiquetas claras.
      popup.vars = setNames(c(iso_col, country_col, fill_var_name),
                            c("ISO Code", "País", obs_label)),
      
      # Formato para el popup (ya no necesitamos html.escape)
      # Puedes añadir formato para números si quieres, ej: list(digits = 2)
      popup.format = list() # Lista vacía para formato por defecto, o añade opciones
    ) +
    tm_view(
      # Usamos tu configuración original para la vista
      set_view = c(lng = view_lng, lat = view_lat, zoom = view_zoom),
      basemap.server = basemap # Usamos basemap.server como en tu original
    )
  
  # Lista de mapas base (la dejamos como referencia)
  available_basemaps <- c(
    "OpenStreetMap", "OpenStreetMap.DE", "OpenStreetMap.HOT",
    "OpenTopoMap", "Stamen.Toner", "Stamen.TonerLite",
    "Stamen.Terrain", "Stamen.Watercolor", "Esri.WorldStreetMap",
    "Esri.WorldTopoMap", "Esri.WorldImagery", "Esri.WorldTerrain",
    "Esri.WorldShadedRelief", "Esri.OceanBasemap", "CartoDB.Positron",
    "CartoDB.DarkMatter", "CartoDB.Voyager"
  )
  
  # Descomenta si quieres ver la lista cada vez que llamas a la función
  # message("Mapas base disponibles: ", paste(available_basemaps, collapse=", "))
  
  return(tm)
}



#- 2. BOXPLOTS --------------------------------------------------------------------


#- primera intentona ---------
#- quiero hacer un boxplot x continente. Tb poner los codigos ISO, solo q se superponen, 
#- asi que he de hacer pirulas para que no se superpongan

# Función para agrupar países con valores similares dentro de cada región
prepare_labels <- function(data, y_var, label_var, group_var, tolerance = 0.1) {
  result <- data.frame()
  
  # Para cada región, procesar por separado
  for (reg in unique(data[[group_var]])) {
    region_data <- data[data[[group_var]] == reg, ]
    region_data <- region_data[order(region_data[[y_var]]), ]
    
    # Inicializar variables para este grupo
    groups <- list()
    current_group <- 1
    groups[[current_group]] <- region_data[1, ]
    
    # Agrupar países con valores Y similares dentro de esta región
    if (nrow(region_data) > 1) {
      for (i in 2:nrow(region_data)) {
        last_y <- tail(groups[[current_group]], 1)[[y_var]]
        current_y <- region_data[i, ][[y_var]]
        
        # Si el valor Y es similar al último del grupo actual, añade al mismo grupo
        if (abs(current_y - last_y) <= tolerance * diff(range(data[[y_var]]))) {
          groups[[current_group]] <- rbind(groups[[current_group]], region_data[i, ])
        } else {
          # Si no, crea un nuevo grupo
          current_group <- current_group + 1
          groups[[current_group]] <- region_data[i, ]
        }
      }
    }
    
    # Crear etiquetas concatenadas para cada grupo
    for (i in 1:length(groups)) {
      g <- groups[[i]]
      
      if (nrow(g) == 1) {
        # Para un solo país
        label_row <- g
        label_row$concatenated_label <- g[[label_var]]
      } else {
        # Para múltiples países, usar el primer registro pero concatenar etiquetas
        label_row <- g[1, ]
        label_row$concatenated_label <- paste(g[[label_var]], collapse = " ")
      }
      
      result <- rbind(result, label_row)
    }
  }
  
  return(result)
}

# Preparar los datos para etiquetas
# label_data <- prepare_labels(zz, "n", "iso3_code", "region", tolerance = 0.01)


# Crear el gráfico con etiquetas agrupadas
crear_boxplot_etiquetado <- function(datos, y, x, color, 
                                     etiqueta_data = NULL,
                                     etiqueta_var = NULL,
                                     titulo = "Boxplot por región",
                                     tema = ggplot2::theme_minimal()) {
  
  # Convertir los argumentos de texto a expresiones
  y_var <- rlang::sym(y)
  x_var <- rlang::sym(x)
  color_var <- rlang::sym(color)
  
  # Crear el gráfico base
  p <- ggplot2::ggplot(datos, ggplot2::aes(y = !!y_var, x = !!x_var, color = !!color_var)) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::labs(
      title = titulo,
      x = x,
      y = y
    ) +
    tema
  
  # Añadir etiquetas si se proporcionan los datos de etiquetas
  if (!is.null(etiqueta_data) && !is.null(etiqueta_var)) {
    etiqueta_var_sym <- rlang::sym(etiqueta_var)
    
    p <- p + ggplot2::geom_text(
      data = etiqueta_data,
      ggplot2::aes(
        y = !!y_var, 
        x = as.numeric(factor(!!x_var)) + 0.1, 
        label = !!etiqueta_var_sym
      ),
      color = "black",
      size = 2.3,
      hjust = 0
    )
  }
  
  return(p)
}

# p <- crear_boxplot_etiquetado(
#   datos = zz,
#   y = "n",
#   x = "region",
#   color = "region.x",
#   etiqueta_data = label_data,
#   etiqueta_var = "concatenated_label",
#   titulo = "Distribución por región"
# )

# p + coord_flip()

# plotly::ggplotly(p)




#- segunda intentona ---------

prepare_labels_2 <- function(data, y_var, label_var, group_var, tolerance = 0.01, 
                           right_offset = 0.25, left_offset = -0.25, 
                           space_between = "  ", color1 = "black", color2 = "blue") {
  
  # Preparar un data frame vacío con todas las columnas necesarias
  result <- data.frame(
    matrix(ncol = ncol(data) + 5, nrow = 0)
  )
  colnames(result) <- c(colnames(data), "concatenated_label", "position", "nudge_x", "hjust", "text_color")
  
  # Para cada región, procesar por separado
  for (reg in unique(data[[group_var]])) {
    region_data <- data[data[[group_var]] == reg, ]
    region_data <- region_data[order(region_data[[y_var]]), ]
    
    # Inicializar variables para este grupo
    groups <- list()
    current_group <- 1
    groups[[current_group]] <- region_data[1, ]
    
    # Agrupar países con valores Y similares dentro de esta región
    if (nrow(region_data) > 1) {
      for (i in 2:nrow(region_data)) {
        last_y <- tail(groups[[current_group]], 1)[[y_var]]
        current_y <- region_data[i, ][[y_var]]
        
        # Si el valor Y es similar al último del grupo actual, añade al mismo grupo
        if (abs(current_y - last_y) <= tolerance * diff(range(data[[y_var]]))) {
          groups[[current_group]] <- rbind(groups[[current_group]], region_data[i, ])
        } else {
          # Si no, crea un nuevo grupo
          current_group <- current_group + 1
          groups[[current_group]] <- region_data[i, ]
        }
      }
    }
    
    # Crear etiquetas y posiciones para cada grupo
    for (i in 1:length(groups)) {
      g <- groups[[i]]
      
      if (nrow(g) == 1) {
        # Para un solo país
        label_row <- g
        label_row$concatenated_label <- g[[label_var]]
        label_row$position <- "right1"  # Por defecto a la derecha, primera posición
        label_row$nudge_x <- right_offset
        label_row$hjust <- 0
        label_row$text_color <- color1
        result <- rbind(result, label_row)
        
      } else {
        # Para múltiples países en el mismo nivel Y
        # Primero, añadir todos individualmente con posiciones predeterminadas
        for (j in 1:nrow(g)) {
          label_row <- g[j, ]
          current_label <- g[j, ][[label_var]]
          
          # Determinar posición y color
          if (j %% 4 == 1) {         # 1º, 5º, 9º... - Derecha primero, color1
            position <- "right1"
            nudge <- right_offset
            hjust_val <- 0
            color <- color1
            
          } else if (j %% 4 == 2) {  # 2º, 6º, 10º... - Izquierda primero, color1
            position <- "left1"
            nudge <- left_offset
            hjust_val <- 1
            color <- color1
            
          } else if (j %% 4 == 3) {  # 3º, 7º, 11º... - Derecha segundo, color2
            position <- "right2"
            nudge <- right_offset
            hjust_val <- 0
            color <- color2
            
            # Buscar el primer país a la derecha para combinarlo
            right1_idx <- which(result$position == "right1" & 
                                  abs(result[[y_var]] - label_row[[y_var]]) < tolerance * diff(range(data[[y_var]])))
            
            if (length(right1_idx) > 0) {
              # Combinar con el primero de la derecha
              new_label <- paste(result$concatenated_label[right1_idx[1]], space_between, current_label)
              result$concatenated_label[right1_idx[1]] <- new_label
              result$text_color[right1_idx[1]] <- color2
              # Saltar la adición de esta fila
              next
            }
            
          } else if (j %% 4 == 0) {  # 4º, 8º, 12º... - Izquierda segundo, color2
            position <- "left2"
            nudge <- left_offset
            hjust_val <- 1
            color <- color2
            
            # Buscar el primer país a la izquierda para combinarlo
            left1_idx <- which(result$position == "left1" & 
                                 abs(result[[y_var]] - label_row[[y_var]]) < tolerance * diff(range(data[[y_var]])))
            
            if (length(left1_idx) > 0) {
              # Combinar con el primero de la izquierda
              new_label <- paste(current_label, space_between, result$concatenated_label[left1_idx[1]])
              result$concatenated_label[left1_idx[1]] <- new_label
              result$text_color[left1_idx[1]] <- color2
              # Saltar la adición de esta fila
              next
            }
          }
          
          # Añadir información a la fila
          label_row$concatenated_label <- current_label
          label_row$position <- position
          label_row$nudge_x <- nudge
          label_row$hjust <- hjust_val
          label_row$text_color <- color
          
          # Añadir a los resultados
          result <- rbind(result, label_row)
        }
      }
    }
  }
  
  return(result)
}


# Preparar los datos para etiquetas
# label_data_2 <- prepare_labels_2(zz, "n", "iso3_code", "region", 
#                              tolerance = 0.01, 
#                              right_offset = 0.1, 
#                              left_offset = -0.1,
#                              space_between = "  ",
#                              color1 = "black", 
#                              color2 = "black")

# Crear el gráfico
# p <- ggplot(zz, aes(y = n, x = region, color = region.x)) +
#   geom_boxplot(outlier.shape = NA) +
#   geom_text(
#     data = label_data_2,
#     aes(y = n, 
#         x = as.numeric(factor(region)) + nudge_x, 
#         label = concatenated_label,
#         hjust = hjust),
#     color = label_data$text_color,
#     size = 2.3
#   )
# p <- p + coord_flip()
# 
# plotly::ggplotly(p)

#- tercera intentona (ok) ------------------------------------------------------
p_boxplot_interactivo <- function(datos, 
                                  num_columnas = 1, 
                                  colores = NULL, 
                                  anchura_svg = 10, 
                                  altura_svg = 8,
                                  proporcion_anchura = c(3, 1),
                                  col_n = "n", 
                                  col_pais = "pais", 
                                  col_iso3 = "iso3", 
                                  col_region = "region") {
  require(ggplot2)
  require(ggiraph)
  require(dplyr)
  require(patchwork)
  require(stringr)
  
  # Verificar que las columnas existen
  columnas_especificadas <- c(col_n, col_pais, col_iso3, col_region)
  if (!all(columnas_especificadas %in% colnames(datos))) {
    stop("El dataframe debe contener las columnas especificadas en col_n, col_pais, col_iso3 y col_region.")
  }
  
  # Crear copia para no modificar original
  df <- datos
  
  # Reordenar regiones por mediana
  medianas_por_region <- df %>%
    group_by(.data[[col_region]]) %>%
    summarize(mediana = median(.data[[col_n]])) %>%
    arrange(mediana)
  
  df[[col_region]] <- factor(df[[col_region]], levels = medianas_por_region[[col_region]])
  
  # Crear paleta de colores si no se da
  regiones_unicas <- unique(df[[col_region]])
  if (is.null(colores)) {
    colores_regiones <- scales::hue_pal()(length(regiones_unicas))
    names(colores_regiones) <- regiones_unicas
  } else {
    if (length(colores) < length(regiones_unicas)) {
      warning("No hay suficientes colores. Se generan adicionales.")
      colores_adicionales <- scales::hue_pal()(length(regiones_unicas) - length(colores))
      colores_regiones <- c(colores, colores_adicionales)
    } else {
      colores_regiones <- colores[1:length(regiones_unicas)]
    }
    names(colores_regiones) <- regiones_unicas
  }
  
  # -------- Gráfico 1: Boxplot interactivo con códigos ISO3 --------
  g1 <- ggplot(df, aes(x = .data[[col_n]], y = .data[[col_region]])) +
    geom_boxplot(aes(fill = .data[[col_region]]), outlier.shape = NA, width = 0.3, alpha = 0.7) +
    geom_text_interactive(
      aes(
        label = .data[[col_iso3]],
        tooltip = paste("País:", .data[[col_pais]], 
                        "<br>Código:", .data[[col_iso3]], 
                        "<br>Valor:", .data[[col_n]]),
        data_id = .data[[col_pais]]
      ),
      position = position_jitter(height = 0.15, width = 0.05, seed = 123),
      size = 2.5,  # Tamaño más pequeño
      color = "black",  # Todos en negro
      alpha = 0.8
    ) +
    scale_fill_manual(values = colores_regiones) +
    labs(
      title = "Distribución de valores por región",
      x = "Valor",
      y = "Región"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none",
      # Cambio para que los labels de regiones estén en negro
      axis.text.y = element_text(color = "black")
    )
  
  # -------- Gráfico 2: Códigos ISO agrupados por región --------
  # Preparar datos para el panel derecho
  datos_iso <- df %>%
    select(all_of(c(col_pais, col_iso3, col_region, col_n)))
  
  # Guardar los niveles de regiones para mantener el orden consistente
  niveles_region <- levels(df[[col_region]])
  
  # MODIFICACIÓN: Usar el mismo orden de regiones que en el boxplot izquierdo
  # Eliminar la inversión que estaba aquí anteriormente
  
  # Contar países por región para distribuir verticalmente
  conteo_por_region <- datos_iso %>%
    group_by(.data[[col_region]]) %>%
    summarize(n_paises = n_distinct(.data[[col_iso3]])) %>%
    # Usar el mismo orden que el boxplot
    mutate(!!col_region := factor(.data[[col_region]], levels = niveles_region))
  
  # Crear posiciones para cada ISO code
  datos_posicion <- data.frame()
  offset_y <- 0
  
  # Procesamos las regiones en el MISMO orden que el boxplot
  for (region_actual in niveles_region) {
    # Obtener el recuento para esta región
    n_paises <- conteo_por_region %>% 
      filter(.data[[col_region]] == region_actual) %>% 
      pull(n_paises)
    
    # Filtrar países de esta región y mantener el orden
    paises_region <- datos_iso %>%
      filter(.data[[col_region]] == region_actual) %>%
      distinct(.data[[col_iso3]], .keep_all = TRUE) %>%
      arrange(.data[[col_iso3]])  # Orden alfabético dentro de cada región
    
    # Crear posiciones en filas (máximo 8 códigos por fila)
    max_por_fila <- 8
    n_filas <- ceiling(n_paises / max_por_fila)
    
    for (j in 1:n_paises) {
      fila <- ceiling(j / max_por_fila)
      col <- (j - 1) %% max_por_fila + 1
      
      temp_df <- paises_region[j, ]
      temp_df$pos_x <- col
      temp_df$pos_y <- offset_y + fila
      # Mantener el mismo factor de región que el boxplot
      temp_df$region_factor <- factor(temp_df[[col_region]], levels = niveles_region)
      
      datos_posicion <- rbind(datos_posicion, temp_df)
    }
    
    # Actualizar offset para la siguiente región (añadir filas + espacio)
    offset_y <- offset_y + n_filas + 1
  }
  
  # Ya no necesitamos invertir el eje y
  y_max <- max(datos_posicion$pos_y) + 1
  datos_posicion$pos_y_inv <- datos_posicion$pos_y
  
  # Preparar etiquetas de región correctamente ordenadas en el mismo orden que el boxplot
  etiquetas_region <- datos_posicion %>% 
    group_by(region_factor) %>% 
    summarize(
      pos_x = 0.5, 
      pos_y_inv = min(pos_y_inv) - 0.5,  # Posición ajustada
      !!col_region := first(.data[[col_region]])
    ) %>%
    # Ordenar según el mismo factor que el boxplot
    arrange(match(region_factor, niveles_region))
  
  # Crear el gráfico de códigos ISO3 por región
  g2 <- ggplot(datos_posicion, aes(x = pos_x, y = pos_y_inv)) +
    # Añadir etiquetas para las regiones
    geom_text(
      data = etiquetas_region,
      aes(label = .data[[col_region]], color = .data[[col_region]]),
      hjust = 0,
      size = 3,
      fontface = "bold"
    ) +
    geom_text_interactive(
      aes(
        label = .data[[col_iso3]],
        tooltip = paste("País:", .data[[col_pais]], 
                        "<br>Código:", .data[[col_iso3]],
                        "<br>Región:", .data[[col_region]], 
                        "<br>Valor:", .data[[col_n]]),
        data_id = .data[[col_pais]],
        color = .data[[col_region]]
      ),
      size = 2.3,
      hjust = 0.5,
      vjust = 0.5
    ) +
    scale_color_manual(values = colores_regiones) +
    scale_x_continuous(limits = c(0, 9)) +
    scale_y_continuous(limits = c(0, y_max)) +
    coord_cartesian(expand = FALSE) +
    labs(
      title = "Códigos ISO3 por región",
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    )
  
  # -------- Interactividad --------
  grafico_combinado <- girafe(
    code = { print(g1 + g2 + plot_layout(widths = proporcion_anchura)) },
    width_svg = anchura_svg,
    height_svg = altura_svg,
    options = list(
      opts_hover(css = "fill:orange;font-weight:bold;"),
      opts_hover_inv(css = "opacity:0.5;"),
      opts_selection(
        css = paste0(
          "fill:black !important;",
          "stroke:black !important;",
          "opacity:1 !important;",
          "font-weight:bold;"
        ),
        type = "single"
      ),
      opts_tooltip(css = "background-color:white;color:black;padding:5px;border-radius:3px;font-weight:bold;")
    )
  )
  
  return(grafico_combinado)
}

#- cuarta ----------------
#- con _giraffe_con_gemini_good_ok.R

crear_boxplot_interactivo <- function(datos, 
                                      num_columnas = 3, 
                                      colores = NULL, 
                                      anchura_svg = 10, 
                                      altura_svg = 8,
                                      proporcion_anchura = c(3, 1)) {
  
  # Cargar las librerías necesarias (mejor usar library() al inicio del script/paquete)
  # O usar :: para llamar funciones específicas sin cargar la librería completa.
  # Por ahora, mantenemos require() como en el original.
  require(ggplot2)
  require(ggiraph)
  require(dplyr)
  require(patchwork)
  
  # Verificar que los datos tienen las columnas requeridas
  columnas_requeridas <- c("n", "pais", "iso3", "region")
  if (!all(columnas_requeridas %in% colnames(datos))) {
    stop("El dataframe debe contener las columnas: 'n', 'pais', 'iso3' y 'region'")
  }
  
  # Calcular las medianas de cada región para reordenar
  medianas_por_region <- datos %>%
    group_by(region) %>%
    summarize(mediana = median(n, na.rm = TRUE)) %>% # Asegurar na.rm=TRUE si hay NAs
    arrange(mediana)
  
  # Convertir región a factor con los niveles ordenados por la mediana
  datos$region <- factor(datos$region, levels = medianas_por_region$region)
  
  # Crear colores para las regiones si no se proporcionan
  regiones_unicas <- levels(datos$region) # Usar levels para mantener el orden
  if (is.null(colores)) {
    colores_regiones <- scales::hue_pal()(length(regiones_unicas))
    names(colores_regiones) <- regiones_unicas
  } else {
    if (length(colores) < length(regiones_unicas)) {
      warning("No hay suficientes colores para todas las regiones. Se generarán colores adicionales.")
      colores_adicionales <- scales::hue_pal()(length(regiones_unicas) - length(colores))
      colores_regiones <- c(colores, colores_adicionales)
    } else {
      colores_regiones <- colores[1:length(regiones_unicas)]
    }
    names(colores_regiones) <- regiones_unicas
  }
  
  # Primer gráfico: Boxplot con regiones ordenadas por mediana
  g1 <- ggplot(datos, aes(x = n, y = region)) +
    geom_boxplot(aes(fill = region), outlier.shape = NA, alpha = 0.3) + # Añadido fill al boxplot
    geom_point_interactive(
      aes(tooltip = paste("País:", pais, "<br>Código:", iso3, "<br>Valor:", n),
          data_id = pais, # Usar data_id único por punto, país es bueno si es único
          color = region),
      position = position_jitter(height = 0.2, seed = 123),
      alpha = 0.8, # Un poco más opaco
      size = 3
    ) +
    scale_color_manual(values = colores_regiones, guide = "none") + # Quitar leyenda de color
    scale_fill_manual(values = colores_regiones, guide = "none") + # Quitar leyenda de fill
    labs(
      title = "Distribución de valores por región",
      x = "Valor (n)",
      y = "Región"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none" # Leyenda ya quitada en scale_*_manual
    )
  
  # Preparar datos para el segundo gráfico con ISO3 en múltiples columnas
  datos_iso <- datos %>% 
    distinct(iso3, .keep_all = TRUE) %>% # Forma más moderna con dplyr
    select(pais, iso3, region, n) %>% # Incluir columna n
    arrange(iso3) 
  
  num_paises <- nrow(datos_iso)
  paises_por_columna <- ceiling(num_paises / num_columnas)
  
  # Crear un dataframe para las columnas de códigos ISO3
  datos_columnas <- datos_iso %>%
    mutate(
      columna = rep(1:num_columnas, each = paises_por_columna, length.out = num_paises),
      fila = rep(1:paises_por_columna, times = num_columnas, length.out = num_paises)
    )
  
  # --- MODIFICACIÓN PARA MAYOR COMPACTACIÓN ---
  # Segundo gráfico: Lista compacta de códigos ISO3 ordenados alfabéticamente en columnas
  g2 <- ggplot(datos_columnas, aes(x = columna, y = fila)) +
    geom_text_interactive(
      aes(label = iso3,
          tooltip = paste("País:", pais, "<br>Código:", iso3, "<br>Región:", region, "<br>Valor:", n),
          data_id = pais, # Usar el mismo data_id que en g1 para vincular
          color = region),
      size = 2.2,  # Tamaño de texto ligeramente más pequeño para compactar
      hjust = 0.5, 
      vjust = 0.5, 
      fontface = "bold"
    ) +
    scale_color_manual(values = colores_regiones, guide = "none") + # Quitar leyenda
    # Ajustar límites y expansión para reducir el espacio entre códigos
    scale_x_continuous(
      limits = c(1 - 0.4, num_columnas + 0.4), # Reducir margen horizontal
      expand = c(0, 0) # Eliminar expansión adicional
    ) + 
    scale_y_reverse(
      limits = c(paises_por_columna + 0.4, 1 - 0.4), # Reducir margen vertical
      expand = c(0, 0) # Eliminar expansión adicional
    ) +
    # coord_cartesian(expand = FALSE) # expand=c(0,0) en las escalas es más directo
    labs(
      title = "Códigos ISO3",
      x = NULL,
      y = NULL
    ) +
    theme_void(base_size = 10) + # theme_void() elimina casi todo
    theme(
      plot.title = element_text(hjust = 0.5, size = 10),
      plot.margin = margin(5, 5, 5, 5), # Un pequeño margen para que no se pegue a los bordes
      legend.position = "none"
    )
  
  # Crear gráfico combinado con estilo de selección
  grafico_combinado <- girafe(
    code = {
      # Usar patchwork para combinar los gráficos
      print(g1 + g2 + plot_layout(widths = proporcion_anchura))
    }, 
    width_svg = anchura_svg, 
    height_svg = altura_svg,
    options = list(
      opts_hover(css = "fill:orange;font-weight:bold;"), # Estilo al pasar el ratón
      opts_hover_inv(css = "opacity:0.3;"), # Estilo de los NO hovereados
      # --- COMPORTAMIENTO DE SELECCIÓN ---
      # Esta configuración ya hace lo que pides:
      # - fill:black !important; stroke:black !important; -> Pone el punto negro
      # - r:8px !important; -> Aumenta el radio del punto (más grande)
      # - opacity:1 !important; -> Asegura que sea totalmente opaco
      opts_selection(
        css = "fill:black !important; stroke:black !important; r:8px !important; opacity:1 !important;",
        type = "single" # Solo permite seleccionar un punto a la vez
      ),
      opts_tooltip(css = "background-color:white;color:black;padding:5px;border-radius:3px;font-weight:normal;border:1px solid grey;") # Estilo tooltip
    )
  )
  
  return(grafico_combinado)
}



#- 3. geom_rtile ---------------------------------------------------------------
library(stringr)

#- ff --------------------------------------------------------------------------
# Función para dividir texto en líneas de máximo 10 caracteres
wrap_labels <- function(x, max_length = 20) {
  sapply(x, function(label) {
    # Dividir la cadena en palabras
    words <- unlist(strsplit(label, " "))
    
    # Inicializar variables
    current_line <- ""
    wrapped_lines <- c()
    
    for(word in words) {
      # Si añadir la palabra no supera el máximo, añadirla
      if(nchar(paste(current_line, word)) <= max_length) {
        current_line <- ifelse(current_line == "", word, paste(current_line, word))
      } else {
        # Guardar línea actual y empezar nueva
        wrapped_lines <- c(wrapped_lines, current_line)
        current_line <- word
      }
    }
    
    # Añadir última línea
    wrapped_lines <- c(wrapped_lines, current_line)
    
    # Combinar líneas con salto de línea
    return(paste(wrapped_lines, collapse = "\n"))
  })
}



#- grear un geom_rtile() -------------------------------------------------------
#- para hacer redondeados los tiles
#- https://stackoverflow.com/questions/64355877/round-corners-in-ggplots-geom-tile-possible

`%||%` <- function(a, b) {
  if(is.null(a)) b else a
}

GeomRtile <- ggproto("GeomRtile", 
                     statebins:::GeomRrect, # 1) only change compared to ggplot2:::GeomTile
                     
                     extra_params = c("na.rm"),
                     setup_data = function(data, params) {
                       data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
                       data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)
                       
                       transform(data,
                                 xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
                                 ymin = y - height / 2, ymax = y + height / 2, height = NULL
                       )
                     },
                     default_aes = aes(
                       fill = "grey20", colour = NA, size = 0.1, linetype = 1,
                       alpha = NA, width = NA, height = NA
                     ),
                     required_aes = c("x", "y"),
                     
                     # These aes columns are created by setup_data(). They need to be listed here so
                     # that GeomRect$handle_na() properly removes any bars that fall outside the defined
                     # limits, not just those for which x and y are outside the limits
                     non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),
                     draw_key = draw_key_polygon
)


geom_rtile <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       radius = grid::unit(6, "pt"), # 2) add radius argument
                       ...,
                       #linejoin = "mitre",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRtile, # 3) use ggproto object here
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      radius = radius,
      #linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}


crear_heatmap <- function(datos, 
                          x_var, 
                          y_var, 
                          valor_var,
                          titulo = "Heatmap", 
                          etiqueta_x = NULL, 
                          etiqueta_y = NULL, 
                          etiqueta_valor = NULL,
                          color_bajo = "lightblue", 
                          color_alto = "darkblue",
                          na_color = "lightgrey",
                          radio = 4,
                          tamano_texto = 3) {
  
  # Establecer etiquetas predeterminadas si no se proporcionan
  if (is.null(etiqueta_x)) etiqueta_x <- x_var
  if (is.null(etiqueta_y)) etiqueta_y <- y_var
  if (is.null(etiqueta_valor)) etiqueta_valor <- valor_var
  
  # Función auxiliar para envolver etiquetas largas
  wrap_labels <- function(x) {
    sapply(x, function(y) paste(strwrap(y, width = 20), collapse = "\n"))
  }
  
  # Crear copia de datos para manipulación segura
  df <- datos
  x_col <- df[[x_var]]
  y_col <- df[[y_var]]
  valor_col <- df[[valor_var]]
  
  # Crear el gráfico usando variables directamente
  p <- ggplot(df, aes(x = x_col, y = y_col)) +
    geom_rtile(aes(fill = valor_col), 
               color = "white", 
               width = 0.99, 
               height = 0.99, 
               position = position_nudge(x = 0, y = 0),
               radius = unit(radio, "pt")) +  
    geom_text(aes(label = ifelse(is.na(valor_col), "", as.character(valor_col))), 
              color = "white",
              size = tamano_texto) +
    scale_fill_gradient(
      low = color_bajo, 
      high = color_alto, 
      na.value = na_color
    ) +
    theme_minimal() +
    labs(
      title = titulo,
      x = etiqueta_x,
      y = etiqueta_y,
      fill = etiqueta_valor
    ) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      axis.text.y = element_text(size = 8, lineheight = 0.8),
      panel.background = element_rect(fill = "white", color = "white"),
      plot.background = element_rect(fill = "white", color = "white"),
      panel.grid = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    coord_fixed(ratio = 1)
  
  # Aplicar wrap_labels solo a los valores únicos de la variable y
  # para evitar problemas de dimensiones incompatibles
  unique_y_labels <- unique(y_col)
  wrapped_labels <- wrap_labels(unique_y_labels)
  
  # Asignar etiquetas envueltas a los niveles correspondientes
  p <- p + scale_y_discrete(labels = setNames(wrapped_labels, unique_y_labels)) 
  
  #+ scale_x_continuous(breaks = seq(min(x_col, na.rm = TRUE), max(x_col, na.rm = TRUE), by = 1)
                       #labels = function(x) substr(x, 3, 4)  # Mostrar solo los últimos dos dígitos
                       #)  
  p <- p + theme(axis.text.x = element_text(size = 5, angle = 0, hjust = 0.5))
  
  return(p)
}
