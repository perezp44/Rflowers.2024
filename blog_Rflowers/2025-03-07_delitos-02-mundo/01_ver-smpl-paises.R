#- datos crimen UN: https://dataunodc.un.org/
#- aquí quiero ver el smpl de los datos
#- qué países y tipos de delitos tengo


#- cargo funciones ----------
source(here::here("./_funciones/_mys_ff.R"))


#- 1. nº obs x pais ------------------------------------------------------------
#- q país tiene más observaciones?? Eslovaquia tiene 331 observaciones
zz <- df %>% 
  select(iso3_code, region, country, year, tipo_delito.pjp, tasa_100mil) %>% 
  group_by(iso3_code, region, country) %>% 
  count() %>% 
  arrange(desc(n))


#- preparo zz para poder hacer COROPLETA
#- fusiono geometrías con datos de crímenes (Nº de observaciones)
# df_maps_0 <- left_join(zz, crim_geo, by = c("iso3_code" = "iso3_code")) 
# df_maps_f <- right_join(crim_geo, zz, by = c("iso3_code" = "iso3_code")) 
df_maps_ff <- left_join(crim_geo %>% filter(quitar_para_crimenes != "Si quitar"), zz, by = c("iso3_code" = "iso3_code")) 


#- zz_m es el df q uso para la coropleta
zz_m <- df_maps_ff %>% 
  #rename(pais = NAME_ENGL) %>% 
  mutate(n = as.integer(n))



##- 1.a: Coropleta --------
#- creo q casi lo mejor es hacer una coropleta 

### - ggplot2 ------------------------------------------------------------------

my_name_leyenda =  "Nº obs."
my_title = "Nº de observaciones totales (1990-2023)"
my_subtitle = "(para los 27 tipos de delitos)"


pp <- p_coropleta_gg(zz_m, fill = n, 
                    group = pais, #- solo hace falta para que en ggplotly se vea el nombre del país
                    name_leyenda = my_name_leyenda,
                    title = my_title,
                    subtitle = my_subtitle)
pp


#- version quoted
my_fill  <- "n"      #- vv. para colorar los polígonos
my_group <- "pais"   #- solo hace falta para q se vea el país en ggplotly

pp <- p_coropleta_gg_q(zz_m, fill = my_fill, 
                     group = my_group, #- solo hace falta para que en ggplotly se vea el nombre del país
                     name_leyenda = my_name_leyenda,
                     title = my_title,
                     subtitle = my_subtitle)

pp

pp <- p_coropleta_gg_II(zz_m, fill = my_fill, 
                       group = my_group, #- hace falta para que en ggplotly se vea el nombre del país
                       name_leyenda = my_name_leyenda,
                       title = my_title,
                       subtitle = my_subtitle) 

pp
### ggplotly ----
plotly::ggplotly(pp, tooltip = c(my_group, my_fill), dynamicTicks = TRUE) 
#layout(title = "Click and drag to select points") 
  

###- leaflet -------------------------------------------------------------------

p_coro_leaflet_tidy(zz_m, n)
p_coro_leaflet_tidy_q(zz_m, my_fill, obs_label = "Nº observaciones.") #- quoted
#- no tidyeval

p_coro_leaflet_II(zz_m, n, obs_label = "Nº observaciones", view_zoom = 3)
p_coro_leaflet_II(zz_m, "n", obs_label = "Nº observ.")

# Para guardar el mapa como un archivo HTML (opcional)
# library(htmlwidgets)
# saveWidget(mapa, file = "mapa_coropletico.html")


###- echarts4r -----------------------------------------------------------------
#- https://echarts4r.john-coene.com/
library(echarts4r)
library(sf)

#- voy a poner los nombres de países q (creo q) usa echarts (x debajo usa tidycountries)
#- ya lo h hecho en 00_cargar_datos
# zz_mm <- zz_m %>% 
#   filter(!is.na(CNTR_ID)) %>% 
#   filter(CNTR_ID %in% countrycode::codelist$iso2c) %>% 
#   dplyr::mutate(pais.iso = countrycode::countrycode(sourcevar = iso3_code, 
#                                                     origin = "iso3c", 
#                                                     destination = "country.name.en"))


p <- create_echarts_map(zz_m, iso_col = "pais.iso", value_col = "n")

p |> e_theme("vintage")


###- highcharter ---------------------------------------------------------------
#- https://jkunst.com/highcharter/articles/maps.html
library(highcharter)
p <- create_highcharter_map(zz_m)

p



###- tmap ----------------------------------------------------------------------
library(tmap)

p_coro_tmap(zz_m, "n", basemap = "CartoDB.Positron", palette = "Blues")



##- 1.b: Boxplot --------
#- tb puedo hacer tb un boxplot (por continente) like this:

#- en realidad lo chulo será dos gráficos conectados: el primer gráfico el boxplot con los puntitos 
#- y el segundo plot simplemente los nombres de los países alfabéticamente

#- quiero reordenar los box
zz <- zz %>% 
  mutate(region = forcats::as_factor(region)) %>% 
  group_by(region) %>%
  mutate(mean_n = mean(n, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(region = forcats::fct_reorder(region, mean_n)) %>% 
  mutate(region.x = region)


#- solo pongo los labels (iso3_code)
#- quiero poner el nombre del país (country) en el tooltip pero no lo consigo
p <- ggplot(zz, aes(y = n, x = region, color = region.x)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_jitter(aes(y = n, x = reorder(region, n, median))) +
  #geom_point(aes(label = country), x = 0, y = 0, color = "black", size = 1) +
  geom_text(aes(label = iso3_code), position = position_jitter(), color = "black", size = 2.3) 

p


#- no consigo que en el tooltip se vea "country"
plotly::ggplotly(p, tooltip = c("region", "country", "n"))
#- TODO: probar a hacer el plot con el pkg ggigraph: 
#- un ejemplo de plot: https://github.com/deepdk/TidyTuesday2024/tree/main/2024/week_33

###- 1 
# Preparar los datos para etiquetas
label_data <- prepare_labels(zz, "n", "iso3_code", "region", tolerance = 0.01)


p <- crear_boxplot_etiquetado(datos = zz,
  y = "n",
  x = "region",
  color = "region.x",
  etiqueta_data = label_data,
  etiqueta_var = "concatenated_label",
  titulo = "Distribución por región")

 p + coord_flip()

 plotly::ggplotly(p)


 p <- ggplot(zz, aes(y = n, x = region, color = region.x)) +
   geom_boxplot(outlier.shape = NA) +
   geom_text(
     data = label_data_2,
     aes(y = n,
         x = as.numeric(factor(region)) + nudge_x,
         label = concatenated_label,
         hjust = hjust),
     color = label_data$text_color,
     size = 2.3
   )
 p <- p + coord_flip()

 plotly::ggplotly(p)
 
 ###- otro intento (good) ---------------
 pp <- p_boxplot_interactivo(zz,
                             col_n = "n",
                             col_pais = "country",
                             col_iso3 = "iso3_code",
                             col_region = "region")
 pp

 
 

###- tercera intentona (ok) ------------
 zzz <- zz %>% 
   rename(pais = country) %>% 
   rename(iso3 = iso3_code)
 
 # Llamar a la función para crear el objeto girafe
 pp <- crear_boxplot_interactivo(zzz)
 
 pp
 
 




 
 
 

#- 2. nº de obs x delito -------------------------------------------------------
#- quiero ver q delito tiene más observaciones 
#- AQUI: hay 8 q son los mas documentados
zz <- df %>% 
  select(iso3_code, region, country, year, tipo_delito, tasa_100mil) %>% 
  group_by(tipo_delito) %>% 
  count() %>% 
  arrange(desc(n))


 
 
#- 3. nº de obs x delito y año -------------------------------------------------
#- quiero ver q delito tiene más observaciones x año
#- para graficar una rounded heatmap
zz <- df %>% 
  select(iso3_code, country, year, tipo_delito.pjp, tasa_100mil) %>% 
  group_by(tipo_delito.pjp) %>% 
  mutate(nn_total = n()) %>% ungroup() %>% 
  group_by(tipo_delito.pjp, year) %>% 
  mutate(nn_year = n()) %>% ungroup() %>% 
  select(-iso3_code, -country, -tasa_100mil) %>% 
  distinct(tipo_delito.pjp, year, nn_total, nn_year) %>% 
  arrange(year) 
 
 
 zz_t <- zz %>% 
   pivot_wider(names_from = year, values_from = nn_year) 
 
#- paso a factor y reordeno niveles para el plot
 zzz <- zz %>% 
   mutate(tipo_delito.pjp.f = as.factor(tipo_delito.pjp)) %>% 
   mutate(tipo_delito.pjp.f = forcats::fct_reorder(tipo_delito.pjp.f, nn_total)) 
 
 
###- rounded heatmap ------------
pp <- crear_heatmap(datos = zzz, x_var = "year", y_var = "tipo_delito.pjp.f", valor_var = "nn_year")

pp






#- 4. nº de obs x pais y delito ------------------------------------------------
zz <- df %>% 
  select(iso3_code, country, year, tipo_delito.pjp, tasa_100mil) %>% 
  group_by(iso3_code, country) %>% 
  mutate(nn_total = n()) %>% ungroup() %>% 
  group_by(iso3_code, country, tipo_delito.pjp) %>% 
  mutate(nn_delito = n()) %>% ungroup() %>% 
  select(-year, -tasa_100mil) %>% 
  distinct() 

zz_t <- zz %>% pivot_wider(names_from = tipo_delito.pjp, values_from = nn_delito) 

##- 2.a
#- paso a factor y reordeno niveles para el plot
zzz <- zz %>% 
  mutate(country.f = as.factor(country)) %>% 
  mutate(country.f = forcats::fct_reorder(country.f, nn_delito)) %>% 
  mutate(tipo_delito.pjp.f = as.factor(tipo_delito.pjp)) 

zzzz <- zzz %>%
  mutate(tipo_delito.pjp.f = as.factor(tipo_delito.pjp)) %>% 
  group_by(tipo_delito.pjp.f) %>% 
  mutate(nn_total_deito = sum(nn_delito)) %>% ungroup() %>% 
  mutate(tipo_delito.pjp.f = forcats::fct_reorder(tipo_delito.pjp.f, nn_total_deito))  
  
  
###- rounded heatmap ------------
pp <- crear_heatmap(datos = zzzz, y_var = "country.f", x_var = "tipo_delito.pjp.f", valor_var = "nn_delito")

pp

##- 2.b
#- quiero hacer un histograma
paises_si <- zz %>% filter(iso3_code %in% c("SVK", "AUT", "FIN", "POL", "GER", "ITA", "FRA", "ESP", "HUN",  "IRL", "KEN"))
ggplot(zz, aes(x = nn_total)) + geom_histogram() +
  geom_jitter(aes(x = nn_total, y = -2))  +
  geom_text(data = paises_si, aes(x = nn_total, label = iso3_code, y = -5))



ggplot(zz, aes(x = nn_total)) + geom_histogram() +
  geom_jitter(aes(x = nn_total, y = -2))  +
  geom_text(data = paises_si, aes(x = nn_total, label = iso3_code, y = -5, position = position_jitter()))


ggplot(zz, aes(x = nn_total)) + geom_density() + geom_point(aes(x = nn_total))



ggplot(zz, aes(x = nn_total, y = -2)) + geom_jitter()




#- x año (esto tb otra tabla)
zz <- df %>% 
  select(iso3_code, country, year, tipo_delito, tasa_100mil) %>% 
  group_by(iso3_code, country) %>% 
  mutate(nn_total = n()) %>% ungroup() %>% 
  group_by(iso3_code, country, year) %>% 
  mutate(nn_year = n()) %>% ungroup() %>% 
  distinct(country, year, nn_year) %>% 
  pivot_wider(names_from = country, values_from = nn_year) %>% 
  relocate(Spain, .before = 2)




#- 2. nº obs x delito x país ---------------------------------------------------
#- quiero ver q país tiene más observaciones (para cada tipo_delito)
zz <- df %>% 
  select(iso3_code, country, year, tipo_delito, tasa_100mil) %>% 
  group_by(country, tipo_delito) %>% 
  count() %>% 
  #pivot_wider(names_from = tipo_delito, values_from = n) %>% 
  #select(-iso3_code) %>% 
  pivot_wider(names_from = country, values_from = n) %>% 
  identity()

#- seleccionó países a ver
my_paises <- c("France", "Italy")
zz_esp <- zz %>% select(Spain, all_of(my_paises)) %>% 
  arrange(desc(Spain))


#- 3. nº de observaciones en cada par (delito/año) -----------------------------
#- y si Spain tiene observación ese año/tipo de delito
zz1 <- df %>% 
  select(iso3_code, country, year, tipo_delito, tasa_100mil) %>% 
  group_by(year, tipo_delito) %>% 
  mutate(zz1 = n()) %>% 
  distinct(year, tipo_delito, zz1)

  
my_pais <- "Spain"  
zz2 <- df %>% 
  filter(country == my_pais) %>% 
  select(iso3_code, country, year, tipo_delito, tasa_100mil) %>% 
  group_by(year, tipo_delito) %>% 
  mutate(zz2 = n()) %>% 
  distinct(year, tipo_delito, zz2)

#- ESP le faltan datos de: "Unlawful interception or access of computer data"

zz3 <- left_join(zz1, zz2) %>% 
  #mutate(esp_na_negativo = ifelse(is.na(zz2), -1, 1)) %>% 
  #mutate(xx = zz1 * esp_na_negativo) %>% 
  #select(-zz1, -zz2, -esp_na_negativo) %>% 
  mutate(esp_si = ifelse(is.na(zz2), "*", "")) %>% 
  mutate(xx = paste0(zz1, esp_si)) %>% 
  select(-zz1, -zz2, -esp_si) %>% 
  arrange(year) %>% 
  tidyr::pivot_wider(names_from = year, values_from = xx, values_fill	= "-") 

#- hacer tabla coloreada (las celdas q españa no tiene datos)
#- https://stackoverflow.com/questions/71471367/gt-r-package-giving-a-different-color-to-a-tables-cells-according-to-numerical
my_data <- zz3
library(gt)
my_table <- gt::gt(my_data) 
#col.names.vect <- colnames(my_data)



my_no_esp_ff <- function(x) {
  stringr::str_detect(x, "\\*$")
}



for(i in seq_along(col.names.vect)) {
  my_table <- gt::tab_style(my_table,
                           style = gt::cell_fill(color="#f5ddd5"), 
                           locations = gt::cells_body(
                             columns = colnames(my_data)[i],
                             rows = my_no_esp_ff(my_table$`_data`[[colnames(my_data)[i]]]))) 
}


my_table




#- nº observaciones ------------------------------------------------------------
#- quiero ver en q año y delito  hay más observaciones
#- 2023 no hay casi datos; en 2022 tb hay pocos datos (salvo en homicidio)
zz <- df %>% 
  select(iso3_code, country, year, tipo_delito, tasa_100mil) %>% 
  group_by(year, tipo_delito) %>% 
  filter(!is.na(tasa_100mil)) %>% 
  count() %>% 
  tidyr::pivot_wider(names_from = year, values_from = n) %>% 
  arrange(tipo_delito) %>% 
  ungroup()

gt::gt(zz)
DT::datatable(zz)

#- quiero ver en que tipo_delito/year tiene observaciones Spain
zz_esp <- df %>% 
  filter(country == "Spain") %>% 
  select(iso3_code, country, year, tipo_delito, tasa_100mil) %>% 
  group_by(year, tipo_delito) %>% 
  filter(!is.na(tasa_100mil)) %>% 
  count() %>% 
  tidyr::pivot_wider(names_from = year, values_from = n) %>% 
  arrange(tipo_delito) %>% 
  ungroup()

waldo::compare(zz[[1]], zz_esp[[1]])

#- en la fila 25de ESP falta - "Unlawful interception or access of computer data"



#- RANKING's -------------------------------------------------------------------
#- ranking de Spain en el mundo cada año

tt_ranking <- df %>% 
  #filter(region == "Europe & Central Asia") %>% 
  #dplyr::filter(!is.na(!!sym(my_table))) %>% 
  # tidyr::drop_na(my_table)
  dplyr::filter(!is.na(tasa_100mil)) %>% 
  group_by(year, tipo_delito) %>%
  mutate(NN = n(), .after = tasa_100mil) %>%    #- cuantos hay cada año y categoría de delito
  arrange(desc(tasa_100mil)) %>% 
  mutate(rank = row_number(), .after = tasa_100mil) %>% 
  mutate(rank_normalizado = rank/NN, .after = tasa_100mil) %>% 
  mutate(percentil = (rank-1)/(NN-1), .after = tasa_100mil) %>%              #- Percentil del Ranking
  mutate(z_score = (rank - mean(rank))/sd(rank), .after = tasa_100mil) %>%   #- Z-Score del Ranking
  mutate(decil = ntile(rank, 10), .after = tasa_100mil) %>%                  #- Transformación en Deciles
  #filter(year == 2021) %>% 
  #filter(iso3c == "ESP") %>% 
  identity()


my_vv <- uniques_df$category[1]


tt_ranking_2019 <- tt_ranking %>% 
  filter(year == 2019) %>% 
  filter(category == my_vv) %>% 
  ungroup() %>% identity()

tt_ranking_spain <- tt_ranking %>% 
    filter(country == "Spain") %>% 
    filter(year == 2021) %>% 
    mutate(category_copy = category) %>% 
    ungroup() %>% identity()

tt_ranking_EU <- tt_ranking %>% filter(iso3_code %in% c("ESP", "FRA", "ITA")) %>%
  select(year, iso3_code, rank) %>%
  tidyr::pivot_wider(names_from = iso3_code, values_from = rank) %>% 
  mutate(esp_ita = ESP - ITA) %>% 
  mutate(esp_fra = ESP - FRA) 
  

tt_ranking_1 <- tt_ranking %>% 
  filter(rank == 1)

tt_ranking_venezuela <- tt_ranking %>% 
  filter(iso3_code == "VEN")


rm(list=ls()[! ls() %in% my_no_borrar])


#- MAPS ------------------------------------------------------------------------
library(sf)

#- fusiono países de crímenes UNDOC con geometrías
#- las geometrías ya están completas (UK, FRA e Iraq)
#- tb quite Antartida, Groenlandia, aunque en "Greenland" si hay datos de homicidios (solo homicidios)
#- tb quite muchos (unos 40) territorios/islitas de UK, US, NL, FRA y tb territorios en disputa
#- logicamente deje el Sahara
#- parece q se ve bien el mapa mundi 

ggplot(crim_geo, aes(geometry = geometry)) + 
  geom_sf(color = "green")

#- fusiono geometrías con datos de cíimenes
df_maps_0 <- left_join(df, crim_geo, by = c("iso3_code" = "iso3_code")) 
df_maps_f <- right_join(crim_geo, df, by = c("iso3_code" = "iso3_code")) 

##- elijo crimen y año ---------------------------------------------------------

vv_crimenes <- df %>% distinct(tipo_delito) %>% pull()
my_crimen <- vv_crimenes[15]
my_anyo <- "2005"

zz <- df_maps_f %>% 
  filter(year == my_anyo) %>% 
  filter(tipo_delito == my_crimen)

## - ggplot2 -------------------------------------------------------------------

ggplot(zz, aes(geometry = geometry)) + 
  geom_sf(data = crim_geo, aes(geometry = geometry)) +
  geom_sf(aes(fill = tasa_100mil)) 


#- ahora hacer una mapa mundi con datos del 2021
#- https://www.r-graph-gallery.com/327-chloropleth-map-from-geojson-with-ggplot2.html

ggplot2::ggplot(data = zz, aes()) +
  geom_sf(data = crim_geo, aes(geometry = geometry)) +
  geom_sf(aes(fill = tasa_100mil, geometry = geometry)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = paste0(my_crimen, " (por 100.000 habitantes). Perido: ", my_anyo),
       fill = my_crimen) +
  theme(plot.title = element_text(hjust = 0.5))

##- tmap -----------------------------------------------------------------------

#- https://marcinstepniak.eu/post/interactive-choropleth-maps-with-r-and-tmap-part-i/

library(tmap)  #- https://mtennekes.github.io/tmap/reference/qtm.html
library(sf)

# data(World)
# xx <- left_join(World, tt_ranking_2019, World, by = c("iso_a3" = "iso3c"))



# coropletas
qtm(zz, fill = "tasa_100mil", style = "cobalt", crs = "+proj=eck4")
qtm(zz, fill = "tasa_100mil")

# choropleth with way more specifications
qtm(zz, fill="tasa_100mil", fill.n = 9, fill.palette = "div",
    fill.title = "Happy Planet Index", fill.id = "name", 
    style = "gray", format = "World")

# this map can also be created with the main plotting method,
#- qtm() es para hacer quick graphs. Generalmente se usa está sintaxis con tmap
tm_shape(zz) +
  tm_polygons("tasa_100mil") +
  tm_layout(bg.color = "skyblue")


tm_shape(zz, projection = "+proj=eck4") +
  tm_polygons("tasa_100mil", n = 20, palette = "div", title = "Happy Planet Index", id = "name",
              # popup definition
              popup.vars=c("Country: " = "name", "Crimes: " = "VC.IHR.PSRC.P5")) +
  tm_layout(bg.color = "skyblue")



tm_shape(zz, projection = "+proj=eck4") +
  tm_polygons("tasa_100mil", n = 9, palette = "div", title = "Happy Planet Index", id = "isoa3") +
  tm_style("gray") +
  tm_format("World")

#- con tmap se pueden hacer gráficos interactivos
#- tmap_mode("plot") 
tmap_mode("view")  

tm_shape(zz) + tm_polygons("tasa_100mil", id = "country",  n =10)

tm_shape(zz) + 
  tm_polygons("tasa_100mil", id = "country",  n =10, palette = "div", 
              title = "Crimenes",
              popup.vars=c("Country: " = "country", "Crimes: " = "tasa_100mil"))


tm_shape(zz, filter = zz$region=="Europe") +
  tm_polygons("tasa_100mil", id = "country", n = 10)


#- tooltips: https://gis.stackexchange.com/questions/469419/using-tmap-r-package-to-plot-a-map-in-a-shiny-app-the-tooltip-remains-the-value
