#- datos crimen UN: https://dataunodc.un.org/
#- descargué los datos (el xx de marzo de 2025)
#- en data.mungings arreglo los datos y añado geometrías

my_no_borrar <- c("crim_rel", "crim_geo", "crim_tipos", "my_no_borrar")
rm(list=ls()[! ls() %in% my_no_borrar])

#- AQUI parto de esos datos ya arreglados y en el pkg
library(tidyverse)
library(sf)

crim_rel <- pjpv.pkg.datos.2024::UNDOC_delitos_x_relacion_2013_23
crim_geo <- pjpv.pkg.datos.2024::UNDOC_geometrias
crim_tipos <- pjpv.pkg.datos.2024::UNDOC_delitos_tipologia_1990_2023

#- trabajo con df
df <- crim_tipos

df_dicc <- pjpv.pkg.ff.2024::pjp_dicc(df)
df_uniques <- pjpv.pkg.ff.2024::pjp_valores_unicos(df, nn = 250)


#- 1. nº obs x pais ------------------------------------------------------------
#- quiero ver q país tiene más observaciones
zz <- df %>% 
  select(iso3_code, country, year, tipo_delito, tasa_100mil) %>% 
  group_by(iso3_code, country) %>% 
  count() %>% 
  arrange(desc(n))

#- 2. nº obs x delito x pais ---------------------------------------------------
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
