#- datos crimen UN: https://dataunodc.un.org/
#- descargué los datos en marzo de 2025
#- en data.mungings arreglé los datos y añadí geometrías

my_no_borrar <- c("crim_geo", "crim_tipos", "my_no_borrar", "df", "df_dicc")
rm(list=ls()[! ls() %in% my_no_borrar])

#- AQUI parto de esos datos ya arreglados y en el pkg
library(tidyverse)
library(sf)

#crim_rel <- pjpv.pkg.datos.2024::UNDOC_delitos_x_relacion_2013_23
crim_geo <- pjpv.pkg.datos.2024::UNDOC_geometrias
crim_tipos <- pjpv.pkg.datos.2024::UNDOC_delitos_tipologia_1990_2023

#- trabajo con df
df <- crim_tipos

df_dicc <- pjpv.pkg.ff.2024::pjp_dicc(df)
df_uniques <- pjpv.pkg.ff.2024::pjp_valores_unicos(df, nn = 250)

#- elegir países (y geo) -------------------------------------------------------
#- hay 211 "países" con datos de delitos, pero unos 8 no tienen geometría
#- son los 3 de Uk (Escocia, irlanda del Norte e Inglaterra y gales), Kosovo y algunos territorios de ultramar franceses
#- igual me interesa quitar países si no tienen suficientes datos
#- NO, voy a dejar todos los países (ya quitaré Antaártida y demás en los plots)
#- crim_geo: las geometrías ya están completas (UK, FRA e Iraq)
#- pensé en quitar Antártida, Groenlandia y unos 40 territorios/islitas de UK, US, NL, FRA y tb territorios en disputa
#- pero finalmente los deje, aunque hay una vv. para poder quitarlos easy
#- en "Greenland" si hay datos de homicidios (solo homicidios)

zz1 <- crim_tipos %>% distinct(iso3_code, country, region) #- 211 países/territorios
zz2 <- crim_geo %>% sf::st_drop_geometry() %>% select(iso3_code, NAME_ENGL, SVRG_UN, CAPT,quitar_para_crimenes)
zz <- full_join(zz1, zz2)
zz <- left_join(zz1, zz2)


#- mis etiquetas ---------------------------------------------------------------
#- igual no tengo q mostrar todos los tipos de delitos
#- en cualquier caso he de arreglar las etiquetas de los delitos para los plots
zz <- df %>% distinct(tipo_delito)  #- 27 tipos de delitos

my_etiquetas <- c("Serious assault", "Kidnapping", "Sexual violence", "Rape", "Sexual assault", "Sexual violence: other", "Sexual Exploitation", "Induce to fear", "Induce to fear (cyber)", "Homicide", "Corruption", "Bribery", "Corruption: other", "Smuggling of migrants", "Burglary", "Theft", "Theft: vehicles", "Fraud", "Fraud (cyber)", "Money laundering",  "Access computer",  "Interference computer",  "Interception data", "Environmental", "Dumping of waste", "Trade protected species", "Natural resources")


df <- df %>% mutate(tipo_delito.pjp = tipo_delito) %>% 
  pjpv.pkg.ff.2024::pjp_ff_etiquetas(vv = tipo_delito.pjp, my_etiquetas= my_etiquetas) %>% 
  relocate(tipo_delito.pjp, .after = tipo_delito)

zz <- df %>% distinct(tipo_delito, tipo_delito.pjp)



#- voy a preparar geometrías 
#- quiero mantener Greenland, entonces esta linea
crim_geo <- crim_geo %>% 
  mutate(quitar_para_crimenes = if_else(NAME_ENGL == "Greenland", "No quitar", quitar_para_crimenes))

zz <- countrycode::codelist$iso3c %>% as.data.frame()

#- voy a poner los nombres de países tal como los usa echarts (x debajo usa tidycountries)
crim_geo <- crim_geo %>% 
  filter(!is.na(CNTR_ID)) %>% 
  filter(iso3_code %in% countrycode::codelist$iso3c) %>% #- algunas islitas etc...
  dplyr::mutate(pais.iso = countrycode::countrycode(sourcevar = iso3_code, 
                                                    origin = "iso3c", 
                                                    destination = "country.name.en"))

crim_geo <- crim_geo %>% 
  rename(pais = NAME_ENGL)
  


#- BORRANDO --------------------------------------------------------------------
rm(list=ls()[! ls() %in% my_no_borrar])


#- hasta AQUI ------------------------------------------------------------------
#- lo de abajo YA NO VALE (he hecho outsourcing)
#- simplemente lo dejo x si acaso m hiciese falta,
#- PERO lo iré borrando




#- 1. nº obs x pais ------------------------------------------------------------
#- quiero ver q país tiene más observaciones (Eslovaquía)
zz <- df %>% 
  select(iso3_code, region, country, year, tipo_delito, tasa_100mil) %>% 
  group_by(iso3_code, region, country) %>% 
  count() %>% 
  arrange(desc(n))

##- 1.a coropleta: --------
#- creo q lo mejor es hacer una coropleta 


##- 1.b histograma: --------
#- tb puedo hacer tb un histograma like this:

ggplot(zz, aes(y = n, x = reorder(region, n, median), color = region)) +
  geom_boxplot() +
  geom_jitter(aes(y = n, x = reorder(region, n, median))) +
  geom_text(aes(label = iso3_code), position = position_jitter())


#- quiero que geom_text() solo afecte a los outliers
ff_findoutlier <- function(x) {
  return(x < quantile(x, .25) - IQR(x) | x > quantile(x, .75) + IQR(x))
  #return(x > mean(x, na.rm = TRUE) + 1*sd(x, na.rm = TRUE)  | x < mean(x, na.rm = TRUE) - 1*sd(x, na.rm = TRUE) )
}

zz1 <- zz %>% 
  group_by(region) %>% 
  mutate(n_out = ff_findoutlier(n)) %>% ungroup () %>% 
  filter(n_out == TRUE)

ggplot(zz, aes(y = n, x = reorder(region, n, median), color = region)) +
  geom_boxplot() +
  geom_jitter(aes(y = n, x = reorder(region, n, median))) +
  geom_text(data = zz1, 
            aes(label = iso3_code), 
            position = position_jitter())





findoutlier <- function(x) {
  return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
}

#Add a column to identify which participants are outliers
set.seed(0)
performance_tibble <- tibble(Perc_Correct = -rlnorm(30), Pt_ID=sample(1:3, 30, TRUE))

performance_tibble <- performance_tibble %>%
  mutate(outlier = ifelse(findoutlier(performance_tibble$Perc_Correct), Pt_ID, NA))

#Plot boxplot of %correct including outliers labelled with Pt_ID
ggplot(performance_tibble, aes(y=Perc_Correct, x=1)) + geom_boxplot(outlier.colour= "red")+
  geom_text(aes(label=outlier), nudge_x=0.01) +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x= element_blank(),
        axis.title.x = element_blank())



# Method 1: Using IQR calculation
ggplot(zz, aes(y = n, x = reorder(region, n, median), color = region)) +
  geom_boxplot(outlier.colour = "red") +
  geom_jitter(aes(y = n, x = reorder(region, n, median))) +
  geom_text_repel(
    data = function(x) {
      # Calculate IQR and outlier thresholds
      stats <- boxplot.stats(x$n)$stats
      outliers <- x[x$n < (stats[2] - 1.5 * (stats[4] - stats[2])) | 
                      x$n > (stats[4] + 1.5 * (stats[4] - stats[2])), ]
      outliers
    },
    aes(label = iso3_code)
  )


#- 2. nº de obs x delito -------------------------------------------------------
#- quiero ver q delito tiene más observaciones 
zz <- df %>% 
  select(iso3_code, region, country, year, tipo_delito, tasa_100mil) %>% 
  group_by(tipo_delito) %>% 
  count() %>% 
  arrange(desc(n))


#- 3. nº de obs x delito y año -------------------------------------------------
#- quiero ver q delito tiene más observaciones x año
zz <- df %>% 
  select(iso3_code, country, year, tipo_delito, tasa_100mil) %>% 
  group_by(tipo_delito) %>% 
  mutate(nn_total = n()) %>% ungroup() %>% 
  group_by(tipo_delito, year) %>% 
  mutate(nn_year = n()) %>% ungroup() %>% 
  select(-iso3_code, -country, -tasa_100mil) %>% 
  distinct(tipo_delito, year, nn_total, nn_year) %>% 
  arrange(year) %>% 
  pivot_wider(names_from = year, values_from = nn_year) 


#- 3. nº de obs x pais y delito ------------------------------------------------
zz <- df %>% 
  select(iso3_code, country, year, tipo_delito, tasa_100mil) %>% 
  group_by(iso3_code, country) %>% 
  mutate(nn_total = n()) %>% ungroup() %>% 
  group_by(iso3_code, country, tipo_delito) %>% 
  mutate(nn_delito = n()) %>% ungroup() %>% 
  select(-year, -tasa_100mil) %>% 
  distinct() %>% 
  pivot_wider(names_from = tipo_delito, values_from = nn_delito) 

##- 2.a


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
