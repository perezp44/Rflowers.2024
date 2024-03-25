#- quiero coger todos los cuadros de Goya y hacer un collage
library(tidyverse)
library(rvest)
library(wikifacts) #- install.packages("wikifacts")
library(janitor)


#- TODO: -----------------------------------------------------------------------
#- 2: obtener las imágenes de los cuadros y hacer un collage

#- importo fichero (con datos originales de Wikidata)
# my_ruta_datos_orig <- fs::dir_ls(here::here("datos", "orig_wikidata"))
my_ruta_datos_orig <-  fs::dir_ls(here::here("blog_Rflowers", "2024-03-25_wikidata-Goya", "datos", "orig_wikidata"))
df_orig <- rio::import(my_ruta_datos_orig)

#- borro todo
objetos_no_borrar <- c("df_orig", "df_orig_nested",  "df", "objetos_no_borrar")
rm(list = ls()[!ls() %in% objetos_no_borrar])

#- CURRANDO --------------------------------------------------------------------

##- ver Nesting --------------------------------------------------------------------
#- puede haber obras con varias rows xq tengan dos afotos, o estén en 2 colecciones etc... etc...
#- así que hay que verlo

rows_unicas_1 <- 0
rows_unicas_t <- 0
for(ii in 1:length(df_orig)){
  print(ii)
  vv_no_nest <- names(df_orig)[1:ii]
  vv_si_nest <- names(df_orig)[!(names(df_orig) %in% vv_no_nest)]
  df_orig_nest_tmp <- df_orig %>% nest(data = all_of(vv_si_nest)) #- df nested
  rows_unicas_t[ii] <- nrow(df_orig_nest_tmp)
  vv_no_nest <- names(df_orig)[c(1,ii)]
  vv_si_nest <- names(df_orig)[!(names(df_orig) %in% vv_no_nest)]
  df_orig_nest_tmp <- df_orig %>% nest(data = all_of(vv_si_nest)) #- df nested
  rows_unicas_1[ii] <- nrow(df_orig_nest_tmp) 
}


#- df_ver_nested te permite ver filas unicas. nn_1 es filas unicas al poner la primera variable y otra mas
df_ver_nested <- data.frame(variables = names(df_orig), nn_1 = rows_unicas_1, nn_t = rows_unicas_t)


#- las vv. q. no dan pb's son: 
df_ver_variables <- df_ver_nested %>% filter(nn_1 == min(nn_1, na.rm = TRUE)) %>% pull(variables)
df_ver_variables #- estna son las q no dan pb's no generan "duplicados"

#- ok. ya he visto q variables hay q hacer un nesting, ahora toca hacerlo
vv_no_nest <- df_ver_variables
vv_si_nest <- names(df_orig)[!(names(df_orig) %in% vv_no_nest)]

df_orig_nested <- df_orig %>% nest(data = all_of(vv_si_nest)) #- df nested
names(df_orig)

rm(list = ls()[!ls() %in% objetos_no_borrar])

#- Ok: 1.088 rows, pero solo 799 items únicos
 
#- me quedo con un df con 799 items únicos (los primeros) 
df <- df_orig %>% distinct(item, .keep_all = TRUE)

#- ver cuantos NA's
df %>% summarise_all(~sum(is.na(.)))


zz <- df %>% janitor::tabyl(ubicacionLabel) 


#- MAPA ------------------------------------------------------------------------
#- algunos cuadros. Por ejmeplo: https://www.wikidata.org/wiki/Q47533444  están en 3 ubicaciones  y por tanto, cuando recupero el pais salen 3 paises
zz <- df_orig %>% filter(item == "http://www.wikidata.org/entity/Q47533444")

#- Hay 7 ( 1 con 3 y 6 con 2) cuadros con varias ubicaciones

##- cuadros en varios museos ----
zz_varios_museos_id <- df_orig %>%  #- obtengo los id's de los cuadros que están en varios museos
  distinct(item, itemLabel, ubicacionLabel) %>% 
  group_by(item, itemLabel) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>% filter(n > 1)  %>% pull(1) #- 14 id's

# veo los cuadros q están en varios museos
zz_varios_museos <- df_orig %>% filter(item %in% zz_varios_museos_id) %>% 
  select(1:2, paisLabel, ciudadLabel, ubicacionLabel) %>% distinct()  #- 29 registros

gt::gt(zz_varios_museos)

##- quito esos 14 cuadros (q tienen ubicación en varios museos)
df_orig_x <- df_orig %>% filter(!item %in% zz_varios_museos_id)


##- cuadros en varios ciudades ----
#- ahora ya empiezo en df_orig_x
zz_varias_ciudades_id <- df_orig_x %>%  #- obtengo los id's de los cuadros que están en varios ciudades
  distinct(item, itemLabel, ciudadLabel) %>% 
  group_by(item, itemLabel) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>% filter(n > 1)  %>% pull(1) #- 29 id's

# veo los cuadros q están en varias ciudades
zz_varias_ciudades <- df_orig_x %>% filter(item %in% zz_varias_ciudades_id) %>% 
  select(1:2, paisLabel, ciudadLabel, ubicacionLabel) %>% distinct()  #- 61 registros

gt::gt(zz_varias_ciudades)

# quito los cuadros con varias ciudades
df_orig_x <- df_orig_x %>% filter(!item %in% zz_varias_ciudades_id) #- 958 rows
zz <- df_orig_x %>% janitor::tabyl(ciudadLabel)
#- cuantos id's unicos quedan
zz <- df_orig_x %>% distinct(item)  #- 754 , pero deberian ser 756 = 799 - (14 + 29)


##- cuadros en varios países ----
zz_varios_paises_id <- df_orig_x %>%  #- obtengo los id's de los cuadros que están en varios paises
  distinct(item, itemLabel, paisLabel) %>% 
  group_by(item, itemLabel) %>% summarise(n = n()) %>% 
  arrange(desc(n)) %>% filter(n > 1) %>% pull(1) #- no hay, asi q no quito nada

#- ya no me caliento el cap más con los duplicados

#- OK ya empiezo el análisis de las ubicaciones (habiendo quitado los duplicados)
#- En df_orig_x quedan 756 cuadros únicos, PERO 958 rows (puede ser x imagens etc)
#- ME QUEDO CON LA PRIMERA ROW DE CADA CUADRO

df_ubicaciones <- df_orig_x %>% distinct(item, .keep_all = TRUE) #-


#- MAPA
df_plot <- df_ubicaciones %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326)

mapview::mapview(df_plot)


#- tablas de uboicaciones

#- museos
zz_museos <- df_ubicaciones %>% janitor::tabyl(ubicacionLabel)

zz_unir_museos <- df_ubicaciones %>% distinct(ubicacionLabel, ciudadLabel, paisLabel)

zz_museos <- left_join(zz_unir_museos, zz_museos) %>% arrange(desc(n)) 

#- parece q si hay muchos Goya en Washgiton  https://www.nga.gov/collection-search-result.html?artist=Goya

# ciudades

zz_ciudades <- df_ubicaciones %>% 
  group_by(ciudadLabel, paisLabel) %>% summarise(n = n()) %>% arrange(desc(n))

zz_unir_ciudades <- df_ubicaciones %>% distinct(ciudadLabel, paisLabel) 

zz_ciudades <- left_join(zz_unir_ciudades, zz_ciudades)


#- paises

zz_paises <- df_ubicaciones %>% 
  group_by(paisLabel) %>% summarise(n = n()) %>% arrange(desc(n))



#- tabla con todos los cuadros

tabla_cuadros <- df_ubicaciones %>% 
  select(itemLabel, year, ubicacionLabel, ciudadLabel, paisLabel, image, link_esp, link_eng, image) %>% 
  arrange(year, paisLabel, ciudadLabel, ubicacionLabel)

DT::datatable(tabla_cuadros, rownames = FALSE, options = list(pageLength = 10))

#- el entierro de la sardina no puede ser de 1900 !!

#- IMAGENES - Collage  ---------------------------------------------------------

#- ver las que SI tienen imagen
df_con_img <- df_orig %>% 
  filter(!is.na(image)) %>% 
  distinct(item, .keep_all = TRUE) 



#- voy a hacer un collage con fotos de las obras de Goya
#- collage: https://github.com/VincentGuyader/collage
