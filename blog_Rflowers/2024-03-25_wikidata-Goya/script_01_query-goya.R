#- quiero coger todos los cuadros de Goya y hacer un collage
library(tidyverse)
library(rvest)
library(wikifacts) #- install.packages("wikifacts")
library(janitor)


#- TODO: -----------------------------------------------------------------------
#- molaria hacer:
#- 2: obtener las imágenes de los cuadros y hacer un collage




#- querie desde R


#- para poder hacerla desde R
my_query_goya <- '#obras de Goya
SELECT DISTINCT ?item ?itemLabel ?itemDescription  ?paisLabel ?ciudadLabel ?link_esp ?link_eng 
                ?ubicacionLabel ?long ?lat ?idGoya ?image  (year(?date) as ?year) 
WHERE {
    ?item  wdt:P170 wd:Q5432;   # items cuyo creador es Goya
           wdt:P276 ?ubicacion.  

    ?ubicacion wdt:P131 ?ciudad.
    ?ciudad wdt:P17 ?pais.
  
    ?ubicacion p:P625 ?coords .
    ?coords psv:P625 ?coordinate_node .
    ?coordinate_node wikibase:geoLatitude ?lat .
    ?coordinate_node wikibase:geoLongitude ?long .               
  
    OPTIONAL { ?item wdt:P7229 ?idGoya}       # id en Fundación Goya en Aragón
    OPTIONAL { ?item wdt:P18 ?image }         # con imagen
    OPTIONAL { ?item wdt:P571 ?date }         # con fecha de creacion
    #OPTIONAL { ?item wdt:P195/wdt:P361* ?coleccion }  # la colección

    OPTIONAL {  ?link_esp schema:about ?item;
         schema:isPartOf <https://es.wikipedia.org/>;} #- link en Wikipedia española
    OPTIONAL {  ?link_eng schema:about ?item;
         schema:isPartOf <https://en.wikipedia.org/>;} #- link en Wikipedia inglesa
 
    SERVICE wikibase:label { bd:serviceParam wikibase:language "es", "en", "de", "fr", "it" }
}

ORDER BY desc(?year)'

#- mando la consulta a wikidata ---------------------------
#df_goya <- wikifacts::wiki_query(my_query_goya)  #- hecha el 25-marzo-2024  1088 (rows)
rm(my_query_goya)

#- exporto fichero original de Wiidata
fecha <- as.character(Sys.Date())
nombre_fichero <- paste0("wikidata_obras_goya_", fecha, ".rds")
# rio::export(df_goya, here::here("datos", "orig_wikidata", nombre_fichero))


#- importo fichero (con datos originales de Wikidata)
my_ruta_datos_orig <- fs::dir_ls(here::here("datos", "orig_wikidata"))
df_orig <- rio::import(my_ruta_datos_orig)

#- borro todo
objetos_no_borrar <- c("df_orig", "df_orig_nested",  "df", "objetos_no_borrar")
rm(list = ls()[!ls() %in% objetos_no_borrar])

