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
#- simplemente dejo una copia en "00_cargar-datos_UNDOC.R_old_cortar"
#- con cálculos, plots etc .... q he de ir reutilizando


