#- datos crimen Eurostat
library(tidyverse)

#- ya descargué y arreglé los datos (el 10 de marzo de 2025)



#- COMENTARIOS  ----------------------------------------------------------------
#- tt_1 (8.869x7): 21 categorías de crímenes (2008-2022) y 41 "países" (FRA, England)
#- tt_2 (125.008x7): 7 tipos de delitos (2008-2022) (2008-2022) y hasta NUTS3; (FRA, nada de UK ni England); ESP tiene 89 filas (ES, 1, CC:AA, provincias)

#- tt_1 y tt_2 se pueden juntar 5 tipos de delitos
#-  pero al final no me merece la pena fusionarlas

#- tt_3 (3.532x7): 7 categorías de delitos (1993-2007) y 59 "países": está UK pero NO esta England; está FRA y metropolitan FRA
#- no tenian la tasa, asi que tuve q construirlas buscando datos de población: "demo_pjan"
#- en los datos de poblacion está UK, pero no están England, Scothland y NI, así que no puedo calcular tasas de esos 3

# fusiono tt_1 con tt_3 para tener datos de los países desde 1993; pero solo voy a poder fusionar 4 tipos de delitos, 3 no voy a poder

#- tt_4 (21.310x7): 7 categorías de delitos (2008-2022), 318 entidades territoriales de 35 países (UK nada)
#- las entidades son CIUDADES: para ESP tenemos 25 filas:
#- ES, 23 ciudades y finalemente "Non-metropolitan regions in Spain"

#- tt_5: son grandes ciudades y en ESP solo esta Madrid (NO ME INTERESA)

#- tt_6 (641x7): Population reporting occurrence of crime, violence or vandalism in their area by poverty status: 
#- (2003-2023); 43 paises; % de gente q reporta violencia (total, above y below 60% of median de la equivalised income)
#- esta el TOTAL, el % de gente q reporta Above and below 60% of median equivalised income

#- IMPORTAR --------------------------------------------------------------------
#- importo los datos (solo hay un fichero .Rdata)

df <- pjpv.pkg.datos.2024::crime_1_eurostat_paises_2008_22
df <- pjpv.pkg.datos.2024::crime_2_eurostat_nuts3_2008_22
df <- pjpv.pkg.datos.2024::crime_3_eurostat_paises_1993_07
df <- pjpv.pkg.datos.2024::crime_4_eurostat_ciudades_2008_22
df <- pjpv.pkg.datos.2024::crime_6_eurostat_reporting_2003_23
df <- pjpv.pkg.datos.2024::crime_1_3_eurostat_paises_1993_22

#----------
df_aa <- pjpv.curso.R.2022::pjp_unique_values(df, truncate = FALSE)
df_bb <- pjpv.curso.R.2022::pjp_valores_unicos(df, nn = 1000)


zz <- df %>% filter(stringr::str_starts(metroreg_code, "ES")) %>% distinct(metroreg, metroreg_code)

names(df)

#- RANKINGS --------------------------------------------------------------------

#- nº observaciones ------------------------------------------------------------
#- quiero ver en q año y delito  hay más observaciones
#- 2023 no hay datos; en 2022 tb hay pocos datos (salvo en homicidio)
zz <- df %>% 
  select(metroreg_code, metroreg, time, iccs_code, tasa_100mil) %>% 
  group_by(time, iccs_code) %>% 
  filter(!is.na(tasa_100mil)) %>% 
  count() %>% 
  tidyr::pivot_wider(names_from = time, values_from = n)

gt::gt(zz)
DT::datatable(zz)
#- RANKING's -------------------------------------------------------------------
#- ranking de Spain en el mundo cada año

tt_ranking <- df %>% 
  #filter(region == "Europe & Central Asia") %>% 
  #dplyr::filter(!is.na(!!sym(my_table))) %>% 
  # tidyr::drop_na(my_table)
  dplyr::filter(!is.na(tasa_100mil)) %>% 
  group_by(time, iccs_code) %>%
  mutate(NN = n(), .after = tasa_100mil) %>%    #- cuantos hay cada año y categoría de delito
  arrange(desc(tasa_100mil)) %>% 
  mutate(rank = row_number(), .after = tasa_100mil) %>% 
  mutate(rank_normalizado = rank/NN, .after = tasa_100mil) %>% 
  mutate(percentil = (rank-1)/(NN-1), .after = tasa_100mil) %>%              #- Percentil del Ranking
  mutate(z_score = (rank - mean(rank))/sd(rank), .after = tasa_100mil) %>%   #- Z-Score del Ranking
  mutate(decil = ntile(rank, 10), .after = tasa_100mil) %>%                  #- Transformación en Deciles
  #filter(time == 2021) %>% 
  #filter(iso3c == "ESP") %>% 
  identity()


my_vv <- df_bb$iccs_code[1]


tt_ranking_2019 <- tt_ranking %>% 
  filter(time == 2019) %>% 
  filter(iccs_code == my_vv) %>% 
  ungroup() %>% identity()

tt_ranking_spain <- tt_ranking %>% 
  filter(metroreg == "Madrid") %>% 
  filter(time == 2021) %>% 
  mutate(iccs_code_copy = iccs_code) %>% 
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

