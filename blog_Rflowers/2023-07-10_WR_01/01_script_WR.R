#- analizar datos de WR --------------------------------------------------------
library(tidyverse)
df_2023 <- pjpv.datos.2022::IAAF_WR_2023
names(df_2023)

#zz_100m <- aa %>% filter(prueba == "100-metres") %>% filter(genero == "men")



#- me quedo con menos pruebas
df <- df_2023 %>% 
  filter(!(prueba.f.9 %in% c("Relevos", "Combinadas",  "Race-walk",  "Carreras (raras)" ))) %>% 
  filter(!prueba.f %in% c("1000-metres", "2000-metres","one-mile", "3000-metres", "half-marathon", "5-kilometres", "10-kilometres", "50-kilometres" ,"100-kilometres"))

df <- df %>% mutate(prueba.f.9 = forcats::fct_drop(prueba.f.9))
df <- df %>% mutate(prueba.f = forcats::fct_drop(prueba.f))
unique(df$prueba.f)
levels(df$prueba.f)


#- análisis --------------------------------------------------------------------
#- récords más duraderos

aa1 <- df %>% 
  slice_max(longevidad_del_WR, n = 10) %>% 
  select(competitor, prueba, genero, longevidad_del_WR, fecha_marca, edad, tt_WR_to_today, iso.name.en_pjp, es_WR_actual)

#- WR más duraderos para cada (prueba, genero)
aa1 <- df %>% 
  group_by(genero, prueba) %>% 
  slice_max(longevidad_del_WR, n = 1) %>% 
  select(competitor, prueba, genero, longevidad_del_WR, fecha_marca, edad, tt_WR_to_today, iso.name.en_pjp, es_WR_actual)

  
#- WR más jóvenes
aa1 <- df %>% 
  #group_by(genero, prueba) %>% 
  slice_min(edad, n = 10) %>% 
  select(competitor, prueba, genero, longevidad_del_WR, fecha_marca, edad, tt_WR_to_today, iso.name.en_pjp, es_WR_actual)

aa1 <- df %>% 
  group_by(genero, prueba) %>% 
  slice_min(edad, n = 1) %>% 
  select(competitor, prueba, genero, longevidad_del_WR, fecha_marca, edad, tt_WR_to_today, iso.name.en_pjp, es_WR_actual)



#- WR más viejos
aa1 <- df %>% 
  #group_by(genero, prueba) %>% 
  slice_max(edad, n = 10) %>% 
  select(competitor, prueba, genero, longevidad_del_WR, fecha_marca, edad, tt_WR_to_today, iso.name.en_pjp, es_WR_actual)

aa1 <- df %>% 
  group_by(genero, prueba) %>% 
  slice_max(edad, n = 1) %>% 
  select(competitor, prueba, genero, longevidad_del_WR, fecha_marca, edad, tt_WR_to_today, iso.name.en_pjp, es_WR_actual)


#- mayores rebajas del WR en %
aa1 <- df %>% 
  #group_by(genero, prueba) %>% 
  filter(prueba.f.3 == "carreras") %>% 
  slice_min(mejora_WR_percent, n = 10) %>% 
  select(competitor, prueba, genero, longevidad_del_WR, mejora_WR, mejora_WR_percent, fecha_marca, edad, tt_WR_to_today, iso.name.en_pjp, es_WR_actual)

aa1 <- df %>% 
  filter(prueba.f.3 == "carreras") %>% 
    group_by(genero, prueba) %>% 
  slice_min(mejora_WR_percent, n = 1) %>% 
  select(competitor, prueba, genero, longevidad_del_WR, mejora_WR, mejora_WR_percent, fecha_marca, edad, tt_WR_to_today, iso.name.en_pjp, es_WR_actual)



#- Trocitos --------------------------------------------------------------------

zz <- df_2023 %>% filter(competitor == "Usain BOLT")
zz_100m <- df_2023 %>% filter(prueba == "100-metres") %>% filter(genero == "women")

dicc_zz <- pjpv.curso.R.2022::pjp_dicc(zz)
dicc_zz_100m <- pjpv.curso.R.2022::pjp_dicc(zz_100m)

uniques <- pjpv.curso.R.2022::pjp_valores_unicos(df_2022)


#- 

zz <- df %>% group_by(genero, prueba.f) %>% 
  summarise(min = min(edad, na.rm = TRUE), max = max(edad, na.rm = TRUE)) %>% 
  mutate(dif = max - min)
