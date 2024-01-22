#- analizar datos de WR --------------------------------------------------------
library(tidyverse)
df_2022 <- pjpv.datos.2022::IAAF_WR_2022
df_2022 <- df_2022 %>% dplyr::relocate(fecha_marca , .after = nat)
names(df_2022)
unique(df_2022$prueba.f.9)

df <- df_2022 %>% filter(!(prueba.f.9 %in% c("Relevos", "Combinadas",  "Race-walk",  "Carreras (raras)" )))
df_1 <- df %>% filter(prueba.f.3 == "carreras")
df_2 <- df %>% filter(prueba.f.3 == "saltos y lanzamientos")

zz <- df_2022 %>% filter(competitor == "Usain BOLT")
zz_100m <- df_2022 %>% filter(prueba == "100-metres") %>% filter(genero == "men")

dicc_zz <- pjpv.curso.R.2022::pjp_dicc(zz)
dicc_zz_100m <- pjpv.curso.R.2022::pjp_dicc(zz_100m)

uniques <- pjpv.curso.R.2022::pjp_valores_unicos(df_2022)


#- 

zz <- df %>% group_by(genero, prueba.f) %>% 
  summarise(min = min(edad, na.rm = TRUE), max = max(edad, na.rm = TRUE)) %>% 
  mutate(dif = max - min)
