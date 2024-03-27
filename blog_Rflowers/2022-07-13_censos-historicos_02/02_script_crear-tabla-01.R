library(tidyverse)

df <- pjpv.datos.2022::ine_censos_historicos #- censos históricos (1842-2011)

# TABLA GOOD del CENSO: 
tabla_0 <- df %>% drop_na(pob) %>% 
  group_by(year) %>% summarise(nn = n_distinct(ine_muni))

tabla_1 <- df %>% 
  filter(indicador %in% c("aparece", "desaparece")) %>% 
  count(year, indicador) %>% 
  pivot_wider(names_from = indicador, values_from = n) %>% 
  mutate(nn_netos = aparece - desaparece) %>% 
  add_case(year = 1842, .before = 1)

tabla_1a <- left_join(tabla_0, tabla_1) %>% 
  mutate(comprobacion = nn_netos) %>% 
  mutate(comprobacion = ifelse(year == "1842", first(nn), comprobacion)) %>% 
  mutate(comprobacion = cumsum(comprobacion) - nn)

tabla_censo_ok <- tabla_1a %>% select(-comprobacion)
names(tabla_censo_ok) <- c("Censo", "Nº municipios", "desaparecen", "aparecen", "Incremento")
tabla_censo_ok <- tabla_censo_ok %>% select(1, 2, "Incremento", everything())

table_ok <- tabla_censo_ok %>% 
  reactable::reactable(defaultPageSize = 18, theme = reactablefmtr::fivethirtyeight()) 

table_ok
