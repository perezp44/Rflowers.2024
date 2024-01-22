#- Objetivo: ver que capitales de provincia estuvieron en algún momento entre las 10 más pobladas (periodo: 1842-2011)
#- Datos: Análisis de CENSOS HISTÓRICOS (INE)
#- https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176998&menu=resultados&idp=1254735572981


library(tidyverse)

#- cargo censos históricos (capitales de provincia)
df <- pjpv.curso.R.2022::ine_censos_historicos_capitales

df_dicc <- pjpv.curso.R.2022::pjp_dicc(df) #- hay 18 censos (1842-2011)

#- selecciono las v. q me interesan
df <- df %>%
  select(year, ine_muni, ine_muni.n,
         pob_de_hecho, pob_de_derecho, ine_prov, ine_prov.n)



#- hay un pb: algunos censos (1857 y 1860) no tienen pob_de_derecho
df <- df %>%
  mutate(poblacion = pob_de_derecho, .after = pob_de_derecho) %>%
  mutate(poblacion = ifelse(is.na(poblacion), pob_de_hecho, poblacion))

#- calculo rankings (para cada censo)
df <- df %>%
  group_by(year) %>%
  arrange(year, desc(poblacion)) %>%
  mutate(rank_0 = 1:52) %>%
  mutate(rank_1 = 1:length(poblacion)) %>%
  mutate(rank_2 = 1:n()) %>%
  mutate(rank_3 = dplyr::dense_rank(desc(poblacion))) %>%
  ungroup()

#- veamos q ciudades han estado alguna vez entre las 10 más pobladas
zz <- df %>%
  filter(rank_1 <= 10) %>%
  distinct(ine_muni, ine_muni.n) %>% pull(ine_muni.n) %>% print()  #- 14 ciudades (cuidado con filtrar x nombre)

#- filtremos, quedémonos con los datos de esas 14 ciudades
df_table <- df %>% filter(rank_1 <= 10)         #- sí, pero ....
df_table <- df %>% filter(ine_muni.n %in% zz)   #- ok


#- Tablas ----------------------------------------------------------------------
#- nos quedamos con las 3 v. q queremos mostrar
df_table <- df_table %>%
  select(ine_muni.n, year, rank_1)



#- RANK PLOTS ------------------------------------------------------------------
#- hagamos un gráfico con los rankings
#- Gallery ggplot2: https://r-graph-gallery.com/
#- Parallel plot: https://r-graph-gallery.com/parallel-plot.html
#- 2 ejemplos:
#- https://github.com/fblpalmeira/highest-dwelling-mammal/blob/main/data/slope.R
#- https://github.com/bydata/30DayChartChallenge/blob/main/2022/06/06-owid-working-hours.R
