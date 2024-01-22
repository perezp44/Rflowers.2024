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

#- hay q pasar la df_table a formato ancho (2 posibilidades)
df_t1 <- df_table %>% tidyr::pivot_wider(names_from = year, values_from = rank_1)
df_t2 <- df_table %>% tidyr::pivot_wider(names_from = ine_muni.n, values_from = rank_1)

#- hay muchos pkgs para tablas
DT::datatable(df_t1)
gt::gt(df_t2)

#- mejoramos las tablas
#- DT: https://rstudio.github.io/DT/
DT::datatable(df_t1, filter = 'top', extensions = "Scroller",
              class = 'cell-border stripe',
              options = list(autoWidth = TRUE,deferRender = TRUE,
                             scroller = TRUE, scrollY = 450 ))

#- gt: https://gt.rstudio.com/
#- gallery: https://community.rstudio.com/c/table-gallery/64
#- ejemplo: https://jelloque.github.io/trabajo_BigData/#5_LOS_CICLISTAS
#- otro: https://twitter.com/irg_bio/status/1486291227255451648?ref_src=twsrc%5Etfw%7Ctwcamp%5Etweetembed%7Ctwterm%5E1486291227255451648&ref_url=file%3A%2F%2F%2Fhome%2Fpjpv%2FEscritorio%2Fmys_COSAS%2Fmy_learn_RR%2Flearn_ggplot2%2Fejemplos_ggplot2_2022.html
gt::gt(df_t2) %>%
  gt::tab_header(title = "Capitales más pobladas",
    subtitle = glue::glue("{min(df$year)} to {max(df$year)}"))





#- rank plot -------------------------------------------------------------------
#- hagamos un gráfico con los rankings
#- Gallery ggplot2: https://r-graph-gallery.com/
#- Parallel plot: https://r-graph-gallery.com/parallel-plot.html
#- 2 ejemplos:
#- https://github.com/fblpalmeira/highest-dwelling-mammal/blob/main/data/slope.R
#- https://github.com/bydata/30DayChartChallenge/blob/main/2022/06/06-owid-working-hours.R


#- plot-rank básico ------------------------------------------------------------
p0 <- df %>%
  ggplot(aes(x = year, y = rank_1, group = ine_muni.n)) +
  geom_line() +
  geom_point()
p0

p1 <- df_table %>%
  ggplot(aes(x = year, y = rank_1, group = ine_muni.n)) +
  geom_line(aes(color = ine_muni.n), size = 1)  +
  geom_point(aes(color = ine_muni.n), size = 2)
p1

p1 <- p1 +
  labs(title = "Ranking capitales provincia",
       subtitle = glue::glue("{min(df$year)} to {max(df$year)}")) +
  theme_minimal()

p1 <- p1 + scale_y_reverse() #-   scale_y_continuous(30:1)
p1


my_color <- "blue"
my_color <- "#E6F0FF"

p1 <- p1 +
  theme(plot.background = ggplot2::element_rect(fill = my_color, color = NA),
        panel.background = ggplot2::element_rect(fill = my_color,color = NA),
        legend.background = ggplot2::element_rect(fill = my_color, color = NA))

p1



#- mejoramos
p1 <- p1 + scale_x_continuous(breaks = unique(df_table$year), labels = unique(df_table$year))
p1 <- p1 + scale_x_continuous(breaks = c(1842, 1860, 1900, 1950, 1991, 2011))

p1

#- con GGally
df_t1 <- df_table %>% tidyr::pivot_wider(names_from = year, values_from = rank_1)
GGally::ggparcoord(df_t1, # data
                   columns = 2:19, # plotting first 3 columns
                   scale = "globalminmax",
                   groupColumn = 1) # target parameter




#- 2º plot ---------------------------------------------------------------------
#- veamos que ciudad ha crecido más (en %) en cada periodo (entre dos censos)
zz <- df %>%
  select(year, ine_muni, ine_muni.n, poblacion, rank_1) %>%
  group_by(ine_muni.n) %>%
  arrange(year) %>%
  dplyr::mutate(pob_incre = poblacion - lag(poblacion)) %>%
  dplyr::mutate(pob_percent = pob_incre / lag(poblacion) * 100) %>%
  dplyr::mutate(qq_years = year - lag(year)) %>%
  dplyr::mutate(pob_percent_acu = cumsum(coalesce(pob_incre,0)/first(poblacion)*100)) %>%
  dplyr::mutate(qq_years_acu = cumsum(ifelse(is.na(qq_years), 0, qq_years))) %>%
  dplyr::mutate(crec_percent_medio = pob_percent_acu/qq_years_acu)


zzx <- zz %>% filter(ine_muni.n == "Madrid")


#- 3er plot --------------------------------------------------------------------
#- capitales más importantes para la provincia (por su % de población)

df_prov <- pjpv.datos.2022::ine_censos_historicos_prov %>%
  mutate(poblacion = pob_de_derecho, .after = pob_de_derecho) %>%
  mutate(poblacion = ifelse(is.na(poblacion), pob_de_hecho, poblacion)) %>%
  mutate(poblacion = as.numeric(poblacion)) %>%
  select(ine_prov, year, poblacion) %>%
  rename(pob_prov = poblacion)

zz <- left_join(df, df_prov) %>% relocate(pob_prov, .after = poblacion)
zz <- zz %>% mutate(percent_capi = poblacion/pob_prov*100, .after = pob_prov)

feo <- zz %>% filter(ine_prov == "44")
names(df_prov)

ggplot(data = zz, aes(x = year, y = percent_capi, color = ine_prov.n, group = ine_prov.n)) + geom_line()

p <- ggplot(data = zz, aes(x = year, y = percent_capi, group = ine_prov.n)) + geom_line()
p + facet_wrap(vars(ine_prov.n), nrow = 8, ncol = 8)        # graf x filas y columnas







#- TO-DO: ----------------------------------------------------------------------
#- visualización: rankings
#- visualización: heatmap con tasa de crecimiento
#- visualización lineas de tt (crecimiento)

#- mapa????

#- modelo: crecimiento en ff. de inicio (para ver un MRLM)????

#- https://twitter.com/aarora79/status/1385624265995825154?ref_src=twsrc%5Etfw%7Ctwcamp%5Etweetembed%7Ctwterm%5E1385624265995825154&ref_url=file%3A%2F%2F%2Fhome%2Fpjpv%2FEscritorio%2Fmys_COSAS%2Fmy_learn_RR%2Flearn_ggplot2%2Fbitacora_learn_ggplot2.html
#- heatmap: https://github.com/aarora79/30DayChartChallenge/blob/main/23-tiles/tiles.R
