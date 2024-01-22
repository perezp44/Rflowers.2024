# Post censos históricos
# Objetivo: ver que capitales de provincia estuvieron en algún momento entre las 10 más pobladas
# Datos: Análisis de CENSOS HISTÓRICOS (INE)
# https://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176998&menu=resultados&idp=1254735572981


library(tidyverse)

#- cargo censos históricos (capitales de provincia)
#df <- pjpv.curso.R.2022::ine_censos_historicos_capitales
df <- pjpv.datos.2022::ine_censos_historicos
df_dicc <- pjpv.curso.R.2022::pjp_dicc(df) #- hay 18 censos (1842-2011)
zz <- df %>% filter(ine_muni.n.h == "Pancrudo")


#- fusiono con datos de padrón 2021 --------------------------------------------
df_padron <- pjpv.curso.R.2022::ine_pob_mun_1996_2021 %>% 
  #filter(capital_prov == "Sí") %>% 
  filter(year == 2021) %>% filter(poblacion == "Total") %>% 
  select(- poblacion, - ine_muni.n.orig) %>% 
  rename(pob = values) %>% 
  select(ine_muni, year, pob)

#- quito municipios de 2021 que no estan en df (1842-2011)
df_padron <- df_padron %>% filter(ine_muni %in% unique(df$ine_muni))
#- pongo a df_padron las v. q van a hacer falta (ine_prov etc ....
zz <- df %>% select(ine_muni, ine_muni.n.h, ine_prov, ine_prov.n, ine_ccaa, ine_ccaa.n, capital_prov, capital_ccaa, year) %>% filter(year == 2011) %>% select(-year)
df_padron <- left_join(df_padron, zz)


#- fusiono censos y padrón OK --------------------------------------------------
df <- full_join(df, df_padron)
zz <- df %>% filter(ine_muni == "28079")
zz <- df %>% filter(ine_muni.n.h == "Pancrudo")


#- CALCULOS (empiezan) ---------------------------------------------------------
#- CALCULOS (empiezan) ---------------------------------------------------------

#- calculo rankings (para cada censo)
df <- df %>%
  #select(year, ine_muni, ine_muni.n, pob, ine_prov)  %>%
  group_by(year) %>%
  arrange(desc(pob), desc(lag(pob)) )  %>%
  mutate(rank_1 = 1:n(), .after = pob) %>%
  mutate(rank_2 = dplyr::dense_rank(desc(pob))) %>%
  ungroup() %>% 
  arrange(year)

zz <- df %>% filter(ine_muni.n.h == "Pancrudo")


#- calculo tb variaciones en el ranking
df <- df %>%
  group_by(ine_muni) %>%
  arrange(year) %>%
  mutate(rank_dif_1 = lag(rank_1) - rank_1, .after = rank_1) %>% 
  mutate(rank_max_dif_1 = max(rank_1, na.rm = TRUE) - min(rank_1, na.rm = TRUE), .after = rank_dif_1) %>% ungroup()
  
zz <- df %>% filter(ine_muni.n.h == "Teruel")



#- calculo de crecimientos etc....
df <- df %>%
  group_by(ine_muni) %>%
  arrange(year) %>%
  dplyr::mutate(pob_incre = pob - lag(pob)) %>%
  dplyr::mutate(pob_percent = pob_incre / lag(pob) * 100) %>%
  dplyr::mutate(qq_years = year - lag(year)) %>%
  dplyr::mutate(pob_percent_acu = cumsum(coalesce(pob_incre,0)/first(pob)*100)) %>%
  dplyr::mutate(qq_years_acu = cumsum(ifelse(is.na(qq_years), 0, qq_years))) %>%
  dplyr::mutate(crec_percent_medio = pob_percent/qq_years) %>% 
  dplyr::mutate(crec_percent_medio_acu = pob_percent_acu/qq_years_acu) %>% 
  relocate(c(ine_prov, ine_prov.n), .after = last_col()) %>% ungroup

zz <- df %>% filter(ine_muni.n.h == "Teruel")


#- veamos q municipios han estado alguna vez entre las 20 más pobladas
qq_num <- 20
zz <- df %>%
  filter(rank_1 <= qq_num) %>%
  distinct(ine_muni, ine_muni.n.h) %>% pull(ine_muni.n.h) %>% print()  #- 17 ciudades (cuidado con filtrar x nombre)

zz <- df %>%
  filter(rank_1 <= qq_num) %>%
  distinct(ine_muni, ine_muni.n.h)  #- 17 ciudades (cuidado con filtrar x nombre)
print(zz$ine_muni.n.h)

#- filtremos, quedémonos con los datos de esas 14 ciudades
df_table <- df %>% filter(ine_muni %in% zz$ine_muni)   #- ok


#- Tablas ----------------------------------------------------------------------
#- nos quedamos con las 3 v. q queremos mostrar
df_table <- df_table %>%
  select(ine_muni.n.h, year, rank_1)

#- hay q pasar la df_table a formato ancho (2 posibilidades)
df_t1 <- df_table %>% tidyr::pivot_wider(names_from = year, values_from = rank_1)
df_t2 <- df_table %>% tidyr::pivot_wider(names_from = ine_muni.n.h, values_from = rank_1)

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




#- 1er plot de rank ------------------------------------------------------------
#- hagamos un gráfico con los rankings
#- Gallery ggplot2: https://r-graph-gallery.com/
#- Parallel plot: https://r-graph-gallery.com/parallel-plot.html

#- https://github.com/fblpalmeira/highest-dwelling-mammal/blob/main/data/slope.R
#- good: https://github.com/bydata/30DayChartChallenge/blob/main/2022/06/06-owid-working-hours.R

#- haz tb este para rankings: <https://twitter.com/ThomIvar/status/1518289166026616832
library(ggtext)




#- calculetes para gráfico
#- paletas: https://github.com/EmilHvitfeldt/r-color-palettes/blob/master/type-sorted-palettes.md#diverging-color-palettes
library(RColorBrewer)
my_color_bg <- "#E6F0FF"
vv_eje_x <- unique(df_table$year) [-c(2,6)]
my_cc <- c(RColorBrewer::brewer.pal(name="Dark2", n = 8), RColorBrewer::brewer.pal(name="Paired", n = 6))
my_colors <- c(my_cc, my_cc, my_cc, my_cc)

nn_cols <- 52
my_colors <- colorRampPalette(brewer.pal(8, "Set2"))(nn_cols)

scales::show_col(my_colors)
df_para_labels <- df_table %>% filter(year %in% c(1842, 1860, 1887, 1910, 1930, 1950, 1970, 1991, 2011, 2021))

df_para_pob <- df %>% filter(year %in% c(1842, 2211)) %>%
  select(ine_muni.n.h, year, rank_1, pob) %>%
  mutate(ine_muni.n.h = case_when(
    ine_muni.n.h == "Castelló de la Plana" ~ "Castellón",
    ine_muni.n.h == "Alicante/Alacant" ~ "Alicante",
    ine_muni.n.h == "Palmas de Gran Canaria, Las" ~ "Las Palmas",
    ine_muni.n.h == "Pamplona/Iruña" ~ "Pamplona",
    ine_muni.n.h == "Donostia/San Sebastián" ~ "Donostia",
    ine_muni.n.h == "Vitoria-Gasteiz" ~ "Vitoria",
    ine_muni.n.h == "Santa Cruz de Tenerife" ~ "Tenerife",
    TRUE ~ as.character(ine_muni.n.h) )) %>%
  group_by(ine_muni.n.h) %>%
  arrange(year) %>%
  mutate(crec_pob = (pob - lag(pob))/lag(pob)*100) %>%
  mutate(crec_pob_x = scales::number(crec_pob,  big.mark = ".", decimal.mark = ",")) %>%
  ungroup() %>%
  mutate(pob_x = scales::number(pob,  big.mark = ".", decimal.mark = ",")) %>%
  mutate(muni_label_pob = if_else(year != 1842,
                                  glue::glue("{ine_muni.n.h} ({pob_x})"),
                                  glue::glue("({pob_x}) {ine_muni.n.h}"))) %>%
  mutate(muni_label_pob_2 = if_else(year != 1842,
                                  glue::glue("{ine_muni.n.h} (Pob: {pob_x}) (Crec: {crec_pob_x} %)"),
                                  glue::glue("(Pob: {pob_x}) {ine_muni.n.h}")))



#- p1: ranking plot ----
p1 <- df %>%
  ggplot(aes(x = year, y = rank_1, group = ine_muni.n.h)) +
  geom_line(aes(color = ine_muni.n.h), size = 0.6, alpha = 0.8)  +
  geom_point(aes(color = ine_muni.n.h), size = 2.9, fill = "white", alpha = 0.2) +
  #- escalas X e Y
  scale_x_continuous(breaks = vv_eje_x, labels = vv_eje_x) +
  scale_y_reverse(breaks = c(1, 10, 20, 30, 40, 50, 52),
                  limits = c(max(df$rank_1) + 0.5 , 1)) +
  #- escala de colores
  scale_color_manual(values = my_colors) +
  #- títulos
  labs(title = 'Ranking de capitales con más población',
       subtitle = glue::glue("Censos de población ({min(df$year)} a {max(df$year)})"),
       caption = 'Datos del INE (Alteraciones de los municipios en los Censos de Población desde 1842). Visualización @pjpv4444',
       x = '', y = 'Ranking') +
  #- anotaciones (valores del ranking)
  geom_text(data = df, fontface = "bold",
            aes(x = year, y = rank_1, label = rank_1), color = "black",
            size = 1.9, vjust = 0, hjust = 0, nudge_x = -0.62, nudge_y = -0.27) +
  #- anotaciones (ppio y final)
 geom_text(data = df_para_pob %>% filter(year == min(year)), fontface = "bold",
            aes(x = year, y = rank_1, label = muni_label_pob), color = "black",
            size = 2, vjust = 0, hjust = 0, nudge_x = -13, nudge_y = 0) +
 geom_text(data = df_para_pob %>% filter(year == max(year)), fontface = "bold",
            aes(x = year, y = rank_1, label = muni_label_pob_2), color = "black",
            size = 2, vjust = 0, hjust = "right", nudge_x = 20, nudge_y = 0) +
  #- anotaciones con ggrepel
  # ggrepel::geom_text_repel(data = df_table %>% filter(year == min(year)),
  #                          aes(label = paste0(ine_muni.n.h)),
  #                          hjust = "left",
  #                          fontface = "bold",
  #                          size = 1.5,
  #                          nudge_x = -.6,
  #                          direction = "y") +
  #- theme
  theme_minimal() +
  theme(plot.background = ggplot2::element_rect(fill = my_color_bg, color = NA),
        panel.background = ggplot2::element_rect(fill = my_color_bg,color = NA),
        legend.background = ggplot2::element_rect(fill = my_color_bg, color = NA)) +
  theme(legend.position = "none") +   #theme(caption.position = c(.9,.2))
  theme(text = element_text(family = "Varta"),
        plot.title = element_text(size = 16, face = "bold", margin = margin(t = 10)),
        plot.subtitle = element_text(lineheight = .35, size = 13, margin = margin(t=10,b=15)),
        plot.caption = element_text(size = 8, lineheight = .35,hjust = 0),
        axis.text = element_text(size = 7)) +
  theme(axis.text.x = element_markdown(size = 6.8, face = "bold")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(panel.background = element_blank()) +
  geom_vline(xintercept = unique(df_table$year), size = 0.2, linetype = 2, colour = "black", alpha = 0.2) +
  theme(plot.caption = element_text(face = "italic", hjust = 1)) +
  theme(plot.margin = margin(0.5, r = 0.25 , 0.25, l = 0.25, 'cm'))

p1






#- 2º plot de ranks ------------------------------------------------------------
#- https://twitter.com/ThomIvar/status/1518289166026616832
#- https://twitter.com/jburnmurdoch/status/1250538655707430913
library(ftplottools) #- remotes::install_github("Financial-Times/ftplottools")

df_para_labels <- df_table %>% filter(year %in% c(1842, 1860, 1887, 1910, 1930, 1950, 1970, 1991, 2011))
p <- ggplot(df_table, aes(x = year, y = rank_1, color = ine_muni.n.h)) +
  geom_path(data = df_table, aes(x = year, y = rank_1, group = ine_muni.n.h),
            color = ft_colors('black-20'), size = 0.4) +
  geom_path(color = ft_colors('oxford-60'), size = 0.8) +
  #geom_point(data = readRanking(29), color = ft_colors('oxford-60'), size = 1.5) +
  geom_point(data = df_table, color = ft_colors('oxford-60'), size = 1.5) +
  geom_text(data = df_para_labels,
            aes(x = year, y = rank_1, label = rank_1), color = "brown",
            size = 2.4, vjust = 0, hjust = 0, nudge_x = 0.6, nudge_y = -7) +
  scale_x_continuous(breaks = c(1842, 1900, 1950, 2011),
                     labels = c("1842", "1900", "1950",  2011)) +
  #scale_y_continuous(breaks = c(-1, -10, -18), labels = c('1', '10', '18')) +
  scale_y_reverse(breaks = c(1, 10, 20, 30, 40, 50), limits = c(50, 1)) +
  #scale_y_reverse(breaks = c(1, 25, 50, 75, 100), limits = c(100, 1)) +
  
  #scale_x_continuous(breaks = -30:1) +
  facet_wrap(~ ine_muni.n.h) +
  labs(title = 'Ranking de capitales con más población',
        subtitle = glue::glue("{min(df$year)} to {max(df$year)}"),
        caption = 'graphic: @pjpv4444\ninspiración: @ThomIvar\ntheme: Financial Times',
        x = 'Periodo', y = 'Ranking') +
  ftplottools::ft_theme() +
  theme(plot.background = element_rect(fill = ft_colors('paper'), color = ft_colors('paper')),
        strip.text = element_text(hjust = 0, color = ft_colors('oxford-60'), face = 'bold'),
        plot.margin = margin(1, 1, 0.25, 1, 'cm'),
        axis.text.y = element_text(angle = 0, hjust = 0, size = 7),
        axis.text.x = element_text(angle = 0, hjust = 0.01, vjust = 0.21, size = 7),
        plot.caption = element_text(size = 6, color = ft_colors('black-30')))
p

