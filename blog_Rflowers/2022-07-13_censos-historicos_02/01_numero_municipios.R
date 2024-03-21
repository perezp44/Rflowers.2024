#- Para hacer las cosas de las q hablo en el post
#- tablas y gráficos de la evolución del nº de municipios 
#- con datos de los CENSOS y del PADRON

library(tidyverse)

#- censos históricos (1842-2011)
df <- pjpv.datos.2022::ine_censos_historicos
df_dicc <- pjpv.curso.R.2022::pjp_dicc(df)

zz <- df %>% filter(ine_muni.n.h == "Pancrudo") 

#- datos del padron (1996-1998-2021)
df_padron <- pjpv.datos.2022::ine_pob_mun_1996_2021
df_dicc_2 <- pjpv.curso.R.2022::pjp_dicc(df_padron)


#- Nº de municipios (evolución) ------------------------------------------------
zz_unique_censos <- df %>% distinct(ine_muni)          #- 11.910
zz_unique_padron <- df_padron %>% distinct(ine_muni)   #- 8.136

#- municipios q han existido alguna vez (11.926)
zz_union <- union(zz_unique_censos, zz_unique_padron)         #- 11.926

zz_intersect <- intersect(zz_unique_censos, zz_unique_padron) #-  8.120
zz_dif_1 <- setdiff(zz_unique_censos, zz_unique_padron)       #-  3.790
zz_dif_2 <- setdiff(zz_unique_padron, zz_unique_censos)       #-     16



#- el nº de muni en cada periodo -----------------------------------------------
#- en censo
tabla_0 <- df %>% drop_na(pob) %>% 
  group_by(year) %>% summarise(nn = n_distinct(ine_muni))
#- en padrón
tabla_0_padron <- df_padron %>% drop_na(poblacion) %>% 
  group_by(year) %>% summarise(NN = n_distinct(ine_muni))

#- junto tablas (de nº de municipios cada año)
zz <- full_join(tabla_0, tabla_0_padron)

#- 2001: xq hay distinto nº de munis en 2001 en censo(8.108) y padron(8.107)
zz_1 <- df %>% drop_na(pob) %>% dplyr::filter(year == 2001) %>% distinct(ine_muni) #- 8108 
zz_2 <- df_padron %>% filter(year == 2001) %>% distinct(ine_muni)                  #- 8107

zz_intersect <- intersect(zz_1, zz_2) #-  8.107
zz_union <- union(zz_1, zz_2)         #-  8.108
zz_dif_1 <- setdiff(zz_1, zz_2)       #-  1 (23905, Arroyo del Ojanco)
zz_dif_2 <- setdiff(zz_2, zz_1)       #-  0
#zz <- df_padron %>% filter(ine_muni == "23905")

#- 2011: en censo y padron tienen igual nº de ine_muni, pero no son iguales
zz_1 <- df %>% drop_na(pob) %>% dplyr::filter(year == 2011) %>% distinct(ine_muni) #- 8116      
zz_2 <- df_padron %>% filter(year == 2011) %>% distinct(ine_muni)                  #- 8116

zz_intersect <- intersect(zz_1, zz_2) #-  8.115
zz_union <- union(zz_1, zz_2)         #-  8.117
zz_dif_1 <- setdiff(zz_1, zz_2)       #-  1 (44139, Luco de Bordón)
zz_dif_2 <- setdiff(zz_2, zz_1)       #-  1 (44141, LLedo)


#- TABLA GOOD del CENSO: -------------------------------------------------------
#- tabla_1: qq aparecen y desaparecen cada año
tabla_0 <- df %>% drop_na(pob) %>% 
  group_by(year) %>% summarise(nn = n_distinct(ine_muni))

tabla_1 <- df %>% 
  filter(indicador %in% c("aparece", "desaparece")) %>% 
  count(year, indicador) %>% 
  pivot_wider(names_from = indicador, values_from = n) %>% 
  mutate(nn_netos = aparece - desaparece) %>% 
  add_case(year = 1842, .before = 1)

#- GOOD tabla censo
tabla_1a <- left_join(tabla_0, tabla_1) %>% 
  mutate(comprobacion = nn_netos) %>% 
  mutate(comprobacion = ifelse(year == "1842", first(nn), comprobacion)) %>% 
  mutate(comprobacion = cumsum(comprobacion) - nn)

tabla_censo_ok <- tabla_1a %>% select(-comprobacion)
names(tabla_censo_ok) <- c("Censo", "Nº municipios", "desaparecen", "aparecen", "Incremento")
tabla_censo_ok <- tabla_censo_ok %>% select(1, 2, "Incremento", everything())

tabla_censo_ok %>% 
  reactable::reactable(defaultPageSize = 18, theme = reactablefmtr::fivethirtyeight()) 


tabla_censo_ok %>% 
  reactable::reactable(defaultPageSize = 18, theme = reactablefmtr::nytimes()) 

#- GRAFICO del nº municipios (CENSO) -------------------------------------------
df_p <- tabla_1a %>% select(1,2)


p <- ggplot() + 
  geom_line(data = df_p, aes(x = year, y = nn), size = 1.5) +
  geom_label(data = df_p, aes(x = year, y = nn, label = nn)) +
  labs(title = "Evolución del número de municipios en España ",
       subtitle = "(Censos de población del INE)",
       x = "",
       y = "",
       caption = "Datos del INE. Visualización: @pjpv4444") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.position = "none") +
  #scale_x_discrete(expand = expansion(mult = c(0.04, .225))) +
  theme_minimal() + 
  theme(plot.title = element_text(size = 12, face = "bold"),
       legend.position = "none",
        text = element_text(family = "Ubuntu Regular", color = "#22211d"),
        plot.background = element_rect(fill = "ghostwhite", color = NA))

p


#- para guardar el plot --------------------------------------------------------
name_of_plot <- "plot_nn_muni_censo.png"
path_file <- rstudioapi::getSourceEditorContext()$path %>% 
  stringr::str_remove(., "/[^/]+$") %>% paste0(., "/imagenes/", name_of_plot)

# ggsave(p, filename = path_file,
#        device = "png", width = 32, height = 20, units = "cm")



#- TABLA GOOD del PADRÓN: ------------------------------------------------------
#- tabla_1: qq aparecen y desaparecen cada año
tabla_0 <- df_padron %>%
  group_by(year) %>% arrange(year) %>% 
  summarise(nn = n_distinct(ine_muni)) %>% 
  mutate(incremento = nn - lag(nn))

tabla_censo_ok <- tabla_0

tabla_censo_ok %>% 
  reactable::reactable(defaultPageSize = 25, theme = reactablefmtr::fivethirtyeight()) 


tabla_censo_ok %>% 
  reactable::reactable(defaultPageSize = 25, theme = reactablefmtr::nytimes()) 







#- GRAFICO del nº municipios (Padrón) -------------------------------------------
df_p <- tabla_censo_ok


p <- ggplot() + 
  geom_line(data = df_p, aes(x = year, y = nn), size = 1.5) +
  geom_label(data = df_p, aes(x = year, y = nn, label = nn)) +
  labs(title = "Evolución del número de municipios en España ",
       subtitle = "(Datos de población del Padrón, INE)",
       x = "",
       y = "",
       caption = "Datos del INE. Visualización: @pjpv4444") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.position = "none") +
  #scale_x_discrete(expand = expansion(mult = c(0.04, .225))) +
  theme_minimal() + 
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.position = "none",
        text = element_text(family = "Ubuntu Regular", color = "#22211d"),
        plot.background = element_rect(fill = "ghostwhite", color = NA))

p


#- para guardar el plot --------------------------------------------------------
name_of_plot <- "plot_nn_muni_padron.png"
path_file <- rstudioapi::getSourceEditorContext()$path %>% 
  stringr::str_remove(., "/[^/]+$") %>% paste0(., "/imagenes/", name_of_plot)

# ggsave(p, filename = path_file,
#        device = "png", width = 32, height = 20, units = "cm")






#- curiosidades ----------------------------------------------------------------
#- municipios mas grandes q desaparecen
curio_1 <- df %>% 
  group_by(ine_muni, ine_muni.n.h, ine_prov) %>% 
  mutate(qq_se_hh = lead(ine_muni.se.integra), .after = indicador) %>% 
  filter(lead(indicador) == "desaparece") %>% 
  #mutate(muni.22 = first(df$ine_muni.n.h[df$ine_muni == "28079"]), .after = indicador) 
  #- no me sale usando which which(df$ine_muni.n.h == df$qq_se_hh )
  mutate(muni.22 = first(df$ine_muni.n.h[df$ine_muni == first(qq_se_hh)]), .after = indicador) 



zz <- df %>% filter(ine_muni == "28508") 
curio_2 <- df %>% 
  group_by(ine_muni, ine_muni.n.h, ine_prov) %>% 
  filter(lag(indicador) == "aparece")
zz <- df %>% filter(ine_muni == "04902")



#- posibles post:
#- 1) Municipios desaparecidos y aparecidos en el t
#- posible mapa con los aparecidos en la época moderna con LAU2
#- importancia histórica de las provincias (en cuanto a población)
#- importancia histórica de las capitales de provincia


#- poblaciones homogeneas de capitales de provincia????
df_capi <- pjpv.datos.2022::ine_censos_historicos %>% 
  filter(capital_prov == "Sí") %>% 
  filter(year == 2011)
print(df_capi$ine_muni.n.h)
print(df_capi$anotaciones)
