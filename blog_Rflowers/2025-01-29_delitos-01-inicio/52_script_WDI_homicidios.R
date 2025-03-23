#- https://datageeek.com/2024/02/28/homicide-rates-from-gender-perspective-understanding-with-radar-chart-and-bootstrap-intervals/
#- buscar datos World Bank : https://data.worldbank.org/indicator?tab=all

library(tidyverse)
library(WDI)

#- Catálogo del World Bank
catalogo <- WDI::WDI_data   #- devuelve una lista con dos elementos
series <- WDI::WDI_data %>% .[[1]] #- catalogo de series: 5 sobre robbery; 9 sobre homicidio intencionado
paises <- WDI::WDI_data %>% .[[2]]   #- catalogo de países


homicidio <- WDI::WDIsearch("homicide")
population <- "SP.POP.TOTL"

my_table <- "VC.IHR.PSRC.P5"  #- Intentional homicides (per 100,000 people)
#- Intentional homicides are estimates of unlawful homicides purposely inflicted as a result of domestic disputes, interpersonal violence, violent conflicts over land resources, intergang violence over turf or control, and predatory violence and killing by armed groups. Intentional homicide does not include all intentional killing; the difference is usually in the organization of the killing. Individuals or small groups usually commit homicide, whereas killing in armed conflict is usually committed by fairly cohesive groups of up to several hundred members and is thus usually excluded.
#- World Development Indicators
#- UN Office on Drugs and Crime's International Homicide Statistics database.
#- https://databank.worldbank.org/metadataglossary/world-development-indicators/series/VC.IHR.PSRC.P5

my_table <- "VC.IHR.PSRC.P5"  #- Intentional homicides (per 100,000 people)
df  <- WDI(indicator = my_table, extra = TRUE) %>% as_tibble() 
dfx <- df %>% filter(iso3c == "ESP")
dfx <- df %>% filter(year == "2021")


df_pop <- WDI(indicator = population, extra = TRUE) %>% as_tibble() 
zzzz <- df_pop %>% filter(iso3c == "ESP")


#- ranking de españa en el mundo cada año

tt_ranking <- df %>% 
  #filter(region == "Europe & Central Asia") %>% 
  #dplyr::filter(!is.na(!!sym(my_table))) %>% 
  # tidyr::drop_na(my_table)
  dplyr::filter(!is.na(VC.IHR.PSRC.P5)) %>% 
  group_by(year) %>%
  mutate(NN = n(), .after = my_table) %>%    #- cuantos hay cada año
  arrange(desc(VC.IHR.PSRC.P5)) %>% 
  mutate(rank = row_number(), .after = my_table) %>% 
  mutate(rank_normalizado = rank/NN, .after = my_table) %>% 
  mutate(percentil = (rank-1)/(NN-1), .after = my_table) %>%              #- Percentil del Ranking
  mutate(z_score = (rank - mean(rank))/sd(rank), .after = my_table) %>%   #- Z-Score del Ranking
  mutate(decil = ntile(rank, 10), .after = my_table) %>%                  #- Transformación en Deciles
  #filter(year == 2021) %>% 
  #filter(iso3c == "ESP") %>% 
  identity()

tt_ranking_2019 <- tt_ranking %>% filter(year == 2019) %>% ungroup() %>% identity()
tt_ranking_esp <- tt_ranking %>% filter(iso3c == "ESP") %>% identity()

tt_ranking_EU <- tt_ranking %>% filter(iso3c %in% c("ESP", "FRA", "ITA")) %>%
  select(year, iso3c, rank) %>%
  tidyr::pivot_wider(names_from = iso3c, values_from = rank)

tt_ranking_1 <- tt_ranking %>% 
  filter(rank == 1)

tt_ranking_venezuela <- tt_ranking %>% 
  filter(iso3c == "VEN")

#- ahora hacer una mapa mundi con datos del 2021
#- https://www.r-graph-gallery.com/327-chloropleth-map-from-geojson-with-ggplot2.html

# ggplot2::ggplot(data = dfx) +
#   geom_sf(aes(fill = VC.IHR.PSRC.P5)) +
#   scale_fill_viridis_c() +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   labs(title = "Homicidios en España 2021",
#        fill = "Homicidios por 100,000 habitantes") +
#   theme(plot.title = element_text(hjust = 0.5))


#-------------------------------------------------------------------------------
#- MAPAS -----------------------------------------------------------------------
#- https://marcinstepniak.eu/post/interactive-choropleth-maps-with-r-and-tmap-part-i/

library(tmap)  #- https://mtennekes.github.io/tmap/reference/qtm.html

library(sf)
data(World)
xx <- left_join(World, tt_ranking_2019, World, by = c("iso_a3" = "iso3c"))

# coropletas
qtm(xx, fill = "VC.IHR.PSRC.P5")

# choropleth with way more specifications
qtm(xx, fill="VC.IHR.PSRC.P5", fill.n = 9, fill.palette = "div",
    fill.title = "Happy Planet Index", fill.id = "name", 
    style = "gray", format = "World", projection = "+proj=eck4")

# this map can also be created with the main plotting method,
#- qtm() es para hacer quick graphs. Generalmente se usa está sintaxis con tmap
tm_shape(xx) +
  tm_polygons("VC.IHR.PSRC.P5") +
  tm_layout(bg.color = "skyblue")


tm_shape(xx, projection = "+proj=eck4") +
  tm_polygons("VC.IHR.PSRC.P5", , n = 20, palette = "div", title = "Happy Planet Index", id = "name",
              # popup definition
              popup.vars=c(
                "Country: " = "name",
                "Crimes: " = "VC.IHR.PSRC.P5")  )
  )) +
  tm_layout(bg.color = "skyblue")



tm_shape(xx, projection = "+proj=eck4") +
  tm_polygons("VC.IHR.PSRC.P5", n = 9, palette = "div", title = "Happy Planet Index", id = "isoa3") +
  tm_style("gray") +
  tm_format("World")

#- con tmap se pueden hacer gráficos interactivos
#- tmap_mode("plot") 
tmap_mode("view")  


tm_shape(xx) +
  tm_polygons("VC.IHR.PSRC.P5", id = "name",  n =10)

tm_shape(xx, filter = World$continent=="Europe") +
  tm_polygons("HPI", id = "name", n =10)


#- tooltips: https://gis.stackexchange.com/questions/469419/using-tmap-r-package-to-plot-a-map-in-a-shiny-app-the-tooltip-remains-the-value