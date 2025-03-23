#- quiero hallar el conjunto de países que tienen datos en todo el periodo
#- pero tengo el PB de que no hay NA's solo hay zeros ?????
my_vv <- "Victims of intentional homicide"

my_tt_1 <- 2000
my_tt_2 <- 2022
my_nn_observaciones <- (my_tt_2 - my_tt_1) + 1   #- nº de obsv

zz1 <- df_orig %>% #- salen 98
  filter(category == my_vv) %>% 
  select(1:2, year, value) %>% 
  filter(between(year, my_tt_1, my_tt_2)) 
  
zz1 <- df %>%  #- salen 49
  filter(category == my_vv) %>% 
  select(1:2, year, tasa_100mil) %>% 
  filter(between(year, my_tt_1, my_tt_2))

#- aqui good -----------------------
zz1 <- df %>% 
  filter(category == my_vv) %>% 
  select(1:2, year, tasa_100mil) %>% 
  filter(between(year, my_tt_1, my_tt_2)) %>% 
  group_by(iso3_code, country) %>% 
  summarise(nn_obs_pais = sum(length(tasa_100mil), na.rm = TRUE),
            todo_smpl = ifelse(nn_obs_pais == my_nn_observaciones, TRUE, FALSE))




#- Elegir q graficar -------------

my_vv <- "Victims of intentional homicide"
my_year <- 2022

zz_grafico <- df %>% #- salen 98
  filter(category == my_vv) %>% 
  select(1:2, year, tasa_100mil) %>% 
  filter(year = my_year) 




#-------------------------------------------------------------------------------
#- MAPAS -----------------------------------------------------------------------
#- https://marcinstepniak.eu/post/interactive-choropleth-maps-with-r-and-tmap-part-i/

library(tmap)  #- https://mtennekes.github.io/tmap/reference/qtm.html

library(sf)
data(World)
xx <- left_join(World, zz_grafico, World, by = c("iso_a3" = "iso3_code"))

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
