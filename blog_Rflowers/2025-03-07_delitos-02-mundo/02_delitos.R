#- datos crimen UN: https://dataunodc.un.org/
#- aquí quiero ver el smpl de los datos
#- qué países y tipos de delitos tengo


#- cargo funciones ----------
source(here::here("./_funciones/_mys_ff.R"))


#- selecciono delito y año para hacer coropleta
my_year = "2017"
my_delito = "Homicide"


zz <- df %>% 
  select(iso3_code, region, country, year, tipo_delito.pjp, tasa_100mil) %>% 
  filter(year == my_year) %>%
  filter(tipo_delito.pjp == my_delito)
  

#- preparo zz para poder hacer COROPLETA
#- fusiono geometrías con datos de crímenes (Nº de observaciones)
# df_maps_0 <- left_join(zz, crim_geo, by = c("iso3_code" = "iso3_code")) 
# df_maps_f <- right_join(crim_geo, zz, by = c("iso3_code" = "iso3_code")) 
df_maps_ff <- left_join(crim_geo %>% filter(quitar_para_crimenes != "Si quitar"), zz, by = c("iso3_code" = "iso3_code")) 


#- zz_m es el df q uso para la coropleta
zz_m <- df_maps_ff 




my_name_leyenda =  my_delito
my_title = paste0("Tasa de homicidios (", my_year) 
my_subtitle = "(por 100.000 habitantes)"

##- leaflet ------------------
pp <- p_coro_leaflet_II(zz_m, "tasa_100mil", obs_label = "Tasa de homicidios (por 100.000 habitantes)", legend_title = my_name_leyenda,  view_zoom = 3)

pp

##- echarts4r -------------
library(echarts4r)

p <- create_echarts_map(zz_m, iso_col = "pais.iso", value_col = "tasa_100mil")

p |> e_theme("vintage")



##- boxplot -------------------
#- quiero reordenar los box
my_vv = "tasa_100mil"

zz <- zz %>% 
  mutate(region = forcats::as_factor(region)) %>% 
  group_by(region) %>%
  #- .data es un pronombre proporcionado por dplyr que permite acceder a las columnas del data frame actual.
  mutate(mean_n = mean(.data[[my_vv]], na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(region = forcats::fct_reorder(region, mean_n)) %>% 
  mutate(region.x = region)

#- reordenar pero con tidyeval
# my_vv = "tasa_100mil"
# my_vv_sym <- rlang::sym(my_vv)  # Convertir string a símbolo
# 
# zzz <- zz %>% 
#   mutate(region = forcats::as_factor(region)) %>% 
#   group_by(region) %>%
#   mutate(mean_n = mean(!!my_vv_sym, na.rm = TRUE)) %>% 
#   ungroup() %>%
#   mutate(region = forcats::fct_reorder(region, mean_n)) %>% 
#   mutate(region.x = region)


pp <- p_boxplot_interactivo(zz,
                            col_n = my_vv,
                            col_pais = "country",
                            col_iso3 = "iso3_code",
                            col_region = "region")
pp

zzz <- zz %>% 
  rename(pais = country) %>% 
  rename(iso3 = iso3_code) %>% 
  rename(n = rlang::sym(my_vv))

# Llamar a la función para crear el objeto girafe
pp <- crear_boxplot_interactivo(zzz)

pp
