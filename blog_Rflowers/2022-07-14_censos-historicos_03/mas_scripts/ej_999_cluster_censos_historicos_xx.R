#- CENSOS HISTORICOS (INE) -----------------------------------------------------
#- para hacer CLUSTER

# cargamos paquetes
library(tidyverse)
#pjpv.datos.2022::ine_censos_historicos

#- cargamos datos de CENSOS (para capitales de provincia)
df <- pjpv.curso.R.2022::ine_censos_historicos_capitales
df_dicc <- pjpv.curso.R.2022::pjp_dicc(df) #- hay 18 censos (1842-2011)
names(df)

#- un ejemplo (valencia)
zz <- df %>% filter(ine_muni.n == "Teruel")

#- arreglamos población (mezcla de pob_de_derecho y pob_de_hecho)
df <- df %>%
  mutate(pob_muni = pob_de_derecho, .after = pob_de_derecho) %>%
  mutate(pob_muni = ifelse(is.na(pob_muni), pob_de_hecho, pob_muni)) %>% 
  select(ine_muni, ine_muni.n,  year, pob_muni, hogares, ine_prov) %>%
  #- creo la v. tamaño de la familia
  mutate(tam_fami = pob_muni/hogares)


#- cargo datos de población provincial 
df_prov <- pjpv.datos.2022::ine_censos_historicos_prov %>%
  #- creo población (igual q como antes)
  mutate(pob_muni = pob_de_derecho, .after = pob_de_derecho) %>%
  mutate(pob_muni = ifelse(is.na(pob_muni), pob_de_hecho, pob_muni)) %>%
  mutate(pob_muni = as.numeric(pob_muni)) %>%
  select(ine_prov, year, pob_muni, hogares, ine_prov.n) %>%
  rename(pob_prov = pob_muni) %>% 
  rename(hogares_prov = hogares) %>% 
  mutate(tam_fami_prov = pob_prov/hogares_prov, .before = ine_prov.n) 

#- fusiono datos de las capitales con pob provincial
df_ok <- left_join(df, df_prov)

#- creo v. q represente el "tamaño" de la capital (% de la capital sobre la provincia)
df_ok <- df_ok %>% 
  mutate(percent_pob = pob_muni/pob_prov*100, .after = year) %>% 
  mutate(percent_hogares = hogares/hogares_prov*100, .after = percent_pob) %>% 
  mutate(dif_tam_fami = tam_fami - tam_fami_prov, .after = percent_hogares)
names(df_ok)

#- tb voy a calcular rankings:
#- calculo rankings (para cada censo)
df_ok <- df_ok %>%
  group_by(year) %>%
  arrange(desc(pob_muni)) %>%
  mutate(rank_pob = 1:n()) %>%
  arrange(desc(percent_pob)) %>%
  mutate(rank_percent_pob = 1:n()) %>%
  ungroup()

zz <- df_ok %>% filter(ine_muni.n == "Teruel")

#- quito Ceuta y Melilla
df_ok_2 <- df_ok %>% filter(! (ine_prov %in% c(51,52)) )

#- EUROVISION: intento haces cluster à la eurovision ---------------------------
library(widyr)

set.seed(234)
df_clust <-  df_ok_2 %>% tidyr::drop_na() %>% 
  # dimensionality reduction with SVD: https://www.youtube.com/watch?v=UyAfmAZU_WI
  widyr::widely_svd(ine_muni.n, year, tam_fami, nv = 10) %>%
  #- combination of SVD and k-means: https://www.researchgate.net/publication/284617182_Combination_of_Singular_Value_Decomposition_and_K-means_Clustering_Methods_for_Topic_Detection_on_Twitter
  widyr::widely_kmeans(ine_muni.n, dimension, value, k = 4)

#- CLUSTER ---------------------------------------------------------------------
#- https://www.tidymodels.org/learn/statistics/k-means/
library(tidymodels)
df_ok <- df_ok %>% 
  # en cluster las v. han de ser numéricas
  mutate(ine_prov.2 = parse_number(ine_prov)) 


df_2011 <- df_ok %>% filter(year == 2011) 
df_clus_2011 <- df_2011 %>% 
  select(where(is.numeric)) %>% tidyr::drop_na()
str(df_clus_2011)

kclust <- kmeans(df_clus_2011, centers = 3)
summary(kclust)
tidy(kclust)

kclusts <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(df_clus_2011, .x)),
    tidied = map(df_clus_2011, tidy),
    glanced = map(df_clus_2011, glance),
    augmented = map(kclust, augment, points)
  )


factoextra::fviz_cluster(kclust, data = df_clus_2011[, 1:12],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
             )

tidy(kclust)
clusters <- 
  kclust %>%
  unnest(cols = c(tidied))
aa <- broom::augment(kclust, df_2011)

zz <- aa %>% distinct(ine_muni.n, ine_prov.n, id_muni, ine_muni, .cluster)


#- veamos con toda la información (voy a hacerlo ancho)
df_ok_2 <- df_ok %>% tidyr::pivot_wider(names_from = year, values_from)


#- https://r4ds.hadley.nz/data-tidy.html#untidy-data
col_year <- gapminder |> 
  mutate(gdpPercap = log10(gdpPercap)) |> 
  pivot_wider(
    id_cols = country, 
    names_from = year,
    values_from = gdpPercap
  ) 
col_year


#- estaría bien estimar un modelo para cada censo ------------------------------




df_full <- pjpv.datos.2022::ine_censos_historicos
             