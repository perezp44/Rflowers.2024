#- obtener datos de los premios literarios
#- algunos (Nadal y Planeta) los escrapee de la Wikipedia
#- el Princesa de Aturias de las Letras lo escrapee (a mano) de la Wikipedia
#- Los premios nacionales (incluido el Cervantes) lo saque a mano de la web del Ministerio de Cultura
library(tidyverse)
library(rvest)

df_orig <- rio::import(here::here("datos", "df_TODO_ok.rds"))
df <- df_orig

#- tabla de % por genero
zz <- df %>% filter(!is.na(ganador)) %>% 
  janitor::tabyl(genero, premio) #- porcentajes
zz <- df %>% filter(premio == "Nacional Poesía")


df <- df %>% mutate(edad = as.numeric(ano) - as.numeric(year_nac) +1)
names(df)

zz <- df %>% filter(!is.na(ganador)) %>% group_by(genero, premio) %>% 
  summarise(media = mean(edad, na.rm = TRUE)) %>% 
  tidyr::pivot_wider(names_from = genero, values_from = media)


#- grafico chulo ---------------------------------------------------------------

library(tidyverse)
laureates2 <- read_csv("http://api.nobelprize.org/2.1/laureates?limit=1000&format=csv")
#write_csv(laureates, file = here::here("nobel", "laureates.csv"))

#- pedazo grafico: https://twitter.com/c_gebhard/status/1510146260052766724




#- una querry de Nobeles: @medi_cago: Number of Nobel laureates is usually given per country (of birth), but that's not fair - I prefer normalizing it to population. From @wikidata https://w.wiki/bVX https://twitter.com/medi_cago/status/1302966773348786177/photo/1


#-  @egonwillighagen: as far as @wikidata knows, I have not been cited by a Nobel Prize winner yet. Have you? Try it now! Go to https://w.wiki/4ApX and change the "VALUES' for ?citedAuthor

#-  https://tools.wmflabs.org/scholia/award/Q35637 Prix Nobel de la paix sur Scholia
