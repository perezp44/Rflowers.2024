#- obtener datos de los premios literarios
#- algunos (Nadal y Planeta) los escrapee de la Wikipedia
#- el Princesa de Aturias de las Letras lo escrapee (a mano) de la Wikipedia
#- Los premios nacionales (incluido el Cervantes) lo saue a mano de la web del Ministerio de Cultura
library(tidyverse)
library(rvest)

# #- datos premio Nadal de Wikipedia
# my_url <- "https://es.wikipedia.org/wiki/Premio_Nadal#ganadores_y_finalistas"
# content <- read_html(my_url)
# my_tables <- content %>% html_nodes('body') %>% html_nodes('table') %>% html_table(fill = TRUE) #- casi saca la tabla
# my_table <- my_tables[[2]]
# df <- my_table %>% select(1, 2, 3) %>% janitor::clean_names()
# df <- df %>% 
#   mutate(original_xx = NA_character_, .before = ano) %>% 
#   mutate(genero = NA_character_, .after = obra) %>% 
#   mutate(premio = "Premio Nadal", .after = last_col())
#   
# #- exporto (no lo hagas xq has de poner genero a mano)
# # rio::export(x = df,  file = here::here("datos", "df_premios_nacionales.xlsx"), which = "premio_Nadal")
# 
# 
# #- datos premio Planeta en wikipedia
# my_url <- "https://es.wikipedia.org/wiki/Premio_Planeta"
# content <- read_html(my_url)
# my_tables <- content %>% html_nodes('body') %>% html_nodes('table') %>% html_table(fill = TRUE) #- casi saca la tabla
# my_table <- my_tables[[2]]
# 
# 
# df <- my_table %>% select(1, 2, 3) %>% janitor::clean_names()
# df <- df %>% mutate(ganador = stringr::str_replace(ganador, "\\[.*", ""))  #- quito lo q va despues de []
# df <- df %>% 
#   mutate(original_xx = NA_character_, .before = ano) %>% 
#   mutate(genero = NA_character_, .after = obra) %>% 
#   mutate(premio = "Premio Planeta", .after = last_col())
# 
# #- exporto (no lo hagas xq has de poner genero a mano)
# # rio::export(x = df,  file = here::here("datos", "df_premios_nacionales.xlsx"), which = "premio_Planeta")



#- he sacado a mano los galardonados de varios premios (el genero lo he puesto a mano)
# df <- rio::import(here::here("datos", "df_premios_nacionales.xlsx"))
# df <- df %>% separate(premiados, into = c("ano", "premiado"), sep = " - ")
# 
# library(readxl)
# my_dfs_list <- lapply(excel_sheets(here::here("datos", "df_premios_nacionales_amano.xlsx")), 
#                       read_excel,
#                       path = here::here("datos", "df_premios_nacionales_amano.xlsx"))
# my_dfs_list <- map(my_dfs_list, separate, remove = FALSE,  original_xx, into = c("ano", "ganador", "obra"), sep = " - ", fill = "right", extra = "merge")
# 
# df <- bind_rows(my_dfs_list[[1]], my_dfs_list[[2]], my_dfs_list[[3]], my_dfs_list[[4]], my_dfs_list[[5]])
# unique(df$premio)
# 
# # rio::export(df, here::here("datos", "df_premios_nacionales.xlsx"))


#- tengo los prremios nacionales y los escrapeados en 2 archivos (hay q unirlos)
df1 <- rio::import(here::here("datos", "df_premios_nacionales.xlsx"))

library(readxl)
my_dfs_list <- lapply(excel_sheets(here::here("datos", "df_premios_escrapeados.xlsx")), 
                      read_excel,
                      path = here::here("datos", "df_premios_escrapeados.xlsx"))
df2 <- bind_rows(my_dfs_list[[2]], my_dfs_list[[3]])
df2 <- df2 %>% mutate(ano = as.character(ano))
df2 <- bind_rows(df2, my_dfs_list[[1]])
df_ok <- bind_rows(df1, df2)
rio::export(df_ok, here::here("datos", "df_premios_literarios_ok.rds"))
# unique(df$premio)
# 


df_w1 <- df %>% filter(premio == "Nacional Poesía")


#- Premio Cervantes en wikidata -----------
library(wikifacts) 

query <- 
  'SELECT DISTINCT ?person ?personLabel ?genderLabel ?year_nac ?educated ?educatedLabel ?nacionalidadLabel ?awardLabel  ?year_premio ?lugar_nacLabel ?countryLabel ?fecha_nacimiento ?fecha_muerte ?_image ?_coordinates WHERE {
    ?person p:P166 ?statement.
	?statement ps:P166 ?award.
	?award wdt:P279?/wdt:P31?  wd:Q2883115.
    ?person wdt:P21 ?gender.

	OPTIONAL {
		?statement pq:P585 ?date.
		BIND(YEAR(?date) AS ?year_premio)
	}
	SERVICE wikibase:label { bd:serviceParam wikibase:language "es". }
	OPTIONAL { ?person wdt:P569 ?fecha_nacimiento. }
    OPTIONAL {
		?person wdt:P569 ?fecha_nacimiento.
		BIND(YEAR(?fecha_nacimiento) AS ?year_nac)
	}
  
	OPTIONAL { ?person wdt:P570 ?fecha_muerte. }
	OPTIONAL { ?person wdt:P18 ?_image. }
	OPTIONAL { ?person wdt:P101 ?ocupacion. }
	OPTIONAL { ?person wdt:P69 ?educated. }
    OPTIONAL { ?person wdt:P27 ?nacionalidad.  }
  	OPTIONAL { 
      	?person wdt:P19 ?lugar_nac. 
        ?lugar_nac wdt:P625 ?_coordinates.  
      	?lugar_nac wdt:P17 ?country. 
    }

    
}
ORDER BY DESC (?year) (?awardLabel)'
#- mando la consulta a wikidata
df_w2 <- wiki_query(query)
#rio::export(df_nobeles_wiki, "./pruebas/df_nobeles_wiki.csv")

df_w1 <- df_w1 %>% mutate(ano = stringr::str_remove(ano, " -"))
  
#- a ver si se pueden limpiar y juntar


df_w1 <- df_w1 %>% mutate(ganador = case_when(
  ganador == "Pilar Pallarés" ~ "Pilar Pallarés García",
  ganador == "Antonio Hernández" ~ "Antonio Hernández Ramírez",
  ganador == "Joan Margarit" ~ "Joan Margarit i Consarnau",
  ganador == "José Corredor-Matheos" ~ "José Corredor Matheos",
  ganador == "José María Millares Sall" ~ "José María Millares",
    TRUE ~ ganador))

zz1 <- df_w2 %>% distinct(person, personLabel, genderLabel, year_premio, year_nac, lugar_nacLabel, awardLabel, countryLabel) %>% mutate(nombrecito_wikidata = personLabel, .after = personLabel)
zz2 <- left_join(df_w1, zz1, by = c("ganador" = "personLabel"))

testthat::expect_equal(sum(is.na(zz2$person)), 0)  #- no puede haber NA's' (salvo q haya premios desiertos)



#-------------------------------------------------------------------------------
#- solo me hace falta los datos de: person, personLabel, genderLabel
#- PERO me gustaria tb los datos de  year, year_nac
#- asi que tengo que hacer un check para que los datos de year y year_nac sean unicos para cada nombre
zz1 <- df_w2 %>% distinct(person, personLabel) %>% nrow()  #- nº único de autores
zz2 <- df_w2 %>% distinct(person, personLabel, genderLabel, year_premio, year_nac, lugar_nacLabel, awardLabel, countryLabel) %>% nrow()  #- nº único de autores (genero y fecha_nac)
testthat::expect_equal(zz1, zz2)
rm(zz1, zz2)

#- hago nest xq hay imagenes, educacion etc... no unicos p.ej Alvaro Cunqueiro tiene 2 coordenadas
df_w2a <- df_w2 %>% group_by(person, personLabel, genderLabel, year_premio, year_nac, lugar_nacLabel, awardLabel, countryLabel) %>% nest()

#- fusiiono wikidata y wikipedia
df <- left_join(df_w1, df_w2a, by = c("ganador" = "personLabel"))
testthat::expect_equal(nrow(df), nrow(df_w1))


#- exporto ---------------------------------------------------------------------
#rio::export(df, here::here("datos", "df_nacional_poesia.rds"))
