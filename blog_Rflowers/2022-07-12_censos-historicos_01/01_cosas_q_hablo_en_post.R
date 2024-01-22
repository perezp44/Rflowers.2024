#- para ver algunas cosas sobre las q hablo en el post -------------------------
library(tidyverse)

df <- pjpv.datos.2022::ine_censos_historicos
df_dicc <- pjpv.curso.R.2022::pjp_dicc(df)

zz <- df %>% filter(ine_muni.n.h == "Pancrudo") 

df_padron <- pjpv.datos.2022::ine_pob_mun_1996_2021

#- detectar los 24 municipios raros (solo censo de 1842) que no detectaba mi scrapping ----------------
feos <- c("085023",  "095060",  "165017", "175090", "175164", "175173", "225031", "255011", "255032", "255102", "255120", "255122", "255188", "255205", "255225", "255265", "255279", "255323", "255356", "255383", "255396", "255399", "255402", "265007")
#- estos 24 municipios sólo tienen datos de pob en el censo de 1842, luego desaparecen
#- y encima no se dice en las notas donde se integran
zz <- df %>% filter(ine_muni %in% feos) %>% 
  filter(year == 1842) %>% 
  tidyr::drop_na(pob)
#- sus anotaciones son "raras": aparece muchas veces "el Madoz"
zz$anotaciones

#- los 4 municipios q tampoco detectaba mi scrapping
feos_4 <- c("09342", "12066", "26504", "28511")


#- ver las discrepancias entre fjg y mis datos  --------------------------------
#- para ver si condiciona los rtdos
options(scipen = 999) #- quitar notación científica
cc <- df %>% mutate(percent = abs((dif_pob/pob)*100) , .before = dif_pob) %>% 
  mutate(percent_0 = (pob.fjg/pob) , .before = dif_pob) %>% 
  filter(dif_pob != 0) %>% 
filter( !(is.na(pob)  & is.na(pob.fjg) ) )
  
#- discrepancias de población con FJG en las capitales de prov.
cc_prov <- cc %>% filter(capital_prov == "Sí")




