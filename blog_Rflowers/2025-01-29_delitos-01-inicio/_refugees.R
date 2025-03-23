#- pkg refugee: https://populationstatistics.github.io/refugees/
#- pak::pkg_install("PopulationStatistics/refugees")
#- buen tutorial de plots con estos datos de UNHCR: https://dataviz.unhcr.org/tutorials/r/   
#- tb themes de UNHCR: https://fosstodon.org/@cvidonne/113838299284540354   

library(tidyverse)
library(refugees)

ref_coo_10 <- refugees::population |>
  filter(year == 2019) |>
  summarise(refugees = sum(refugees, na.rm = TRUE) + sum(oip, na.rm = TRUE),
            .by = coo_name) 

#- esp
esp <- refugees::population %>% 
  filter(year == 2019) |>
  filter(coa_name == "Spain")
  