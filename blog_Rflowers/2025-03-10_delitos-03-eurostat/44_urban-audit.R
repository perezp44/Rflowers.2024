#- URBAN AUDIT: -----------------------------
#- https://ec.europa.eu/eurostat/web/cities/database
df <- pjpv.pkg.ff.2024::pjp_eurostat_download("urb_ctour")
df <- pjpv.pkg.ff.2024::pjp_eurostat_download("urb_ceduc")




dicc_df <- pjpv.pkg.ff.2024::pjp_dicc(df)
uniques_df <- pjpv.pkg.ff.2024::pjp_valores_unicos(df)
names(df)

zz <- df %>% filter(stringr::str_starts(cities_code, "ES")) %>% distinct(cities_code, cities) #- 99
                    
                    

                    
df2 <- pjpv.pkg.datos.2024::urban_audit_grandes_ciudades_2010_2023                  
dicc_df2 <- pjpv.pkg.ff.2024::pjp_dicc(df2)

zz2 <- df2 %>% distinct(ine_muni.n, ine_muni) #- 426
