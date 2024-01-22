#- my funciones para el proyecto de contratacion

# my_check_ID_UPDATED <- function(df, v1 = ID, v2 = UPDATED) {
#     n_ID_unicos <- lic_publicaciones %>% distinct(ID) %>% nrow()
#     n_ID_UPDATED_unicos <- lic_publicaciones %>% distinct(UPDATED, ID) %>% nrow()
#     dif_nn <- n_ID_unicos - n_ID_UPDATED_unicos
# }


#- retorna el nº de rows únicas de un df (tiene en cuenta todas las columnas del df)
my_f_unique_rows_df <- function(df, print = TRUE) {
  nn_df_rows <- df %>% nrow()
  nn_df_unique_rows <- df %>% n_distinct()
  if (print == TRUE) cat("En el df completo hay", nn_df_unique_rows, "filas UNICAS. Como en el df completo hay ", nn_df_rows, "filas, entonces , hay" , nn_df_rows-nn_df_unique_rows, "filas repetidas.")
  nn_df_unique_rows
}


#- retorna un df con las filas únicas de cada columna de un df
my_f_unique_rows_cols_df <- function(df, print = TRUE) {
  nn_df_rows <- df %>% summarise_all(list(~ n_distinct(.)))    #-- GOOD tb esto: https://tidyr.tidyverse.org/reference/expand.html
  #- la paso a 2 columnas (la transpongo)
  nn_df_rows <- nn_df_rows %>% gather(variables, NN_unicos) %>% mutate(NN = nrow(df)) %>% mutate(dif = NN - NN_unicos)
  if (print == TRUE) print(nn_df_rows)
  nn_df_rows
}


#- f. q. chequea si en un df hay una unica row para cada uno de los valores de la variable v1  (puedes elegir la v1; osea cambiar ID por otra variable)
#- si en el df hay el mismo nº de row unicas q en v1, entonces dif_nn sera igual a cero
my_f_check_v1_unicos <- function(df, v1 = ID, print = TRUE) {
  v1 <- enquo(v1)
  name_v1 <- df %>% dplyr::select(!!v1) %>% names()
  nn_v1_unicos <- df %>% dplyr::select(!!v1) %>% unique() %>% nrow()
  nn_todas_v_unicos <- df %>% nrow()
  if (print == TRUE) {
    cat("- la v.", name_v1, "tiene", (nn_todas_v_unicos - nn_v1_unicos) , "filas con valores repetidos", "\n")
    cat("- la v.", name_v1, "tiene",  nn_v1_unicos, "filas UNICAS" , ", como el df tiene" , nn_todas_v_unicos, "filas, entonces la v.", name_v1, "tiene"  , (nn_todas_v_unicos -nn_v1_unicos) , "filas de con valores repetidos")
  }
  aa <- df %>% dplyr::group_by(!!v1) %>% summarise(NN = n()) %>% arrange(desc(NN))  #- te muestra los valores de ID q se repiten
}


#- f. q. retorna las rows de un df que resulta que tienen repetidos los valores de la v. ID
my_f_v1_valores_repetidos <- function(df, v1 = ID, print = TRUE) {
  v1 <- enquo(v1)
  aa <- df %>% dplyr::group_by(!!v1) %>% summarise(NN = n()) %>% filter(NN > 1) %>% select(!!v1) %>% as_vector()
  bb <- df %>% filter(!!v1 %in% aa) %>% arrange(!!v1)
}



#- f. q. chequea si en df hay una única row por v1 cuando lo cruzas con mas variables. Los ... son para que pongas las variables que quieras q estén en el df donde se chequea.
#Por ejemplo: my_check_ID_vv_s(df, ID, STATUS, UPDATED)  . Lo que devuelve la función (dif_nn) debe salir zero si hay una fila única por v1 (ID)

my_f_check_ID_vv_s <- function(df, v1 = ID, ... , print = TRUE) {
  v1 <- enquo(v1)
  nn_v1_unicos <- df %>% dplyr::select(!!v1) %>% unique() %>% nrow()
  name_v1 <- df %>% dplyr::select(!!v1) %>% names()
  #df1 <- df %>% dplyr::group_by(!!v1) %>% unique(!!V1)
  df1 <- df %>% dplyr::select(!!v1, ...)
  names_vv <- df1 %>% names()
  nn_todas_v_unicos <- df1 %>% distinct() %>% nrow()
  if (print == TRUE) cat("Hay", nn_v1_unicos, "filas UNICAS para la variable" , name_v1 , " ;   ; y hay"  , nn_todas_v_unicos, "filas unicas si lo combinamos con las variables:",  names_vv, "\n")
  dif_nn <- nn_v1_unicos - nn_todas_v_unicos
  #dif_nn
}


#- f. q. mira cuantas rows únicas hay en todas y cada una de las combinaciones de de v1 con las demas columnas de df
my_bucle_check_v1_unicos <- function(df, v1 = ID, print = TRUE) {
  my_print <- print
  v1 <- enquo(v1)
  name_v1 <- df %>% dplyr::select(!!v1) %>% names()
  names_vv <- df %>% names()
  vvvv <- c()
  for (ii in names_vv) {
    #print(ii)
    vvvv[ii] <- my_f_check_ID_vv_s(df, !!v1, ii, print = my_print)
  }
  vvvv
}


# ff. para formatear números ---------------------------------------------------
my_format_d0 <- function(number, nn_decimales = 0) {
  format(round(number), nsmall = nn_decimales, big.mark = ".", decimal.mark = ",")
}

my_format_d2 <- function(number, nn_decimales = 2) {
  format(round(number), nsmall = nn_decimales, big.mark = ".", decimal.mark = ",")
}




#- ff. especificas del proyecto de SCRAPPING con FRAN --------------------------

#- creo ff. para hallar el rank_atleta_prueba_temporada
my_rank_atleta_prueba_temporada <- function(df){
  df_tiempo <- df %>% filter(prueba.f.2 == "carreras")
  df_metros <- df %>% filter(prueba.f.2 == "saltos y lanzamientos")
  df_tiempo <- df_tiempo %>%  group_by(prueba, genero, temporada) %>% 
    arrange(mark_xx, fecha_marca, edad) %>% 
    mutate(rank_atleta_prueba_temporada = row_number(), .before = mark_xx) %>%
    ungroup()
  df_metros <- df_metros %>%  group_by(prueba, genero, temporada) %>% 
    arrange(desc(mark_xx), fecha_marca, edad) %>% 
    mutate(rank_atleta_prueba_temporada = row_number(), .before = mark_xx) %>%
    ungroup()    
  df <- bind_rows(df_tiempo, df_metros)
}


#- creo ff. para hallar el rank_atleta_prueba
my_rank_atleta_prueba <- function(df){
  df_tiempo <- df %>% filter(prueba.f.2 == "carreras")
  df_metros <- df %>% filter(prueba.f.2 == "saltos y lanzamientos")
  df_tiempo <- df_tiempo %>%  group_by(prueba, genero) %>% 
    arrange(mark_xx, fecha_marca, edad) %>% 
    mutate(rank_atleta_prueba = row_number(), .before = mark_xx) %>%
    ungroup()
  df_metros <- df_metros %>%  group_by(prueba, genero) %>% 
    arrange(desc(mark_xx), fecha_marca, edad) %>% 
    mutate(rank_atleta_prueba = row_number(), .before = mark_xx) %>%
    ungroup()    
  df <- bind_rows(df_tiempo, df_metros)
}


#- creo ff. para hallar el nn_maximo, nn_maximo_ok y nn_maximo_ok_ok
#- estas variables sirven para que los graficos de los diferentes años tengan el mismo nº de atletas
my_crear_vv_nn_para_graficos <- function(df){
  df_tiempo <- df %>% filter(prueba.f.2 == "carreras")
  df_metros <- df %>% filter(prueba.f.2 == "saltos y lanzamientos")
  df_tiempo <- df_tiempo %>%
    group_by(prueba, genero, temporada) %>% 
    mutate(nn_maximo = max(rank_atleta_prueba_temporada)) %>% ungroup() %>% #- calcula el nº atletas de esa {prueba, genero, temporada}
    group_by(prueba, genero) %>% 
    mutate(nn_maximo_ok = min(nn_maximo)) %>% ungroup() %>% #- calcula el nº atletas de esa {prueba, genero}
    group_by(prueba) %>% 
    mutate(nn_maximo_ok_ok = min(nn_maximo_ok))  %>% ungroup() #- #- calcula el nº atletas de esa {prueba}, xsi quieres graficar a H y M juntos
  df_metros <- df_metros %>% 
    group_by(prueba, genero, temporada) %>% 
    mutate(nn_maximo = max(rank_atleta_prueba_temporada)) %>% ungroup() %>% 
    group_by(prueba, genero) %>% 
    mutate(nn_maximo_ok = min(nn_maximo)) %>% ungroup() %>% 
    group_by(prueba) %>% 
    mutate(nn_maximo_ok_ok = min(nn_maximo_ok)) %>% ungroup() #- si quieres graficar a H y M juntos
  df <- bind_rows(df_tiempo, df_metros)
}



#- creo ff. para hallar las peores marcas de una {prueba, genero} en un df
my_peores_marcas_prueba_genero <- function(df){
   aa <- df %>% group_by(genero, prueba.f, prueba.f.2) %>% 
     summarise(peor_marca = ifelse(prueba.f.2 == "saltos y lanzamientos", min(mark_xx), max(mark_xx))) %>% 
     distinct() %>% ungroup()
   return(aa)
}

#- creo ff. para hallar los WR actuales
my_WR_actuales <- function(df){
  aa <- df %>% group_by(prueba.f, genero) %>% filter(id_WR_prueba == min(id_WR_prueba)) %>% ungroup() %>% select(genero, prueba.f, prueba.f.2, mark_xx) %>% rename(WR_actual = mark_xx) %>% distinct()
  return(aa)
}
  
#- creo ff. fusionar WR y peores_marcas y para hallar el rango entre WR_actual y peores_marcas
my_fusion_WR_peor <- function(peores_marcas, WR_actuales){
  aa <- left_join(peores_marcas, WR_actuales) 
  aa <- aa %>% 
  mutate(rango_WR_peor = ifelse(prueba.f.2 == "saltos y lanzamientos", 
                                WR_actual - peor_marca , peor_marca - WR_actual)) %>% 
  mutate(rango_WR_peor_percent = rango_WR_peor/WR_actual)
  return(aa)
}



#- creo ff. para juntar dfxx con df_mas 
my_fusion_dfxx_WR_peor <- function(dfxx, df_mas){
  aa <- left_join(dfxx, df_mas) 
  aa <- aa %>% 
  select(competitor, temporada, mark, mark_xx, mark_xx_best, mark_xx_best_temporada, WR_actual, peor_marca, rango_WR_peor_percent, everything()) %>% 
  mutate(peor_q_WR_percent = ifelse(prueba.f.2 == "saltos y lanzamientos", 
                                    (WR_actual - mark_xx)/WR_actual, 
                                    (mark_xx - WR_actual)/WR_actual), .after = mark_xx) %>% 
  mutate(mejor_q_peor_percent = ifelse(prueba.f.2 == "saltos y lanzamientos", 
                                       (mark_xx - peor_marca)/peor_marca, 
                                       (peor_marca - mark_xx)/peor_marca), .after = mark_xx)
  return(aa)
}





#- creo ff. crear rango de valores entre WR y peor marca 
my_crear_rango_WR_peor <- function(df_mas, my_length_out = 10){
  #- putana!!!, lo voy a hacer con bucles (q no me vea Hadley!!!)
  #- 1) crear un vector con las marcas, desde WR hasta la PEOR. Lo almaceno en df_feete
  #my_length_out <- 10  #- cuantos "intervalos" haces entre WR y la peor marca
  df_feete <- df_mas %>% mutate(prueba = as.character(prueba.f))
  df_feo_2 <- data.frame(genero = "marciano", prueba = "dardos", id_marquita = 1, marquita = 2.44) #- es df fake para poder agregar
  for (ii in 1:length(pruebas_chachis)) {
    for(jj in c("men", "women")) {
      # print("---------------------------------")
      # print(pruebas_chachis[ii])
      # print(jj)
      aa <- df_mas %>% filter(prueba.f == pruebas_chachis[ii]) %>% filter(genero == jj)
      if((nrow(aa) != 0L)){
        if (aa$prueba.f.2 == "saltos y lanzamientos" & (nrow(aa) != 0L)) {
          vv_marcas <- c(aa$peor_marca, aa$WR_actual) 
          vv_marcas <- seq(from = aa$peor_marca, to = aa$WR_actual, length.out = my_length_out) } else{
            vv_marcas <- c( aa$WR_actual, aa$peor_marca) 
            vv_marcas <- seq(from = aa$WR_actual, to = aa$peor_marca, length.out = my_length_out)
          }
      } else{
        vv_marcas <- NA
      }
      #print(vv_marcas)
      df_feo <- data.frame(genero = jj, prueba = pruebas_chachis[ii], id_marquita = c(1:my_length_out), marquita = vv_marcas)
      #print(df_feo)
      df_feo_2 <- bind_rows(df_feo_2, df_feo)
    }
  }
  
  df_feete <- left_join(df_feete, df_feo_2)
return(df_feete)
}


#- creo ff. para fusionar dfxx y feete y crear SUPERA_SI
my_supera_SI <- function(dfxx, df_feete){
#- 2) x {temporada, genero, prueba} generar el % q supera cada marca (hay my_length_out marcas)
  #- TENGO QUE: ------------------------------------------------------------------
  #- calcular cuantos superan una serie de MARCAS, para ello he de:
  #- 1) crear un vector con las marcas, desde WR hasta la PEOR
  #- 2) x {temporada, genero, prueba} generar el % q supera cada marca
  #- 2) x {temporada, genero, prueba} generar el % q supera cada marca (hay my_length_out marcas)  
#- primero fusiono dfxx con lo de las marcas (df_feete)
df_xx_x <- left_join(dfxx, df_feete)
df_xx_x <- df_xx_x %>% select(id_atleta, competitor, prueba, prueba.f, prueba.f.2, genero, temporada, mark_xx, WR_actual, id_marquita, marquita, nn_maximo_ok, everything())
  #zz <- pjpv2020.01::pjp_f_dicc(df_xx_x)
  #- esto hay q hacerlo con case_when para poder hacerlo para carreras y saltos 
  #- GAGON (este era el pb de q no llegase a 100%)
  zz_1 <- df_xx_x %>% mutate(supera_SI = case_when(
    (prueba.f.2 == "carreras") & (mark_xx <= marquita) ~ 1,  
    (prueba.f.2 == "carreras") & (mark_xx > marquita) ~ 0,
    (prueba.f.2 == "saltos y lanzamientos") & (mark_xx >= marquita) ~ 1,
    (prueba.f.2 == "saltos y lanzamientos") & (mark_xx < marquita) ~ 0,
    TRUE ~ NA_real_ ), .after = marquita)
return(zz_1)
  }


#- creo ff. para graficar el PRIMER gráfico
my_graph_01 <- function(dfxx, pruebas = NULL, generos = NULL,  my_pal){
  my_rev_pal <- rev(my_pal)
  if (is.null(pruebas)) {
    pruebas <- levels(dfxx$prueba.f)
  }
  if (is.null(generos)) {
    generos <- c("men", "women")
  }
  for (ii in pruebas) {
    #print(ii)
    df_prueba <- dfxx %>% filter(prueba.f == ii)
    for (jj in generos) {
      #print(jj)
      subtitulo <- glue::glue("Prueba:", ii, " .Genero:", jj)
      df_prueba_2 <- df_prueba %>% filter(genero == jj)
      df_prueba_2_label <- df_prueba_2 %>% group_by(temporada, genero, prueba) %>% 
        filter(rank_atleta_prueba_temporada == max(rank_atleta_prueba_temporada)) %>%   #- se supone q solo esta el ultimo
        ungroup()   
      p <- df_prueba_2 %>% 
        ggplot(aes(x = rank_atleta_prueba_temporada, y = mark_xx, color = temporada)) + 
        scale_color_manual(values = my_pal) +
        geom_line() +
        geom_label(data = df_prueba_2_label, aes(x = rank_atleta_prueba_temporada, y = mark_xx, label = temporada), size = 1) +
        theme_minimal() + 
        theme(legend.position = "none") +  
        labs(title = "Marcas por temporada", subtitle = subtitulo)
      
      
      df_feo <- df_prueba_2_label %>% select(temporada, mark_xx, mark_xx_best_temporada, mark_xx_best) %>% arrange(temporada)
      #df_feo1 <- df_feo %>% mutate(my_df_paleta = my_pal)
      #print(nrow(df_feo))
      if (nrow(df_feo) == nn_paleta) {
        df_feo1 <- bind_cols(df_feo, my_pal = my_pal)
      }else
      { df_feo1 <- df_feo}
      
      df_prueba_2_label_x <- df_feo1 %>% arrange(mark_xx_best_temporada) %>% mutate(rank_graph = row_number()) 
      my_df_paleta <- df_prueba_2_label_x$my_pal
      
      p2 <-  ggplot(df_prueba_2_label_x) + 
        geom_label(data = df_prueba_2_label_x, aes(y = rank_graph, label = temporada), color = df_prueba_2_label_x$my_pal , x = 1, size = 2.9, label.size = 1) +
        theme_void() + theme(legend.position = "none") +
        scale_x_continuous(limits = c(1,1)) +
        scale_color_manual(values = df_prueba_2_label_x$my_pal )
      
      
      
      pp <- p + p2 +  plot_layout(widths = c(6, 1))
      plot(pp)
    }
  }
  
}

# my_graph_01(dfxx, pruebas = "400-metres",  generos = "men", my_pal = my_pal)
# my_graph_01(dfxx, pruebas = "javelin-throw",  generos = "men", my_pal = my_pal)





#- creo ff. para graficar el SEGUNDO gráfico
my_graph_02 <- function(dfxx, pruebas = NULL, generos = NULL, percent = TRUE, my_pal){
  my_rev_pal <- rev(my_pal)
  if (percent == TRUE) {
    my_vv <- expr(NN_superan_marquita_percent)
  }else{
    my_vv <- expr(NN_superan_marquita)
  }
  
  if (is.null(pruebas)) {
    pruebas <- levels(dfxx$prueba.f)
  }
  if (is.null(generos)) {
    generos <- c("men", "women")
  }
  for (ii in pruebas) {
    #print(ii)
    df_prueba <- dfxx %>% filter(prueba.f == ii)
    for (jj in generos) {
      #print(jj)
      subtitulo <- glue::glue("Prueba:", ii, " .Genero:", jj)
      df_prueba_2 <- df_prueba %>% filter(genero == jj)
      
      if (df_prueba$prueba.f.2[[1]] == "carreras") {
        df_prueba_2_label <- df_prueba_2 %>% group_by(temporada, genero, prueba) %>% 
          #- solo cojo el % final
          filter(NN_superan_marquita_percent == max(NN_superan_marquita_percent)) %>%  
          filter(id_marquita == min(id_marquita)) %>% 
          arrange(id_marquita) %>% 
          ungroup()   
      } else{
        df_prueba_2_label <- df_prueba_2 %>% group_by(temporada, genero, prueba) %>% 
          #- solo cojo el % final
          filter(NN_superan_marquita_percent == max(NN_superan_marquita_percent)) %>%  
          filter(id_marquita == max(id_marquita)) %>% 
          arrange(id_marquita) %>% 
          ungroup()   
      }
      
      p <- df_prueba_2 %>% 
        ggplot(aes(x = marquita, y = !!my_vv, color = temporada)) + 
        geom_line() +
        scale_color_manual(values = my_pal) +
        #geom_line(aes(group = temporada))  + 
        geom_label(data = df_prueba_2_label, aes(x = marquita, !!my_vv, label = temporada), size = 1) +
        theme_minimal() + theme(legend.position="none") +  labs(title = "Marcas por temporada", subtitle = subtitulo)
      
      df_feo <- df_prueba_2_label %>% select(temporada, id_marquita, marquita, NN_superan_marquita) %>% arrange(temporada)
      #print(nrow(df_feo))
      
      
      if (nrow(df_feo) != nn_paleta) {
        df_feo <- df_feo
      }else {
        df_feo <- df_feo %>% mutate(my_df_paleta = my_pal)
      }
      
      
      #df_feo <- df_feo %>% mutate(my_df_paleta = my_pal)
      if (df_prueba$prueba.f.2[[1]] == "carreras") {
        df_prueba_2_label_x <- df_feo %>% arrange(desc(id_marquita)) %>% mutate(rank_graph = row_number())
      }else{
        df_prueba_2_label_x <- df_feo %>% arrange(id_marquita) %>% mutate(rank_graph = row_number())
      }
      my_pal_2 <- df_prueba_2_label_x$rank_graph[order(match(df_prueba_2_label_x$rank_graph, my_pal))]  
      p2 <-  ggplot(df_prueba_2_label_x) + 
        #geom_line() +
        #geom_line(aes(group = temporada))  + 
        geom_label(data = df_prueba_2_label_x, aes(y = rank_graph, label = temporada), colour = df_prueba_2_label_x$my_df_paleta, x = 1,size = 2.9, label.size = 1) +
        theme_void() + theme(legend.position = "none") +
        scale_x_continuous(limits = c(1,1))
      
      pp <- p + p2 +  plot_layout(widths = c(6, 1))
      plot(pp)
      
    }
  }
}


#my_graph_02(df_2_ok, pruebas = "javelin-throw", generos = "women", percent = TRUE, my_pal = my_pal)
#my_graph_02(df_2_ok, pruebas = "800-metres", generos = "women", percent = TRUE, my_pal = my_pal)

#my_graph_02(zz_ok, pruebas = "100-metres", generos = "men", percent = TRUE, my_pal = my_pal)





#- creo ff. para graficar el TERCER gráfico: x = EDAD, y = velocidad, (una curva por temporada)
#- TERCER.GRAFICO.A): pongo todos los puntitos, todos los corredores

#my_vv_choosen = expr(mark_xx)
my_graph_03_A <- function(dfxx, pruebas = NULL, generos = NULL,  my_pal = my_pal, vv_choosen = my_vv_chosen){
  my_rev_pal <- rev(my_pal)
  if (is.null(pruebas)) {
    pruebas <- levels(dfxx$prueba.f)
  }
  if (is.null(generos)) {
    generos <- c("men", "women")
  }
  for (ii in pruebas) {
    #print(ii)
    df_prueba <- dfxx %>% filter(prueba.f == ii)
    for (jj in generos) {
      #print(jj)
      subtitulo <- glue::glue("Prueba:", ii, " .Genero:", jj)
      df_prueba_2 <- df_prueba %>% filter(genero == jj)
      df_prueba_2_label <- df_prueba_2 %>% group_by(temporada, genero, prueba) %>% 
        filter(rank_atleta_prueba == max(rank_atleta_prueba)) %>%   #- se supone q solo esta el ultimo
        ungroup()   
      p <- df_prueba_2 %>% 
        #- y = velocidad_ms
        ggplot(aes(x = edad, y = !!vv_choosen, color = temporada)) + 
        scale_color_manual(values = my_pal) +
        geom_point() +
        geom_smooth() +
        #geom_label(data = df_prueba_2_label, aes(x = rank_atleta_prueba, y = mark_xx, label = temporada), size = 1) +
        theme_minimal() + 
        theme(legend.position = "none") +  
        labs(title = "Marcas por temporada", subtitle = subtitulo)
      
      
      df_feo <- df_prueba_2_label %>% select(temporada, mark_xx, mark_xx_best_temporada, mark_xx_best) %>% arrange(temporada)
      #df_feo1 <- df_feo %>% mutate(my_df_paleta = my_pal)
      #print(nrow(df_feo))
      if (nrow(df_feo) == nn_paleta) {
        df_feo1 <- bind_cols(df_feo, my_pal = my_pal)
      }else
      { df_feo1 <- df_feo}
      
      df_prueba_2_label_x <- df_feo1 %>% arrange(mark_xx_best_temporada) %>% mutate(rank_graph = row_number()) 
      my_df_paleta <- df_prueba_2_label_x$my_pal
      
      p2 <-  ggplot(df_prueba_2_label_x) + 
        geom_label(data = df_prueba_2_label_x, aes(y = rank_graph, label = temporada), color = df_prueba_2_label_x$my_pal , x = 1, size = 2.9, label.size = 1) +
        theme_void() + theme(legend.position = "none") +
        scale_x_continuous(limits = c(1,1)) +
        scale_color_manual(values = df_prueba_2_label_x$my_pal )
      
      
      
      pp <- p + p2 +  plot_layout(widths = c(6, 1))
      plot(pp)
    }
  }
  
}


# my_vv_choosen = expr(mark_xx)
# my_vv_choosen = expr(velocidad_ms)

#my_graph_03_A(df_edad, pruebas = "5000-metres",  generos = "men", my_pal = my_pal, vv_choosen = my_vv_choosen)

#my_graph_03_A(df_edad, pruebas = "javelin-throw",  generos = "men", my_pal = my_pal, vv_choosen = my_vv_choosen)

#- si quieres volver a calcular el rank (que creo que no hace falta)
# df_edad <- my_rank_atleta_prueba_temporada(df_edad)
# df_edad <- my_rank_atleta_prueba(df_edad)

# names(df_edad)
# zz <- feo %>% filter(prueba == "100-metres") %>% 
#   filter(genero == "men") %>% 
#   select(1, 2, edad, velocidad_ms, everything())


#- creo ff. para graficar el CUARTO gráfico: x = edad_discreta, y = percent_edad_temporada, (una curva por temporada)

#- 4A es con tidyheatmaps
#- creo ff. para graficar el CUARTO gráfico: x = edad_discreta, y = percent_edad_temporada, (una curva por temporada)
my_graph_04A <- function(dfxx, pruebas = NULL, generos = NULL,  my_pal = my_pal){
  if (is.null(pruebas)) {
    pruebas <- levels(dfxx$prueba.f)
  }
  if (is.null(generos)) {
    generos <- c("men", "women")
  }
  for (ii in pruebas) {
    #print(ii)
    df_prueba <- dfxx %>% filter(prueba.f == ii)
    for (jj in generos) {
      #print(jj)
      subtitulo <- glue::glue("Prueba:", ii, " .Genero:", jj)
      df_prueba_2 <- df_prueba %>% filter(genero == jj)
      #df_prueba_2_label <- df_prueba_2 %>% group_by(temporada, genero, prueba) %>% 
      #filter(rank_atleta_prueba == max(rank_atleta_prueba)) %>%   #- se supone q solo esta el ultimo
      #ungroup()   
      df_prueba_2 <- df_prueba_2 %>% dplyr::arrange(desc(temporada), edad_discreta) #- para q 2021 salga arriba
      
      
    p <- tidyheatmaps::tidy_heatmap(df_prueba_2 ,
                                 rows = temporada,
                                 columns = edad_discreta,
                                 values = percent_edad_temporada,
                                 scale = "none", # "row", "column" and "none"
                                 border_color = "white",
                                 angle_col = c("0"),
                                 display_numbers = TRUE
                                 #annotation_col = c(sample_type, condition, group),
                                 #annotation_row = c(is_immune_gene, direction),
                                 # gaps_row = direction,
                                 #gaps_col = group
      ) 
cat(subtitulo)  
p
    }
  }
  
}


# my_graph_04A(df_edad_rr, pruebas = "100-metres",  generos = "women", my_pal = my_pal)
# my_graph_04A(df_edad_rr, pruebas = "shot-put",  generos = "women", my_pal = my_pal)

# my_graph_04A(df_edad_rr,  my_pal = my_pal)



#- ahora el grafico 4B
my_graph_04B <- function(dfxx, pruebas = NULL, generos = NULL,  my_pal = my_pal){
  if (is.null(pruebas)) {
    pruebas <- levels(dfxx$prueba.f)
  }
  if (is.null(generos)) {
    generos <- c("men", "women")
  }
  for (ii in pruebas) {
    #print(ii)
    df_prueba <- dfxx %>% filter(prueba.f == ii)
    for (jj in generos) {
      #print(jj)
      subtitulo <- glue::glue("Prueba:", ii, " .Genero:", jj)
      df_prueba_2 <- df_prueba %>% filter(genero == jj)
      #df_prueba_2_label <- df_prueba_2 %>% group_by(temporada, genero, prueba) %>% 
      #filter(rank_atleta_prueba == max(rank_atleta_prueba)) %>%   #- se supone q solo esta el ultimo
      #ungroup()   
      p <- df_prueba_2 %>% 
        #- y = velocidad_ms
        ggplot(aes(x = edad_discreta, 
                   y = percent_edad_temporada , 
                   color = temporada, group = temporada)) + 
        scale_color_manual(values = my_pal) +
        geom_line() +
        theme_minimal() + 
        #theme(legend.position = "none") +  
        labs(title = "% de grupos de EDAD", subtitle = subtitulo)
      
      plot(p)
    }
  }
  
}


# my_graph_04B(df_edad_rr, pruebas = "100-metres",  generos = "women", my_pal = my_pal)
# my_graph_04B(df_edad_rr, pruebas = "shot-put",  generos = "women", my_pal = my_pal)

#my_graph_04B(df_edad_rr,  my_pal = my_pal)


