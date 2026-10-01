#- reacttable: https://glin.github.io/reactable/index.html
#- reactablefmtr:  https://kcuilla.github.io/reactablefmtr/
#- un ejemplo chulo: https://r-graph-gallery.com/web-interactive-table-with-images-charts-and-more.html
#- un ejmeplo q has de hacer: https://kcuilla.github.io/reactablefmtr/articles/nba_player_ratings.html
#- JS4R: https://book.javascript-for-r.com/widgets-intro-intro

library(reactable)


iris |> reactable(
  defaultPageSize = 20, 
  compact = TRUE,
  searchable = TRUE,
  filterable  = TRUE, 
  resizable = TRUE,
  highlight	= TRUE,
  #bordered = TRUE,
  striped = TRUE,
  theme = reactablefmtr::fivethirtyeight(cell_padding = 1, font_size = 10, header_font_size = 10)
  ) 



# Tabla con el paquete [`reactable`](https://glin.github.io/reactable/index.html)
dicc_show_2 <- df_dicc %>% 
  select(variable, type, nn_unique, unique_values, p_na, p_zeros) 

library(reactable)
reactable::reactable(dicc_show_2, pagination = FALSE, height = 450, filterable = TRUE, searchable = TRUE, highlight = TRUE, columns = list( variable = colDef(
  # sticky = "left",
  # Add a right border style to visually distinguish the sticky column
  style = list(borderRight = "1px solid #eee"),
  headerStyle = list(borderRight = "1px solid #eee")
)),
defaultColDef = colDef(minWidth = 70)
)     
# table_ok <- df_ok %>% 
#   reactable::reactable(defaultPageSize = 36, compact = TRUE,
#                        filterable  = TRUE, 
#                        theme = reactablefmtr::fivethirtyeight(cell_padding = 1, font_size = 11, header_font_size = 13)) 














reactable(iris, columns = list(
  Species = colDef(
    cell = function(value) {
      htmltools::tags$b(value) #- en negrita
    }
  )
))




data <- MASS::Cars93[20:24, c("Manufacturer", "Model", "Type", "Price")]

reactable(
  data,
  searchable = TRUE,
  columns = list(
    Price = colDef(footer = function(values) {
      htmltools::tags$b(sprintf("$%.2f", sum(values)))
    }),
    Manufacturer = colDef(footer = htmltools::tags$b("Total"))
  )
)
