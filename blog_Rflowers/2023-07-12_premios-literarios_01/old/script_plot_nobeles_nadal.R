#- grafico de los Nobeles : https://twitter.com/c_gebhard/status/1510146260052766724

# install.package('remotes')
# remotes::install_github('coolbutuseless/cssparser')
# remotes::install_github('coolbutuseless/svgparser')
# remotes::install_github('coolbutuseless/ggsvg')
library(tidyverse)
library(ggbeeswarm)
library(ggsvg)
library(ggtext)
library(fontawesome)

library("showtext")
font_add_google("Open Sans")
font_add_google("Bitter")
showtext_auto()

# nobel_url <- "http://api.nobelprize.org/2.1/laureates?limit=1000&format=csv"
# curl::curl_download(url = nobel_url, destfile = "./datos/laureates.csv")

laureates <- read.csv("./datos/laureates.csv")
df_orig <- rio::import(here::here("datos", "df_TODO_ok.rds"))
df <- df_orig

unique(df$premio)
# read data and remove Organisations
df <- df  %>% filter(!genero %in% c("NO", "Organización")) %>% 
  mutate(flabel = ifelse(genero == "H",
      fontawesome::fa("male", fill = "#b9b9b8", fill_opacity = 0.7),
      fa("female", fill = "#9b332b", fill_opacity = 0.8))) %>% 
      mutate(category = factor(premio, levels = unique(df$premio)))
laureates <- df
p
p <- laureates |> 
  ggplot(aes(x=category, y = ano)) +
  geom_point_svg(aes(svg = flabel), size = 2.5,
    position = position_quasirandom(bandwidth = 0.1, varwidth = TRUE)
  ) +
  labs(
    title = "<span style = 'font-size:64pt; font-family:Bitter;'><b>Missing not at random?</b></span>",
    subtitle = "<span style = 'font-size:30pt; font-family:Open Sans;'>The number of <span style = 'color: #9b332b;'><b>female</b></span> Nobel Prize laureates is astonishingly low, when compared to male<br>laureates. In recent years some fields show a trend in the right direction,<br>but there is still an ignobel gap in terms of gender equality.</span>",
    caption = "<span style = 'font-size:16pt; font-family:Open Sans;'>Note: Awarded Organisations are not included in this data.<br>DataViz by @c_gebhard | <b>#30DayChartChallenge 2022, Day 02</b> | Data by NobelPrize.org released under CC0 license</span>",
    y = "<span style = 'font-size: 20pt; font-family:Open Sans;'><b>Award Year</b></span>"
  ) +
  scale_x_discrete(labels = c(
    "<span style = 'font-size: 20pt;font-family:Open Sans;'><b>Nacional Letras</b></span>",
    "<span style = 'font-size: 20pt;font-family:Open Sans;'><b>Economic<br>Cervantes</b></span>",
    "<span style = 'font-size: 20pt;font-family:Open Sans;'><b>Nacional Narrativa</b></span>",
    "<span style = 'font-size: 20pt;font-family:Open Sans;'><b>Nacional Poesía</b></span>",
    "<span style = 'font-size: 20pt;font-family:Open Sans;'><b>Nacional Ensayo</b></span>",
    "<span style = 'font-size: 20pt;font-family:Open Sans;'><b>Premio Planeta</b></span>",
    "<span style = 'font-size: 20pt;font-family:Open Sans;'><b>Princesa de las Letras</b></span>",
    "<span style = 'font-size: 20pt;font-family:Open Sans;'><b>Premio Nadal</b></span>" ) ) +
  scale_y_discrete(
    breaks = c(1900, 1925, 1950, 1975, 2000, 2025),
    labels = c(
      "<span style = 'font-size: 20pt;font-family:Open Sans;'>1900</span>",
      "<span style = 'font-size: 20pt;font-family:Open Sans;'>1925</span>",
      "<span style = 'font-size: 20pt;font-family:Open Sans;'>1950</span>",
      "<span style = 'font-size: 20pt;font-family:Open Sans;'>1975</span>",
      "<span style = 'font-size: 20pt;font-family:Open Sans;'>2000</span>",
      "<span style = 'font-size: 20pt;font-family:Open Sans;'>2025</span>")
  ) +
  coord_flip() +
  annotate(
    geom = "richtext",
    label = "<span style='font-family: Open Sans; font-size: 16pt;'><b>Marie Curie</b><br> was the only woman to be<br>awarded two Nobel Prizes.</span>",
    x = "Economic Sciences",
    y = 1925,
    hjust = 0,
    lineheight = 0.6,
    fill = NA,
    label.color = NA
  ) +
  annotate(
    geom = "curve", x = 2.1, y = 1923, xend = 2.7, yend = 1905,
    curvature = -.3, arrow = arrow(length = unit(1, "mm")),
    color = "#999999"
  ) +
  annotate(
    geom = "curve", x = 1.9, y = 1923, xend = 1.4, yend = 1911,
    curvature = .3, arrow = arrow(length = unit(1, "mm")),
    color = "#999999"
  ) +
  theme_classic(base_family = "Open Sans") +
  theme(
    text = element_text(family = "Open Sans"),
    plot.title.position = "plot",
    plot.title = element_markdown(lineheight = 1.2),
    plot.subtitle = element_markdown(lineheight = 0.8),
    plot.caption = element_markdown(lineheight = 0.8),
    panel.grid.major.x = element_line(color = "#444444", size = 0.1),
    panel.grid.minor = element_blank(),
  ) +
  theme(axis.text.x = element_markdown(lineheight = 1),
        axis.text.y = element_markdown(lineheight = 1),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_markdown(),
        axis.title.y = element_blank(),
        axis.line = element_blank(),
        plot.margin = margin(10,10,10,10,"pt")
  )

p
#ggsave(here::here("..", "plots", "2022_02.png"), plot = p, height = 5.5, width = 5.5, dpi = "retina")