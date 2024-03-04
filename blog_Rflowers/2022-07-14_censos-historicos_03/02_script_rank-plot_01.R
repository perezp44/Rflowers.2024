#- 1er plot de ranks ------------------------------------------------------------
#- https://twitter.com/ThomIvar/status/1518289166026616832
#- https://twitter.com/jburnmurdoch/status/1250538655707430913
library(ftplottools) #- remotes::install_github("Financial-Times/ftplottools")

df_para_labels <- df_table %>% filter(year %in% c(1842, 1860, 1887, 1910, 1930, 1950, 1970, 1991, 2011))
p <- ggplot(df_table, aes(x = year, y = rank_1, color = ine_muni.n.h)) +
  geom_path(data = df_table, aes(x = year, y = rank_1, group = ine_muni.n.h),
            color = ft_colors('black-20'), size = 0.4) +
  geom_path(color = ft_colors('oxford-60'), size = 0.8) +
  #geom_point(data = readRanking(29), color = ft_colors('oxford-60'), size = 1.5) +
  geom_point(data = df_table, color = ft_colors('oxford-60'), size = 1.3) +
  geom_text(data = df_para_labels,
            aes(x = year, y = rank_1, label = rank_1), color = "brown",
            size = 2.4, vjust = 0, hjust = 0, nudge_x = 0.6, nudge_y = -7) +
  scale_x_continuous(breaks = c(1842, 1900, 1950, 2011),
                     labels = c("1842", "1900", "1950",  2011)) +
  #scale_y_continuous(breaks = c(-1, -10, -18), labels = c('1', '10', '18')) +
  scale_y_reverse(breaks = c(1, 10, 20, 30, 40, 50), limits = c(50, 1)) +
  #scale_y_reverse(breaks = c(1, 25, 50, 75, 100), limits = c(100, 1)) +
    #scale_x_continuous(breaks = -30:1) +
  facet_wrap(~ ine_muni.n.h) +
  labs(title = 'Ranking de municipios más poblados',
        subtitle = glue::glue("{min(df$year)} to {max(df$year)}"),
        caption = 'graphic: @pjpv4444\ninspiración: @ThomIvar\ntheme: Financial Times',
        x = 'Periodo', y = 'Ranking') +
  ftplottools::ft_theme() +
  theme(plot.background = element_rect(fill = ft_colors('paper'), color = ft_colors('paper')),
        strip.text = element_text(hjust = 0, color = ft_colors('oxford-60'), face = 'bold'),
        plot.margin = margin(1, 1, 0.25, 1, 'cm'),
        axis.text.y = element_text(angle = 0, hjust = 0, size = 7),
        axis.text.x = element_text(angle = 0, hjust = 0.01, vjust = 0.21, size = 7),
        plot.caption = element_text(size = 6, color = ft_colors('black-30')))
# p
# 
# plotly::ggplotly(p, width = 2000, height = 4000)

#plotly::ggplotly(p, height = 850, tooltip = c("year", "rank_1"))
