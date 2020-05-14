library(extrafont)
library(tidyverse)
library(RColorBrewer)
library(scales)

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')

names_annotation <- c("Etna", "Asosan", "Tambora", "Fournaise, Piton de la")

volcano_loc_n <- eruptions %>%
  distinct(volcano_name, eruption_number, .keep_all = T) %>%
  filter(eruption_category == "Confirmed Eruption") %>% 
  count(volcano_name, sort = T, name = "n_explosions") %>%
  inner_join(volcano) %>%
  mutate(highlight_point = ifelse(volcano_name %in% names_annotation, "yes", "no"))


volcano_loc_n %>% 
  ggplot(data = ., aes(x = population_within_5_km, y= n_explosions, label = volcano_name))+
  geom_point(color = "#E31A1C", alpha = 3/10, size = 4)+
  geom_point(data = subset(volcano_loc_n, highlight_point == "yes"), color = "#E31A1C", size = 4)+
  scale_x_log10(labels = comma)+
  labs(title = "Living with volcanoes",
       subtitle = "Number of confirmed volcano eruptions with population (log scale) currently living within a 5 kilomoter distance.\nRecorded eruptions range from thousands of years back until a month ago, varying in their degree of explosion.",
       caption = "Source: Smithsonian | @Amit_Levinson",
       y = "Number of eruptions",
       x = "\nPopulation (log) within 5 km")+
  theme(text = element_text(family = "Roboto Condensed"),
        #plot.title = element_markdown(color = "#FB9A99"),
    plot.title = element_text(face = "bold", size = 45),
    plot.subtitle = element_text(size = 16, color = "grey10"),
    plot.caption = element_text(size = 9, color = "grey25"),
    plot.background = element_rect(fill = "grey50"),
    panel.background = element_rect(fill = "grey50"),
    panel.grid = element_blank(),
    axis.text = element_text(color = "grey25", size = 12),
    axis.title = element_text(color = "grey15", size = 14, hjust = 1),
    axis.ticks = element_blank(),
    plot.margin = margin(4,4,2,4, unit = "mm")
  )+
  annotate(geom = "curve", x = 45, xend = 72, y = 191, yend = 196,
           curvature = -.2,color = "grey35", size = 0.75, arrow = arrow(length = unit(1.5, "mm")))+
  annotate("text", x = 42, y = 180, label = "Etna (Italy), one of the world's most\nactive volcanoes, erupted 196 times. Today\n78 people live within 5k.",
           color = "grey10", hjust = 0, size = 5, family = "JetBrains Mono")+
  annotate(geom = "curve", x = 105000, xend =	59000, y = 190, yend =189,
           curvature = .2, color = "grey35", size = 0.75, arrow = arrow(length = unit(1.5, "mm")))+ # Fournaise
  annotate(geom = "curve", x = 105000, xend = 80000, y = 179, yend = 171,
           curvature = -.2, color = "grey35", size = 0.75, arrow = arrow(length = unit(1.5, "mm")))+ # Asosan
  annotate("text", x = 111000, y = 187, label = "Piton de la Fournaise (top) and\nAsosan (bottom) are both highly\nactive volcanoes that erupted ~190\ntimes. Today some 55,000 people\nlive within 5k.",
           color = "grey10", hjust = 0, family = "JetBrains Mono", size = 5)+
  annotate(geom = "curve", x = 3000, xend = 4000, y = 70, yend = 9,
           curvature = .2, color = "grey35", size = 0.9, arrow = arrow(length = unit(1.5, "mm")))+
  annotate("text", x = 3000, y = 80, label = "Tambora's (Indonesia) 1815 eruption was the most\npowerful eruption recorded in human history.\nToday 4156 people live in a 5 kilomoter distance.",
           color = "grey10", hjust = 0, size = 5, family = "JetBrains Mono")

ggsave("vc.png", width = 16, height = 10, dpi = 1020)
setwd("2020/week20_volcano")
