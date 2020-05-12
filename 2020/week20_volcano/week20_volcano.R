volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
library(extrafont)
library(tidyverse)
library(ggtext)
library(ggpomological)
volcano %>% filter(str_detect(volcano_name, "Tambora")) %>% View()
volcano_loc_n <- events %>%
  filter(event_type == "Explosion") %>%
  distinct(volcano_name, eruption_number, event_type, .keep_all = T) %>%
  count(volcano_name, sort = T, name = "n_explosions") %>%
  inner_join(volcano) %>%
  mutate(highlight_point = ifelse(.$volcano_name %in% names_annotation, "yes", "no"))

names_annotation <- c("Etna", "Asosan", "Tambora")

arrange(volcano, -(population_within_5_km))
arrange(eruptions, -(vei))

# labels:
# most exploded:
volcano_loc_n %>%
    filter(n_explosions == max(n_explosions))

eruptions %>% arrange(-vei)

# oldest explosion:

# one options: plot of people living near by and number of explosions
volcano_loc_n %>% 
  ggplot(data = ., aes(x = population_within_5_km, y= n_explosions, label = volcano_name))+
  geom_point(color = "red", alpha = 2/10)+
  geom_point(data = subset(volcano_loc_n, highlight_point == "yes"), color = "red", alpha = 7/10)+
  scale_x_log10(labels = comma)+
  labs(title = paste0("<span style='color:red'>**Living with Volcanos**</span>\n Graph shows recorded volcano <span style='color:red'>explosions</span", 
                      ""), subtitle = "")+
  theme(text = element_text(family = "JetBrains Mono"),
        plot.title = element_markdown(),
    plot.background = element_rect(fill = "grey50"),
    panel.background = element_rect(fill = "grey50"),
    panel.grid = element_blank(),
    axis.text = element_text(color = "grey25", size = 10),
    axis.ticks = element_blank()
  )+
  annotate(geom = "curve", x = 45, xend = 75, y = 187, yend = 193,
           curvature = -.2,color = "grey25", size = 0.75, arrow = arrow(length = unit(1, "mm")))+
  annotate("text", x = 15, y = 181, label = "Etna Volcano (Italy) exploded\n193 times. Today 78 people\nlive in a 5k radius",
           color = "grey25", hjust = 0, size = 3.5, family = "JetBrains Mono")+
  annotate(geom = "curve", x = 110000, xend = 76000, y = 160, yend = 165.5,
           curvature = -.2, color = "grey25", size = 0.75, arrow = arrow(length = unit(1, "mm")))+
  annotate("text", x = 110000, y = 153, label = "Asosan Volcano (Japan) exploded\n166 times with the last explosion\nin 2020. Today 57,000 people\nlive in a 5k radius.",
           color = "grey25", hjust = 0, family = "JetBrains Mono", size = 3.5)+
  annotate(geom = "curve", x = 12000, xend = 4160, y = 42, yend = 6.8,
           curvature = .2, color = "grey25", size = 0.9, arrow = arrow(length = unit(1, "mm")))+
  annotate("text", x = 12500, y = 40, label = "Tambora's 1815 eruption is the most\npowerful explosion recorded in human history. Today 4156\npeople live in a 5k radius.",
           color = "grey25", hjust = 0, size = 3.5, family = "JetBrains Mono")

fonts()
 
labs(title = "**Top 10 Broadway Grosses**<br><span style='font-size:22pt'>**Number of years screened: {closest_state}**</span>",
     
annotate(
    geom = "curve", x = 4, y = 35, xend = 2.65, yend = 27, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 4.1, y = 35, label = "subaru", hjust = "left")

ggplotly(p)
library(plotly)
?geom_mark_circle

volcano_loc_n %>% 
  mutate(decade = ifelse(last_eruption_year >= 1920, "1", "0")) %>% 
  count(decade , sort = T) %>% 
  mutate(decade_perc = n / sum(n)*100)

library(scales)
events

events %>%
    filter(event_type == "Explosion")
    count(event_type, sort = T)
