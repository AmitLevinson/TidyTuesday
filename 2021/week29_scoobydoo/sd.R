library(dplyr)
library(tidyr)
library(ggplot2)
library(ggstream)
library(ggstream)
library(lubridate)
library(stringr)
library(ggrepel)
library(ggtext)
library(extrafont)


scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')


sayings_agg <- scoobydoo %>% 
    pivot_longer(cols = c(jeepers, jinkies, my_glasses, zoinks:scooby_doo_where_are_you) , names_to = "saying") %>% 
    select(network, season, date_aired, saying, value) %>% 
    mutate(value = as.numeric(na_if(x = value, y = "NULL"))) %>% 
    group_by(date_year = year(date_aired), saying) %>% 
    # Average number of quotes per aired episode
    summarise(n_episodes = n(),
              total_said = sum(value)/n(),
              .groups = 'drop') 
    
sayings_clean <-  sayings_agg %>% 
  mutate(saying = str_replace_all(saying, "_", " "),
         saying = str_to_sentence(saying),
         saying = ifelse(str_detect(saying, "Scooby"), "Scooby doo\nwhere are you", saying))

# Color palette
scooby_color_palette <- c("#128a84", "#79af30", "#bb5c37", "#4b0055", "black", "#C19C72")
# Give color palette names of groups
names(scooby_color_palette) <- unique(sayings_clean$saying)

sayings_labels <- sayings_clean %>% 
  filter(date_year == min(date_year)) %>% 
  mutate(y_pos = c(-1.05,1.25,1.1,1,-1.15,0.2))


ggplot(data = sayings_clean, aes(x= date_year, y = total_said, fill = saying,  group = saying, label = saying))+
  geom_stream(extra_span = .2, sorting = "onset")+
  # Add white lines for missing values
  geom_segment(data = filter(sayings_clean, is.na(total_said)), aes(x = date_year, xend = date_year, y = -10, yend = 10), color = "white", size = 1, alpha = 0.7)+
  scale_x_continuous(breaks = seq(1970,2020,10), labels = c("1970", "80s", "90s", "2000", "10s", "20s"), limits = c(1966,2021))+
  geom_text_repel(data = sayings_labels, aes(x = date_year, y = y_pos, color = saying), 
                  fontface = "bold", xlim = c(1965,1967), hjust = 0, size = 3,
                  direction = "y", segment.alpha = 0.5)+
  scale_fill_manual(values = scooby_color_palette)+
  scale_color_manual(values = scooby_color_palette)+
  guides(color = "none", fill = "none")+
  labs(
    title = "Scooby Doo Quotes",
    subtitle = "Average number of Scooby Doo quotes across episodes (or movies) aired that year. For example, <span style='color:#C19C72'><b>Zoinks was noted an<br>average of 3 times across 36 episodes in 1981, whereas 14 times in the one episode aired in 1998.</b></span>White lines represent<br>missing values for that year.",
    caption = "Data: | Visualization: @Amit_Levinson"
  )+
  theme_void()+
  theme(
    text = element_text(family = "Raleway"),
    plot.title = element_text(size = 18, family = "Flowers Kingdom"),
    axis.text.x = element_text(color = "gray15", size = 10),
    plot.subtitle = element_markdown(lineheight = 1.1),
    plot.caption = element_text(color = 'gray35', vjust = -2),
    plot.margin = margin(4,4,4,8)
  )+
  coord_fixed(clip = 'off')


ggsave("sd.png", width = 11, height = 7)
