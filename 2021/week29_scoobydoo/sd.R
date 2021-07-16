library(dplyr)
library(tidyr)
library(ggplot2)
library(ggstream)
library(ggstream)
library(lubridate)
library(stringr)
library(ggrepel)
library(ggtext)
scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')





sayings_agg <- scoobydoo %>% 
    pivot_longer(cols = c(jeepers, jinkies, my_glasses, zoinks:scooby_doo_where_are_you) , names_to = "saying") %>% 
    select(network, season, date_aired, saying, value) %>% 
    mutate(value = as.numeric(na_if(x = value, y = "NULL"))) %>% 
    group_by(date_year = year(date_aired), saying) %>% 
    summarise(n_episodes = n(),
              total_said = sum(value)/n(), .groups = 'drop') 
    
sayings_clean <-  sayings_agg %>% 
  mutate(saying = str_replace_all(saying, "_", " "),
         saying = str_to_sentence(saying),
         saying = ifelse(str_detect(saying, "Scooby"), "Scooby doo\nwhere are you", saying))


scooby_color_palette <- c("#128a84", "#79af30", "#bb5c37", "#4b0055", "black", "#C19C72")
names(scooby_color_palette) <- unique(sayings_clean$saying)

sayings_labels <- sayings_clean %>% 
  filter(date_year == min(date_year)) %>% 
  mutate(
         # saying = factor(saying, levels = c("Jeepers", "Jinkies", "My glasses", "Zoinks", "Groovy", "Scooby doo where are you")),
          y_pos = c(-1.05,1.25,1.1,1,-1.15,0.2))
  
# sayings_clean %>% 
#   filter(is.na(total_said)) %>% 
#   mutate(saying = factor(saying, levels = c("Jeepers", "Jinkies", "My glasses", "Zoinks", "Groovy", "Scooby doo where are you")))

library(glue)
ggplot(data = sayings_clean, aes(x= date_year, y = total_said, fill = saying,  group = saying, label = saying))+
  geom_stream(extra_span = .2, sorting = "onset")+
  geom_segment(data = filter(sayings_clean, is.na(total_said)), aes(x = date_year, xend = date_year, y = -10, yend = 10), color = "white", size = 1, alpha = 0.7)+
  scale_x_continuous(breaks = seq(1970,2020,10), labels = c("1970", "'80", "'90", "2000", "'10'", "'20"), limits = c(1966,2021))+
  geom_text_repel(data = sayings_labels, aes(x = date_year, y = y_pos, color = saying), 
                  fontface = "bold", xlim = c(1965,1967), hjust = 0, size = 3,
                  direction = "y", segment.alpha = 0.5)+
  scale_fill_manual(values = scooby_color_palette)+
  scale_color_manual(values = scooby_color_palette)+
  guides(color = "none", fill = "none")+
  labs(
    title = "Scooby Doo Quotes",
    subtitle = "Average number of Scooby do quotes across episodes (or movies) aired that year. The start of the 21st century is characterized with<br>a <b>high occurence of quotes across a low number of episodes aired.</b> For example, <span style='color:#C19C72'><b>Zoinks was noted 14 times in the one<br>episode aired that year.</b></span> White Lines represent missing values for that year.",
    caption = "Data: | Visualization: @Amit_Levinson"
  )+
  theme_void()+
  theme(
    plot.title = element_text(size = 22),
    axis.text.x = element_text(color = "gray15", vjust = 1, size = 10),
    plot.subtitle = element_markdown(lineheight = 1.2),
    plot.caption = element_text(color = 'gray35', vjust = -1)
  )+
  coord_fixed(clip = 'off')

ggsave("sd.png", width = 11, height = 7)
  # 1998-2000 had just a few episodes but with a high occurrence of these sayings.


sayings_agg %>% 
  filter(!is.na(total_said)) %>% 
  arrange(-total_said)

scoobydoo %>% 
  count(year(date_aired), sort = T) %>% View()

  scoobydoo %>% 
    pivot_longer(cols = jeepers:rooby_rooby_roo, names_to = "saying") %>% 
    select(network, season, date_aired, saying, value) %>% 
    filter(year(date_aired) == 1977)
    
  arrange(-total_said)
  
  # How much was the saying said in each network?
  scoobydoo %>% 
    pivot_longer(cols = c(jeepers, jinkies, my_glasses, zoinks:scooby_doo_where_are_you) , names_to = "saying") %>% 
    mutate(value = as.numeric(na_if(x = value, y = "NULL"))) %>% 
    group_by(network, saying) %>% 
    summarise(value = sum(value)) %>% 
    filter(!is.na(value)) %>% 
    arrange(-value) %>% 
    distinct(network)
  