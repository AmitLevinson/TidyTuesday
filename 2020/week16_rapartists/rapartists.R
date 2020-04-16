library(tidyverse)
library(ggalt)
library(ggtext)
library(RColorBrewer)
library(extrafont)

rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

# Get data frame of artists that are playing alone
df_points_alone <- rankings %>% 
  filter(!str_detect(artist, "ft\\. |& | and ")) %>%
  group_by(artist) %>% 
  mutate(count_alone = n(),
         mean_alone = mean(points)) %>% 
  distinct(artist, mean_alone, count_alone)

# Get data frame for artists that are playing together
df_points_together <-  rankings %>%  
  filter(str_detect(artist,"ft\\.")) %>%
  separate(artist, sep = " ft\\. | & | and ", into = c("artist_1", "artist_2", "artist_3", "artist_4")) %>% 
  pivot_longer(cols = artist_1:artist_4, values_to = "artist") %>% 
  drop_na() %>% 
  select(artist, points) %>% 
  group_by(artist) %>%
  mutate(count_together = n(),
          mean_together = mean(points)) %>% 
   distinct(artist, mean_together, count_together)

# Join the tables:
df_join <- inner_join(df_points_alone, df_points_together)

# Look at table with more than two observations:
# df_join_under_2 <- df_join %>%
# filter(count_alone >= 2 , count_together >= 2)

# Choose color palette
brewer.pal(n = 8, name = "Dark2")

ggplot(df_join, aes(x = mean_alone, xend = mean_together, y = fct_reorder(artist, mean_alone)))+
  # use `geom_dumbbell` from the ggalt package for a dumbbll plot
  geom_dumbbell(color = "gray70", size = 2, colour_x = "#1B9E77", colour_xend = "#D95F02")+
  labs(title = "Total points awarded to artists recording songs <span style='color:#D95F02'>together</span> vs recording songs <span style='color:#1B9E77'>alone</span>",
       subtitle = "Out of the 301 BBC Music song list, 25 artists have observations of songs alone and songs together. Half of the artists have\nmore than two alone & two together. For example, The Notorious B.I.G has 4 songs together and 4 songs alone.",
       caption = "Data: BBC Music | @Amit_Levinson",
       x = "Mean points")+
  theme_minimal()+
  theme(text = element_text(family = "Roboto Condensed"),
        axis.line = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 11),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_markdown(face= "bold", size = 18),
        plot.subtitle = element_text(color = "gray55"),
        plot.caption = element_text(size = 8, face = "italic"))

ggsave("rap_points.png", width = 11, height = 8, dpi = 1600)
