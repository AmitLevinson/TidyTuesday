library(tidyverse)
library(janitor)
library(ggrepel)
library(ggthemr)
library(ggthemes)
ggthemr("flat", layout="clear", spacing=2)
ggthemr_reset()

pbarstool <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_barstool.csv")

#cleaning and renaming data
pbarstool <- pbarstool %>% 
  clean_names() %>% 
  rename(
    total_average = review_stats_all_average_score, 
    community_average = review_stats_community_average_score,
    critic_average = review_stats_critic_average_score,
    dave_average = review_stats_dave_average_score,
    community_count = review_stats_community_count,
    critic_count = review_stats_critic_count,
    ) %>% 
  mutate(s_deviation = sd(c(community_average, dave_average, critic_average))) %>% 
  gather("which_group", "score", community_average:dave_average) %>% 
  mutate(which_group = factor(which_group, levels = c("dave_average", "critic_average", "community_average"))) %>% 
  


#Filtering down to Resturants with ratings from all three groups
pcooked <- pbarstool %>% select(name, price_level, provider_rating, community_average, critic_average,
    dave_average, community_count, critic_count, total_average) %>% 
  filter(community_average != 0 & dave_average != 0 & critic_average != 0) %>% 
  group_by(name) %>% 
  mutate(s_deviation = sd(c(community_average, dave_average, critic_average))) %>% 
  gather("which_group", "score", community_average:dave_average) %>% 
  mutate(which_group = factor(which_group, levels = c("dave_average", "critic_average", "community_average"))) %>% 
  arrange(desc(s_deviation))


#taking most differentiated ratings
anomalies <- pcooked %>% 
  filter(s_deviation >= 2) %>% 
  ungroup() %>% 
  #re ordering labels so that dave's ratings is on the left
  mutate(plot_label = ifelse(which_group == "dave_average", name, "")) 


p <- ggplot(anomalies, aes(which_group, Score, group = name))+
  geom_point(shape = 19, size = 3, color = "brown")+
  geom_path(color = "black", size = 0.75, linejoin = "mitre", linetype = 1, alpha = 0.5)+
  labs(title = "Pizzaerias with highest difference in rating", 
       caption = "*data is not weighted | Data from: | AmitL",
       x = NULL, y = "Score")


p + 
  theme(
  plot.background = element_blank(),
  panel.background = element_rect(fill= "white"),
  axis.ticks.x=element_blank(),
  axis.line = element_line(size = 1, linetype = "solid", colour = "black")
) +
  geom_label_repel(aes(label = plot_label),
  point.padding = unit(1, "lines"),
  segment.color = 'black',
  segment.size = 1,
  nudge_x = -2
  )


view(pcooked)


