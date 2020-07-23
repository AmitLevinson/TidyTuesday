library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2020-07-14')
tt <- tuesdata$astronauts

tt %>% 
  mutate(decade = year_of_mission %/% 10 * 10) %>% 
  count(decade, nationality, sex) %>%
  group_by(decade) %>% 
  mutate(pct = n / sum(n)*100) %>%
  ungroup() %>% 
  ggplot(aes(x = decade, y= n, fill = sex))+
  geom_col()


tt %>% 
  count(nationality, sort = T)


### Hours by year?

tt %>% 
  mutate(decade = year_of_mission %/% 10 * 10) %>%
  group_by(year_of_mission) %>% 
  summarise(mean_h = mean(hours_mission)) %>%
  ggplot(aes(x = year_of_mission, y = mean_h))+
  geom_line()+
  theme_minimal()


tt %>% 
  group_by(name) %>%
  filter(n() > 2) %>% 
  mutate(mean_h = mean(total_hrs_sum))


tt %>% 
  group_by(name) %>%
  filter(n() > 5) %>%
  summarise(mean_h = mean(total_hrs_sum))
