transit_cost <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')


transit_cost  %>% 
  filter(real_cost != 0) %>% 
  mutate(
    length_work = as.numeric(end_year) - as.numeric(start_year),
    real_cost = as.numeric(real_cost)) %>% 
  ggplot()+
  geom_point(aes(x = length_work, y = stations))
  scale_x_log10()

class(transit_cost$real_cost)
