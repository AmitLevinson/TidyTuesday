library(tidyverse)

tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')


summary(tbi_age)
glimpse(tbi_age)
View(tbi_age)

tbi_year %>% 
  ggplot(aes(x = year, y = rate_est))+
  geom_col()+
  facet_grid(injury_mechanism ~ type)



tbi_year %>% 
  filter(type == "Deaths", year =="2014") %>% 
  count(, wt= number_est) %>% 
  summarise(mean(n))

tbi_year %>%
  count(type)



tbi_age %>% 
  count(age_group)

tbi_age %>% 
  filter(type == "Emergency Department Visit", age_group != "0-17") %>% 
  count(injury_mechanism, wt = rate_est, sort = T)

tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')


glimpse(tbi_military)

tbi_military %>% 
  ggplot(aes(x = ))
