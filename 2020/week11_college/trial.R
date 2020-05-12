library(tidyverse)
tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')
tuition_income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_income.csv') 
salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')
historical_tuition <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/historical_tuition.csv')
diversity_school <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')

tuition_cost %>% 
group_by(state, type) %>% 
  summarise(avg_tuition = mean(in_state_tuition)) %>% 
  arrange(-avg_tuition)

tuition_income


tuition_cost

new <- salary_potential %>% 
  mutate(diff = abs(mid_career_pay-early_career_pay))


salary_potential %>% 
  right_join(tuition_income)


historical_tuition %>% 
  mutate(new_year = str_extract(year, pattern = "\\d{4}")) %>% 
  #filter(tuition_type == "All Constant") %>% 
  ggplot(aes(x = new_year, y= tuition_cost, group = type, color = type))+
  geom_line()+
  facet_wrap(~ tuition_type)
?facet_wrap
View()  
count(new_year)
  
?str_extract
separate(col = year, into = "new_year", sep = "([1-9])\\1\\1\\1\\1")

?separate

ggplot(aes(x = stem_percent, make_world_better_percent))+
  geom_point()
  
max(salary_potential$make_world_better_percent, na.rm = T)
  
  summarise(avg_p = mean(make_world_better_percent)) %>% 
  
  
  View()
  arrange(-avg_p)

right_join(tuition_cost, by = "name")

cor.test(new$mid_career_pay, new$make_world_better_percent)
ggplot(salary_potential,aes(mid_career_pay, make_world_better_percent, fill = ifelse(rank < 40, "red", "black")))+
  geom_point()

?cor.test
tuition_cost

historical_tuition %>% 
  count(year)



tuition_cost %>% 
  right_join(diversity_school) %>%
  mutate(id = row_number()) %>% 
  pivot_wider(names_from = "category", values_from = "enrollment") %>% 
  select(c(1,2,4,"total_enrollment":"Total Minority"))
  View()
?pivot_wider
View()

diversity_school %>% 
  drop_na() %>% 
  pivot_wider(names_from = "category", values_from = "enrollment")
