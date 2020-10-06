library(tidyverse)
library(gggibbous)
library(scales)
library(extrafont)
library(here)

tournament <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv')

tournament %>% 
  filter(tourney_finish == "Champ")

# Extract winner from each year
champs <- tournament %>% 
  group_by(year) %>% 
  filter(tourney_finish == "Champ") %>% 
  select(year, school) %>% 
  mutate(champ = TRUE)

# Extract maximum number of game won each year
max_w <- tournament %>% 
  group_by(year) %>% 
  filter(full_w == max(full_w)) %>% 
  select(year, school)

joined <- left_join(max_w, champs) %>% 
  group_by(year) %>% 
  # If no winners this should amount to NA, if there are winner (1 winner per year) it should amount to 1
  summarise(champ = sum(champ)) %>% 
  # Count how many from each category
  count(champ) %>% 
  mutate(pct = n/sum(n),
         champ = ifelse(champ == 1, "Yes", "No"),
         right = c(TRUE, FALSE),
         nudge = c(0.05, -0.05))

t_family <- "Roboto Condensed"

ggplot(joined, aes(x= 1, y = 1))+
  geom_moon(aes(ratio = pct, right = right, fill = right), size = 140, show.legend = FALSE)+
  # Add 54% annotations
  annotate("text", y = 1.025, x = 1.017, label = percent(joined[[1,3]]), color = "black", size = 8, family = t_family)+
  annotate("text", y = 0.99, x = 1.01, label = "of tournaments,\nthe NCAA champion\nteam had the most\nor tied wins", color = "black", size = 5, hjust = 0, family = t_family)+
  # Add the 46% annotations
  annotate("text", y = 1.025, x = 0.9575, label = percent(joined[[2,3]]), color = "orange", size = 8, family = t_family)+
  annotate("text", y = 0.99, x = 0.95, label = "of tournaments,\nlosing teams\nhad more wins\nthan the champion", color = "orange", size = 5, hjust = 0, family = t_family)+
  scale_fill_manual(values = c("black", "Orange"))+
  # Create boundaries with limits
  xlim(0.9,1.1)+
  ylim(0.9,1.1)+
  labs(title = "Wins don't guarantee the NCAA championship",
       subtitle = "To win the NCAA division I women's basketball championship, it's not enough to only collect the most number of wins.\nIn only 54% (20 years) of tournaments the champion team achieved or tied with the most wins for that year.\nIn 46% (17 years) other teams in the tournament surpassed the champion in total number of wins.\n", caption = "Data: FiveThirtyEight\nVisualization: @Amit_Levinson")+
  theme_void()+
  theme(
    text = element_text(family = "Roboto Condensed"),
    plot.title = element_text(hjust = 0.5, size = 22),
    plot.subtitle = element_text(color = "gray35", hjust = 0.5),
    plot.caption = element_text(hjust = 0, color = "gray35"),
    plot.margin = margin(4,4,4,4, unit = "mm"))

ggsave("ball_plot.png", path =  here("2020", "week_41"), dpi = 500)


