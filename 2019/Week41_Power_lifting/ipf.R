library(tidyverse)
library(ggthemr)
library(gridExtra)
ggthemr('grape')

#loading data
ipf_lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")

#cleaning the data
df <- ipf_lifts %>% 
  mutate(year = as.numeric(format(date, '%Y'))) %>% 
  select(-event, -division, -federation, -date) %>% 
  gather(activity, weight, best3squat_kg:best3deadlift_kg) %>% 
  group_by(year)

#filtering to obtain max weight lifted 
max_score_all <- df %>%
  group_by(sex, activity, year) %>%
  filter(place != "DD" & place != "DQ" & !is.na(weight) & weight == max(weight, na.rm = T),
         year >= 1980) %>% 
  ungroup() %>% 
  group_by (name) %>% 
  mutate(activity = recode(activity, "best3bench_kg" = "Bench", 
  "best3deadlift_kg" = "Deadlift", "best3squat_kg" = "Squat"),
  sex = recode(sex, "F" = "Female", 'M' = "Male")) %>%
  arrange(desc(year))

#checking to see if the amount of max makes sense. the max
#function had some ties in some cases, but i decided to leave it as is and 
#give the participants their respect :)
check <- max_score_all %>% 
  group_by(year) %>% 
  summarise(total = n())

#plotting the first graph
g_max <- ggplot(max_score_all, aes(x = year, y = weight, color = activity,shape = activity))+
  geom_point()+
  geom_line(aes(color = activity), size = 1)+
  facet_grid(. ~ sex)+
  labs(y = "Weight (kg)", x = NULL,
       title = "Max weight lifted across all of IPF tournaments",caption = "Data from: Open Powerlifting | AmitL")+
  theme(
    plot.caption = element_text(hjust = 0, size = 8, face= "italic"),
    plot.title = element_text(size = 18),
    strip.text = element_text(
      size = 14, face = "bold"),
    legend.position='top', 
    legend.justification='left',
    legend.direction='horizontal',
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.spacing.x = unit(0.4, 'cm'),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(2,"line")
  )

#aggregating number of max achieved
competition_name <- max_score_all %>% 
  group_by(sex, name) %>% 
  summarise(n = n()) %>% 
  group_by(sex, n) %>% 
  summarise(total = n())

#plotting number of max achieved
g_rewin <- ggplot(competition_name, aes(x = n, y= total, fill = sex)) +
  geom_bar(stat = "identity")+
  facet_grid(sex ~ .)+
  scale_x_continuous(breaks = seq(0,12,1))+
  scale_y_continuous(breaks = seq(0,50,5))+
  labs(y= "# of participants achieving that max", x = "Number of max achieved", 
       title = "How do the max \nachievements \ndistribute \nacross participants?",
       subtitle = "",
       caption = "")+
  theme(
    strip.text = element_blank(),
    plot.title = element_text(size = 12),
    legend.title = element_blank(),
    legend.position='top',
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    legend.spacing.x = unit(0.4, 'cm'),
    legend.text = element_text(size = 12),
  )

#binding the two graphs and plotting with the one bigger than the other.
g <- arrangeGrob(g_max, g_rewin, nrow = 1, widths = c(2,0.75))
ggsave(g, filename = "Max_lift.jpeg", width =10, height = 6)
