library(tidyverse)
library(png)
library(gridGraphics)

park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")

park_visits$year <- as.integer(park_visits$year,na.omit = T)
park_visits$visitors <- as.integer(park_visits$visitors,na.omit = T)
park_visits$unit_type <-  as.factor(park_visits$unit_type)

#cleaning the data
National_parks <- park_visits %>% 
  filter(unit_type == "National Park") %>% 
  group_by(year) %>% 
  mutate(average_per_year = mean(visitors)) %>%
  select(year, average_per_year) %>%
  summarise(Visitors_Parks = mean(average_per_year)) %>% 
  mutate(Visitors_Parks = Visitors_Parks/1000) %>% 
  na.omit

#preparing icons for later insertion; 
img1 <- readPNG("sun.png")
fig1 <- rasterGrob(img1)

img2 <- readPNG("cloud.png")
fig2 <- rasterGrob(img2)

img2a <- readPNG("cloud.png")
fig2a <- rasterGrob(img2a)

img3 <- readPNG("stickman.png")
fig3 <- rasterGrob(img3)

#plotting
ggplot(National_parks, mapping = aes(x = year, y = Visitors_Parks)) +
  geom_step(color = "green4") +
  geom_linerange(data = National_parks, aes(x = year, ymin = 0, ymax = Visitors_Parks), 
                 color = 'green4') +
  #adding labels:
  labs(x = "", y = " ", title = "Visitors in National Parks",
       subtitle = "Average per year (Thousands)", caption = "data from: data.world | AmitL")+
  
  #changing background for a nice clear sky
  theme(panel.background = element_rect(fill = 'lightskyblue2', color = 'lightblue', size = 0.5),
        panel.grid.major = element_line(color = 'white', linetype = 'dashed'),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.1, linetype = "solid", colour = "black")) +

  #playing with some icons
  annotation_custom(fig2, xmin=1975, xmax=1985, ymin=1100, ymax=1400) +
  annotation_custom(fig2a, xmin=1925, xmax=1935, ymin=900, ymax=1200) +
  annotation_custom(fig1, xmin=1899, xmax=1920, ymin=1200, ymax=1400)+
  annotation_custom(fig3, xmin=1915, xmax=1925, ymin=50, ymax=200)

ggsave("National Parks.png", width = 10, height = 5)