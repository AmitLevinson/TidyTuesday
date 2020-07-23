library(tidyverse)
library(lubridate)
library(patchwork)
library(ggmap)
library(RColorBrewer)
library(extrafont)

#I saved the dataset as a csv in order to read it easily
tickets <- read_csv("dataset.csv")

#getting Phily map from google's API
phil_map <- get_map(
  "Philadelphia, Pennsylvania", 
  zoom = 12, 
  maptype = "terrain",
  source = "google")

#First plot
p <- ggmap(phil_map)+
  #Creating a density 'cloud' instead of too many points
  stat_density_2d(data = tickets, aes (x = lon, y= lat, fill = stat(level)),
                  geom = "polygon",
                  alpha = .2,
                  bins = 100,
                  )+
  #Brewing the scale color from default
  scale_fill_gradientn(name = "# Tickets", colors = brewer.pal(7, "YlOrRd"))+
  theme_void()+
  labs(title = "Philadelphia parking tickets issued in 2017")

p <- p + theme(text = element_text(family = "Times New Roman"),
  plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
  panel.border = element_rect(color = "black", size = 1.5, fill = NA),
  legend.position = "bottom")

# Second plot -------------------------------------------------------------

#Let's aggregate tickets by the hour they were issued
by_hour <- 
  tickets %>% 
  mutate(hour = hour(issue_datetime)) %>% 
  group_by(hour) %>% 
  summarise(total_fine = n())

#Merging 0 and 24 hour. Otherwise it'll end at 23:00 (11 pm)
first_hours <- by_hour %>% 
  filter(hour == 0) %>% 
  mutate(hour = 24)

#Now binding the df so that 24 is identical to 0, see Below for Christian's blog
#On solving the issue by binding the additional row
hour_ext <- by_hour %>% 
  rbind(first_hours)

p2 <- ggplot(by_hour, aes(hour, y= total_fine))+
#I initially did a polygon but find the geom_bar a little nicer
#for a 'clock plot'
    #geom_polygon(fill = "#009688", group = 1, alpha = 0.8)+
  #geom_point(color = "#99d5cf", size = 0.1)+
  geom_bar(aes(x = hour, y = total_fine, fill = total_fine),stat = "identity")+
  #going for a same color scheme - dark red for a higher value
  scale_fill_gradientn(colors = brewer.pal(7, "YlOrRd"))+
  scale_x_continuous(breaks = seq(0,24,by = 1))+
  labs(x = NULL, y= NULL, title = "Number of tickets by hour of day",
       caption = "Data: Open Data Philly | @Amit_Levinson")+
#turn the geom_bar to a circular plot  
  coord_polar()+
  theme_minimal()+
  theme(text = element_text(family = "Times New Roman", face = "bold"),
        plot.title = element_text(size = 21, hjust = 0.5),
        plot.caption = element_text(size = 10, face = "italic"),
        plot.background = element_blank(), #(color = "black", size = 1.5),
        axis.text = element_text(size = 12),
        axis.text.y = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0.3,0.3,0.3,0.3), "cm"))
p2
#Using the new patchwork package to bind the two plots!
g <- p + p2
g

ggsave(g, filename = "parking_tickets_hours.png", width = 12, height = 10)

#Thanks to Christian's blog about creating a ploygon to plot on a 'clock plot'.
#I decided to focus on a geom_bar instead but it's still a great guide!
#https://ggplot2tutor.com/radar-chart/radar-chart-whatsapp/
#Thanks to this blog where i learned about using stat_density_2d on a ggmap:
#https://cfss.uchicago.edu/notes/raster-maps-with-ggmap/
