library(tidyverse)
library(maps)
library(gganimate)
library(ggthemes)
library(extrafont)
library(viridis)

diseases <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-10/diseases.csv")

#Ok, let's go with Measles and see how many we have each year:
#diseases %>% 
#  filter(disease == "Measles") %>% 
#  group_by(year) %>% 
#  summarise(total_year = sum(count)) %>% 
#  View()

#The vaccine was introduced in the 1960s - That's why the sudden drop.   
#I'll plot untill 1980, however there are are more cases afterwards -  
#for e.g a small peak between 1989-1991

measles <- diseases %>% 
  #filter only Measles and until the year 1980
  filter(disease %in% "Measles" , year <=1980) %>% 
  #changing states to lower cases for joining with the map df
  mutate(region = tolower(state),
  #turning number of observed cases to thousands       
         count = count/1000)

#Load the US state map
states <- map_data("state")

#Join together and discard irrelevant columns
states_disease <- left_join(states, measles) %>% 
  select(-c(6,10,12))

#Making sure all states were joined properly:
# states_disease %>% 
#  group_by(year) %>% 
#  summarise(total = sum (count)) %>% 
#  View()

#Plot

#plotting the outline for the map
plot <- ggplot(states_disease, aes(x= long, y = lat, group = group, fill = count))+
    geom_polygon(color = "gray90", size = 0.1)+
  #these specific values give the map a nice projection
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  #scale_fill_gradientn(colors = jet.colors(16))+
  scale_fill_viridis(name = "# Cases observed\n(thousands)")+
  theme_map()+
  labs(title = "U.S. Incidence of Measles 1928-1980",
       subtitle = "{closest_state}", 
       caption = "Data: Tycho Project | @Amit_Levinson")+
  theme(text = element_text(family = "Microsoft Tai Le"),
        plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 18, face = "bold.italic"),
        plot.caption = element_text(size = 10, face = "italic"),
        legend.title = element_text(size = 14, hjust = 0, vjust = 0.8),
        legend.title.align = 0.5,
        legend.position = "bottom",
        legend.text = element_text(size = 8))+
  #actual animation of plot:
    transition_states(year, transition_length = 1, state_length = 2)

#saving the animation and giving the last frame some extra time:
anim_save("measles.gif", animation = plot, duration = 10,
          nframes = 2 * length(unique(states_disease$year)) + 8,
          end_pause = 8)
