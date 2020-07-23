library(tidyverse)

beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')


# Exploring ---------------------------------------------------------------

glimpse(beer_states)

beer_states %>% 
  filter(state != "total") %>% 
ggplot(aes(x = year, y = barrels, color = state))+
  geom_line(show.legend = FALSE)+
  facet_wrap(~ type, scales = "free_y")+
  scale_y_continuous(breaks = breaks_extended(8))

# Let's look at how much does the top value in kegs and barrels compare to other states in 2019 --------
kegs_2019 <- beer_states %>% 
  filter(state != "total",year == "2019", type == "Kegs and Barrels")

# Leading kegs and barrels maker:
ca <- kegs_2019 %>% 
  arrange(-barrels) %>% 
  slice(1)

# All other states summing up +-:
state_list <- kegs_2019 %>% 
  arrange(barrels) %>% 
  slice(1:36)

new_kegs <- rbind(ca, state_list) %>% 
  # add criteria for only CA:
  mutate(ca = ifelse(state == "CA", "CA", "Not CA"),
         barrels = barrels/10000)
  
new_kegs %>% 
  ggplot(aes(label = ca, values = barrels))+
  geom_pictogram(n_rows = 10,aes(color = ca), size = 0.33, flip = TRUE)+
  scale_color_manual(name = NULL,
                     values = "#c68958", "#a40000",
                     labels = c("CA", "Other"))+  
  scale_label_pictogram(name = NULL,
                        values = c("beer", "beer"),
                        labels = c("CA", "Other"))+
  coord_equal()+
  theme_minimal()+
  theme_enhance_waffle()


install_fa_fonts()
?scale_label_pictogram
  library(waffle)
devtools::install_github("hrbrmstr/waffle")
?geom_pictogram
?facet_wrap
extrafont::font_import()
  
  
beer_states %>% 
  
View(beer_states)

library(scales)
?scales
  count(state)
beer_states

brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
brewer_size %>% 
  count(year)

brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')

brewing_materials %>% 
  count(year)
  View(brewing_materials)
  pivot_longer(cols = month_current:month_prior_year,  = material_type) %>% 
  ggplot(aes(x = year, y = value))+
  geom_line()+
  facet_wrap(~ name)
