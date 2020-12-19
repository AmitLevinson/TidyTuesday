# Read data
ninja_warrior <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-15/ninja_warrior.csv')
library(dplyr)
library(ggraph)
library(igraph)
library(ggtext)
library(extrafont)

# Create an object to pass to a network graph
obstacle_order <- ninja_warrior %>% 
  group_by(season, location, round_stage) %>% 
  # Create a 'from' and 'to' columns for network graph
  mutate(to = lead(obstacle_name),
         # Aggregate to have some sort of count
         arc_name = paste0(obstacle_name, "-", to)) %>% 
  ungroup() %>% 
  select(from = obstacle_name, to, arc_name) %>% 
  filter(!is.na(to)) %>% 
  add_count(arc_name, sort = T) %>% 
  select(-arc_name)

# Count how many are under 2 occurrences
obstacle_order %>% 
  count(n, sort = T) %>% 
  group_by(under_2 = n <=2) %>% 
  summarise(total = sum(nn)) %>% 
  mutate(prop = total/sum(total)) # ~68% of two-sequence obstacles occur only once or twice

# Plot
graph_from_data_frame(obstacle_order) %>% 
  ggraph(layout= 'linear', circular = TRUE)+
  geom_edge_arc(aes(color = ifelse(n<=2, "Under 2 combinations", "more than two")), show.legend = FALSE, edge_width = 0.3)+
  #geom_node_point(size = 1, color = "gray55", fill = NA)+
  scale_edge_color_manual(values = c("Under 2 combinations"= "#26677f", "more than two" ="#89374f")) +
  labs(title = "Randomness in Ninja-Warrior obstacle courses", 
       subtitle = "Each arc represents the transition from one obstacle to another in a ninja-warrior obstacle course. <b><span style='color:#26677f'>Majority of<br>obstacle transitions (i.e. an arc) occur only once or twice</b></span> throughout stages, seasons and locations. For example,<br>transitioning from 'Jumping Bars' to a 'Cargo Climb' occurs only in the Semi-Finals of season 1 in Venice.")+
  theme(
    text = element_text(family = "Roboto Condensed"),
    plot.title = element_text(size = 22, face = "bold"),
    plot.subtitle = element_markdown(size = 13),
    plot.background = element_rect(fill = "gray90", color = "gray55"),
    panel.background = element_rect(fill = "gray90"),
    plot.margin = margin(4,4,2,4, "mm")
  )

# Save
  ggsave("2020/week51_ninja/week51_ninja.png", width = 10, height = 8)