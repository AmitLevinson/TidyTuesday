library(tidyverse)
library(extrafont)
library(ggtext)

animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')


# Exploratory -------------------------------------------------------------

animal_outcomes %>% 
  count(year, wt = Total)

animal_outcomes %>% 
  ggplot(aes( x= year, y = Total, color = animal_type))+
  geom_line()+
  facet_wrap(~ outcome)

# By year - What happened with Euthanize?
animal_outcomes %>% 
  filter(outcome == "Euthanized") %>% 
  pivot_longer(cols = ACT:WA, names_to = "state") %>%
  ggplot(aes( x= year, y = value, color = animal_type))+
  geom_line()+
  facet_wrap (~ state)

# Looking at Euthanization specifically -----------------------------------

# Data Prep

animal_long <- animal_outcomes %>% 
  # Filter specific animals (the rest don't have change)
  filter(outcome %in% c("Euthanized"), animal_type %in% c("Cats", "Dogs", "Wildlife")) %>% 
  # Pivot longer to have them all in one plot
  pivot_longer(cols = ACT:WA, names_to = "state") %>%
  # Create a column with a unique name for each animal and state
  mutate(aggregated_name = paste0(animal_type, " - ", state),
  # Add criteria to highlight
         highlight_point = case_when(
         aggregated_name == "Wildlife - QLD" ~ "darker",
         aggregated_name == "Cats - NT" ~ "lighter",
         TRUE ~ "none"
         ),
  # Create a label for the highlighted values
         value_label = case_when(
           aggregated_name == "Wildlife - QLD" ~ "Wildlife in Queensland (15,690)",
           aggregated_name == "Cats - NT" ~ "Cats in Northern Territory (5,735)",
           TRUE ~ "none"
         )) %>% 
  filter(year %in% c(min(year),max(year))) %>% 
  # Convert to factor to remove years in between
  mutate(year = as.factor(year))

# Plot

ggplot(data = animal_long, aes(x = year, y = value, color = highlight_point, group = aggregated_name, alpha = highlight_point))+
  geom_point(size = ifelse(animal_long$highlight_point ==  "none", 2, 3))+
  geom_line(size = 1.75)+
  # Add additional line so that it's on top of the previous lines
  geom_line(data = filter(animal_long, highlight_point != "none"), size = 1.75)+
  # Add text only to the end point
  geom_text(data = filter(animal_long, highlight_point != "none" & year == 2018), aes(label = value_label), hjust = -0.05, vjust = 0.3, size = 3, fontface = "bold")+
  scale_color_manual(values = c("none" = "grey85", "darker" = "#ED1C24", "lighter" = "#FF9966"))+
  scale_alpha_manual(values = c("none" = 0.5, "darker" = 1, "lighter" = 1))+
  # Remove various legends
  guides(color = "none", alpha = "none")+
  labs(title = "Animal Euthanization across Australia", subtitle = "Animal Euthanization of Cats, Dogs and Wildlife across Australia for the years 1999 & 2018.<br>Euthanizing of <span style='color:#ED1C24'><b>Wildlife in Queensland</b></span> has been increasing since the 2000's, while that of<br><span style='color:#FF9966'><b>Cats in Northern Territory</b></span> has remained steady but rapidly increased in the past few years.<br>", caption = "Data: RSPCA | @Amit_Levinson")+
  theme_minimal()+
  theme(
    text = element_text(family = "IBM Plex Sans"),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12, color = "grey45"),
    plot.subtitle = element_markdown(color = "grey35", size = 12),
    plot.margin = margin(4,2,2,4,"mm"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.caption = element_text(face = "italic", color = "grey45", size = 6)
  )

ggsave("animal-aus.png", width = 10, height = 6, dpi = 400)



# Alaternative Line plot instead of a slope-graph -------------------------------------------------

animal_long_line <- animal_outcomes %>% 
  # Filter specific animals (the rest don't have change)
  filter(outcome %in% c("Euthanized"), animal_type %in% c("Cats", "Dogs", "Wildlife")) %>% 
  # Pivot longer to have them all in one plot
  pivot_longer(cols = ACT:WA, names_to = "state") %>%
  # Create a column with a unique name for each animal and state
  mutate(aggregated_name = paste0(animal_type, " - ", state),
         # Add criteria to highlight
         highlight_point = case_when(
           aggregated_name == "Wildlife - QLD" ~ "darker",
           aggregated_name == "Cats - NT" ~ "lighter",
           TRUE ~ "none"
         ),
         # Create a label for the highlighted values
         value_label = case_when(
           aggregated_name == "Wildlife - QLD" ~ "Wildlife\nin QLD",
           aggregated_name == "Cats - NT" ~ "Cats\nin NT",
           TRUE ~ "none"
         ))

# Plot

ggplot(data = animal_long_line, aes(x = year, y = value, color = highlight_point, group = aggregated_name, alpha = highlight_point))+
  geom_line(size = 1, color = "grey75")+
  # Add additional line so that it's on top of the previous lines
  geom_line(data = filter(animal_long_line, highlight_point != "none"), size = 1.25)+
  # Add text only to the end point
  geom_text(data = filter(animal_long_line, highlight_point != "none" & year == 2018), aes(label = value_label), hjust = -0.09, vjust = 0.3, size = 3, fontface = "bold")+
  coord_cartesian(clip = 'off')+
  scale_color_manual(values = c("none" = "grey85", "darker" = "#ED1C24", "lighter" = "#FF9966"))+
  scale_alpha_manual(values = c("none" = 0.5, "darker" = 1, "lighter" = 1))+
  scale_y_continuous(labels = scales::comma_format())+
  # Remove various legends
  guides(color = "none", alpha = "none")+
  labs(title = "Animal Euthanization across Australia", subtitle = "Animal Euthanization of Cats, Dogs and Wildlife in Australia from 1999-2018.<br>Euthanizing of <span style='color:#ED1C24'><b>Wildlife in Queensland</b></span> has been increasing since the 2000's, while that of<br><span style='color:#FF9966'><b>Cats in Northern Territory</b></span> has remained steady but rapidly increased in the past few years.<br>", caption = "Data: RSPCA | @Amit_Levinson", y = "Number of animals Euthanized", x = NULL)+
  theme_minimal()+
  theme(
    text = element_text(family = "IBM Plex Sans"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_text(size = 8, color = "grey65"),
    axis.text.y = element_text(size = 8, color = "grey45"),
    axis.text.x = element_text(size = 8, color = "grey45"),
    plot.subtitle = element_markdown(color = "grey35", size = 12),
    plot.margin = margin(4,2,2,4,"mm"),
    plot.title = element_text(size = 16, face = "bold"),
    plot.caption = element_text(face = "italic", color = "grey45", size = 6)
  )


ggsave("animal-aus-line.png", width = 10, height = 6, dpi = 400)

