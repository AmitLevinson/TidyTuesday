library(tidyverse)
library(lubridate)
library(gganimate)
library(scales)
library(RColorBrewer)
library(ggstance)
library(ggtext)
library(extrafont)


grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')

# Create adjusted value for March 2020
adjusted_march_2020 <- cpi %>% 
  mutate(march_cpi = cpi[year_month == "2020-03-01"]/cpi) %>% 
  select(-cpi)

# Create a cumulative gross by year
cumulative_gross <- grosses %>% 
  mutate(year_month = floor_date(week_ending, unit = "years")) %>% 
  left_join(adjusted_march_2020) %>%
  mutate(weekly_gross = weekly_gross * march_cpi) %>% 
  group_by(show, year_month) %>%
  summarise(year_gross = sum(weekly_gross)) %>% 
  group_by(show) %>%
  mutate(cumulative_sum = cumsum(year_gross))
  
# Create vector of top 10 shows
top_10  <- 
  cumulative_gross %>% 
  arrange(-cumulative_sum) %>%
  group_by(show) %>% 
  filter(cumulative_sum == max(cumulative_sum)) %>%
  ungroup() %>% 
  top_n(10) %>% 
  pull(show)

# New df with only top 10 + create a cumsum of years_aired for {gganimate}
cumulative_gross_top_10 <- 
  cumulative_gross %>% 
  filter(show %in% top_10) %>%
  group_by(show) %>% 
  mutate(year_value = 1,
         years_aired = cumsum(year_value)) %>% 
  ungroup()

# Change stringAsFactors so that we can read color palette
options(stringsAsFactors = FALSE)


cumulative_df_race <- 
  cumulative_gross_top_10 %>%
  # complete all shows to have all 33 years
  complete(show, years_aired) %>% 
  # Helps keep shows on the plot even if they only ran a few years
  fill(cumulative_sum) %>% 
  # join with a dataframe to add colors for each variable
  left_join(., data.frame(show = unique(cumulative_gross_top_10$show),
                          bar_fill = brewer.pal(10, "Paired")))%>% 
  group_by(years_aired) %>% 
  # Create the changing order of the shows: 
  mutate(rank = rank(-cumulative_sum),
         value_lbl = comma(round(cumulative_sum,digits = 0)),
         # Once the shows stops running, turn it to gray:
         bar_fill = ifelse(is.na(year_month), "gray45", bar_fill),
         # Add a line break for long show names
         show = ifelse(show == "The Book of Mormon", "\nThe Book of \nMormon ",
                        ifelse(show == "The Phantom of the Opera", "\nThe Phantom of \nthe Opera  ",
                               show))) %>%
  select(-c(year_gross, year_value)) %>% 
  ungroup()


static_plot <- 
  ggplot(cumulative_df_race, aes(y = rank, x = cumulative_sum, group = show, fill = bar_fill, color = bar_fill))+
  geom_barh(stat = "identity", aes(fill = bar_fill), alpha = 0.8)+ # From the {ggstance} package, works better with changing axis in gganimate
  geom_text(aes(x = 0, label = paste(show, " ")), vjust = 0.2, hjust = 1, size = 7)+
  scale_x_continuous(breaks = c(0,500000000,1000000000,1500000000,2000000000),labels = c("0","$500 M", "$1 Billion","$1.5 B", "$2 B"))+
  # For having the bars as the color we chose:
  scale_color_identity()+
  scale_fill_identity()+
  scale_y_reverse()+
  coord_cartesian(clip = "off")+
  labs(x = "", y = "")+
  guides(color = FALSE, fill = FALSE)+
  theme(text = element_text(family = "Roboto Condensed"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 18),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(size = .1, color = "grey"),
    plot.background = element_blank(),
    panel.background = element_blank(),
    plot.subtitle = element_text(size = 22, color = "gray45"),
    plot.caption = element_text(size = 14, hjust = 1, face = "italic"),
    plot.margin = margin (2,2,2,4, "cm"),
    plot.title = element_markdown(size = 28)
  )

anim <-  static_plot+
    transition_states(years_aired, transition_length = 3, state_length = 0)+
    view_follow(fixed_y = TRUE)+
    labs(title = "**Top 10 Broadway Grosses**<br><span style='font-size:22pt'>**Number of years screened: {closest_state}**</span>",
         subtitle = "Gray shows were stopped screening at that time point",
         caption = "Data: Playbill | @Amit_Levinson")

animate(anim, 180, width = 1200, height = 1000, renderer = gifski_renderer("broadway.gif"))

# Some fantastic tutorials:
# A good starting point: https://towardsdatascience.com/create-animated-bar-charts-using-r-31d09e5841da
# Slides on gganimate: https://goodekat.github.io/presentations/2019-isugg-gganimate-spooky/slides.html#1
# To create a moving axis scale use geom_barh: https://stackoverflow.com/questions/54646652/animated-barplot-via-gganimate-conflict-of-view-follow-coord-flip/55791059#55791059