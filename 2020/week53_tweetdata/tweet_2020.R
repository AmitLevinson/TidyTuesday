library(tidyverse)
library(lubridate)
library(rvest)
library(glue)
library(ggtext)
library(extrafont)

# Read the #tidytuesday tweet collection
tt <- read_csv("https://github.com/rfordatascience/tidytuesday/blob/master/tidytuesday_tweets/data.csv?raw=true")

# Clean data for this year and get the week number
data_2020 <- tt %>% 
  filter(created_at > "2020-01-01 00:00:00" & !is.na(media_url)) %>% 
  mutate(week_count = week(created_at))

week_count <- data_2020 %>% 
  count(week_count, sort = T)

# Get weeks I participated in
amit_participate_in <- data_2020 %>% 
  filter(str_detect(screen_name, "Amit_Levinson")) %>%
  count(week_count) %>% 
  pull(week_count)

# Get info for the week
week_name <- read_html("https://github.com/rfordatascience/tidytuesday") %>% 
  html_node("table") %>% 
  html_table()

# Join week count and name to get more info
week_data_full <- left_join(week_count, week_name, by = c("week_count" = "Week")) %>% 
  # Create the axis categories and use ggtext for lighter data info
  mutate(name = glue("{Data}<br><span style='color:gray65'>({month(Date, label = TRUE)}, {day(Date)})</span>"),
         al_participated = ifelse(week_count %in% amit_participate_in, "yes", "no"),
         # Fix a few I probably posted in the following week :(
         al_participated = case_when(
           # Didn't participate in the IKEA week
           week_count == 45 ~ "no",
           # Did participate in the Measles week
           week_count == 9 ~"yes",
           TRUE ~ al_participated)) %>% 
  slice(1:15)

ggplot(week_data_full)+
  geom_col(aes(y = fct_reorder(name, n), x = n, fill = al_participated), show.legend = FALSE)+
  labs(title = "#Tidytuesday weeks with the most contributions",
       subtitle = "Only tweets containing photos were aggregated. <b><span style='color:#453F78'>Highlighted are the weeks I participated in.</span></b><br>Caveat: individuals might\npost their analysis in following week from the original data.",
       x = "#TidyTuesday tweets with photos", y = "Data (Date)\n", caption = "Data: Tidytuesday & Thomas Mock\n visualizaiton: @Amit_Levinson")+
  scale_fill_manual(values = c("yes" = "#453F78", "no" = "gray55"))+
  theme_minimal()+
  theme(
    text = element_text(family = "Roboto Condensed"),
    plot.title = element_text(size = 16),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 12, color = "gray25"),
    plot.caption = element_text(size = 8, color = "gray35"),
    axis.text.y = element_markdown(hjust = 0, size = 10),
    axis.text.x = element_text(size = 8),
    axis.title = element_text(size = 10, color = "gray35"),
    plot.margin = margin(4,2,2,4, "mm"))


ggsave("2020/week53_tweetdata/data_2020.png", width = 9, height = 7)
