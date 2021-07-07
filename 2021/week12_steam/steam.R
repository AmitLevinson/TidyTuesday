library(readr)
library(ggplot2)
library(dplyr)
games <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

games_processed <- games %>% 
  mutate(
    avg_peak_perc = parse_number(avg_peak_perc),
    year_month = as.Date(paste0(year,"-", match(month, month.name), "-1"))
  ) %>% 
  add_count(gamename) %>% 
  group_by(gamename) %>% 
  filter(n >= 12 & min(avg) > 1e3, avg_peak_perc > 0) %>% 
  arrange(gamename, year_month) %>% 
  group_by(gamename) %>% 
  mutate(month_since_start = 1:n(),
         avg_total = mean(avg),
         high_peak = max(peak)/avg_total) %>% 
  ungroup()

# dir.create("extra/images/progress/2021-week12") # Directory for images showing the process

games_quart <- games_processed %>%
  filter(!gamename %in% c("PLAYERUNKNOWN'S BATTLEGROUNDS", "Dota 2" )) %>% 
  #mutate(jump = peak - min(avg)) %>%
  distinct(gamename, avg_total) %>% 
  mutate(quart = cut(avg_total, quantile(unique(.$avg_total), probs = 0:4/4), labels = seq(1,4,1),  include.lowest = TRUE)) %>% 
  distinct(gamename, quart) %>% 
  select(gamename, quart) %>% 
  inner_join(games_processed)


highlighted_peaks <- games_quart %>% 
  distinct(gamename, peak, .keep_all = TRUE) %>% 
  group_by(quart) %>% 
  arrange(-avg_total) %>% 
  distinct(gamename) %>% 
  slice(1:4) %>% 
  pull(gamename)

games_quart %>% 
  count(quart, sort = T)

games_plot <- games_quart %>% 
  mutate(highlight = ifelse(gamename %in% highlighted_peaks, "yes", "no")) %>% 
  arrange(desc(highlight), gamename) %>% 
  mutate(id = group_indices(., gamename))

one_line <- games_plot %>% 
  filter(quart == 4) %>% 
  filter(gamename %in% highlighted_peaks)
  


several_lines <- games_plot %>%
  filter(quart == 4) %>% 
  filter(!gamename %in% highlighted_peaks) %>% 
  select(Gamename = gamename, everything())

  ggplot(one_line, aes(x = month_since_start, y = avg)) +
  geom_line(data = several_lines, aes(group = Gamename),color = "gray80")+
  geom_line(aes(color = gamename))+
  facet_wrap(~gamename)+
  # facet_wrap(. ~ gamename, scales = "free") %>% 
  ggsave(paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"), path = "extra/images/progress/2021-week12", type = 'cairo', height = 8, width = 13)  


  games_quart %>% 
    filter(quart == 4) %>% 
    ggplot() +
    geom_line(aes(x = month_since_start, y = avg, group = gamename, color = ifelse(gamename %in% highlighted_peaks, "yes", "no")))+
    #facet_wrap(. ~ gamename, scales = "free")+
    scale_color_manual(values = c("yes" = "red", "no" = "gray55"))

  #facet_wrap(. ~ distinct(), scales = "free")+
  

.

# New? --------------------------------------------------------------------


games_processed <- games %>% 
  mutate(
    avg_peak_perc = parse_number(avg_peak_perc),
    year_month = as.Date(paste0(year,"-", match(month, month.name), "-1"))
  ) %>% 
  add_count(gamename) %>% 
  filter(n >= 12 & avg > 1e3, avg_peak_perc > 0) %>% 
  arrange(gamename, year_month) %>% 
  group_by(gamename) %>% 
  mutate(month_since_start = 1:n()) %>% 
  
  mod <- ~ glm(cbind(avg, max(peak) - avg) ~ month_since_start, ., family = "binomial")
library(tidyr)
library(purrr)
library(broom)

slopes <- games_processed %>%
  nest(-gamename) %>%
  mutate(model = map(data, mod)) %>% 
  mutate(model = map(model, tidy)) %>% 
  unnest(model) %>%
  filter(term == "month_since_start") %>%
  arrange(-estimate)

slope_names <- slopes %>% 
  pull(gamename) %>% 
  .[1:10]

games_processed %>% 
  filter(gamename %in% slope_names) %>% 
  #mutate(highlight = ifelse(gamename %in% slope_names, "yes", "no")) %>% 
  ggplot() +
  geom_line(aes(x = month_since_start, y = avg, group = gamename))+
  scale_color_manual(values = c("yes" = "red", "no"= "gray55"))

games_processed %>% 
  group_by(gamename) %>% 
  filter(month_since_start == max(month_since_start)) %>% 
  ungroup() %>% 
  ggplot()+
  geom_point(aes(x = month_since_start, y = avg_total))
  ggsave(paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"), path = "extra/images/progress/2021-week12", type = 'cairo', height = 8, width = 13)  
  
  
library(ggforce)

  tbl <- data.frame(
    x0 = seq(1:1000),
    y0 = seq(1:1000),
    r = seq(1:1000),
    id = sample(1:10, size = 1000, prob = 1:10/10, replace=  TRUE)
  )
  
  ggplot(tbl)+
    geom_circle(aes(x0 = x0, y0 = y0, r = r))+
    facet_wrap(~ id)
  