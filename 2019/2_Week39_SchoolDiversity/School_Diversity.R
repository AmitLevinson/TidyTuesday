library(tidyverse)
library(stringr)

df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")

#count(df, d_Locale_Txt)

#created a list of residency types to change
citycheck <- c("city-large|city-midsize|city-small")
ruralcheck <- c("rural-distant|rural-fringe|rural-remote")
suburbancheck <- c("suburban-large|suburban-midsize|suburban-small")
towncheck <- c("town-distant|town-fringe|town-remote")

#aggregated all the residency types to one. Used the Base R since i was having
#trouble piping it
df$d_Locale_Txt <-  str_replace_all(df$d_Locale_Txt, citycheck, "City")
df$d_Locale_Txt <-  str_replace_all(df$d_Locale_Txt, ruralcheck, "Rural")
df$d_Locale_Txt <- str_replace_all(df$d_Locale_Txt,suburbancheck, "Suburban")
df$d_Locale_Txt <- str_replace_all(df$d_Locale_Txt,towncheck, "Town")

#creating data set for schools and calculating percents:
byyear <- df %>% group_by(LEAID) %>% 
      mutate(n = length(LEAID), diverse = as.factor(diverse)) %>% 
  rename(Geogliving = d_Locale_Txt, year = SCHOOL_YEAR) %>% 
         filter(n == 2) %>% 
  group_by(year, diverse, Geogliving) %>% 
  summarise(total = length(diverse)) %>%
  na.omit %>% 
  ungroup() %>% 
  group_by(year, Geogliving) %>%
  mutate(percent = total/sum(total)*100, year[year == "1994-1995"] = 1994,
         year[year == "2016-2017"] = 2016)
  
byyear$year[byyear$year == "1994-1995"] <- 1994
byyear$year[byyear$year == "2016-2017"] <- 2016


#plotting a stacked bar plot
p <- ggplot(byyear, aes(factor(year), y = percent, fill = diverse)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  facet_grid(. ~ Geogliving)+ 
  scale_fill_manual(values = c("Diverse" = "green4", "Extremely undiverse" = "tomato3", "Undiverse" = "tan2"))


#adjusting theme
p + theme(
  strip.text = element_text(
    size = 17, color = "black", face = "bold.italic"),
  strip.background = element_blank(),
  plot.background = element_rect(fill = "gray92"),
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.title = element_text(size = 19, face = "italic"),
  plot.subtitle = element_text(size = 11, face = "italic"),
  plot.caption = element_text(size = 10),
  axis.title=element_blank(),
  axis.ticks.x=element_blank(),
  axis.text.x = element_text(size = 16),
  axis.text.y = element_text(size = 11),
  legend.position = "bottom",
  legend.title = element_blank(),
  legend.text = element_text(size = 14),
  legend.background = element_blank()
) +
  labs(title = "School ethnic diversity levels across residency types",
       subtitle = "Percentage of schools",
       caption = "Data: The Washington Post | AmitL")

ggsave("Ethnic_div.png", width =10, height = 6)
  