library(tidyverse)
library(rtweet)
library(lubridate)
library(hrbrthemes)

tmls <- get_timelines(c("netanyahu", "gantzbe"), n = 5000)

df <- tmls %>% 
  mutate(t_year = year(created_at),
         t_week = week(created_at)) %>% 
  group_by(screen_name, t_year, t_week) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  filter(t_year >= 2019 & t_year < 2020) %>% 
  mutate(t_week_date = as.Date(paste(2019, t_week, 1, sep = "-"),"%Y-%U-%u")) #thanks to a stack overflow for this response


ggplot(data = df) + 
  geom_path(aes(x = t_week_date, y = n, color = screen_name), size = 1)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  labs(title = "Israeli political candidates weekly tweets throughout 2019", x = NULL, y = NULL)+
  theme_ipsum_rc()+
  geom_text(data = df %>% filter(t_week_date %in% as.Date(c("2019-04-08", "2019-09-16")) & screen_name == "netanyahu"),
            aes(x = t_week_date, y = 100, label = c("First elections\nApr 9, 2019", "Second Elections\n Sep 17, 2019")), hjust = -0.1)+
  geom_vline(xintercept = as.Date(c("2019-04-08", "2019-09-16")), linetype = "dashed", size = 1, alpha = 7/10)+
  scale_color_discrete(name = "Candidate", breaks = c("netanyahu", "gantzbe"), labels = c("Benjamin Netanyahu (previous PM)","Benny Gantz"))+
  theme(
    legend.background = element_blank(),
    legend.key.size = unit(1.5, "line"),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "top",
    panel.grid = element_blank()
  )

ggsave("elections.png", width = 10, height = 6)
save(tmls, df, file = "mydata.RData")
load("mydata.RData")
