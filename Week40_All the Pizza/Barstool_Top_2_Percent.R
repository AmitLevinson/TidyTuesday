library(tidyverse)
library(janitor)
library(ggrepel)

pbarstool <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_barstool.csv")

#cleaning and renaming data
pbarstool <- pbarstool %>% 
  clean_names() %>% 
  rename(
    total_average = review_stats_all_average_score, 
    community_count = review_stats_community_count,
  )
#filtering to pizzaerias > 0 & and have more than 10 votes from community
p_cooked <- pbarstool %>% select(name, community_count, total_average) %>% 
  filter(total_average != 0 & community_count >= 10) %>% 
  mutate(percents = ntile (total_average , 50)) 


g <- ggplot(p_cooked, aes(x = total_average))+
  geom_area(stat = "bin", color = "black", fill = "lightpink")+
  scale_x_continuous(name="Rating", breaks=seq(0,10,1)) +
  scale_y_continuous(name="Number of pizzerias",breaks = seq(0,30,10))+
  
 #adding Median line
  geom_segment(aes(x = median(total_average),y = 0, xend = median(total_average), yend = 21), color = "dodgerblue2", size = 1, linetype = "dashed")+
  annotate("text", x = median(p_cooked$total_average)-0.08, y = 1.5, label = "Median", color = "black", angle = 90)+
  
  #adding top 2% segment
  #geom_segment(aes(x = min(total_average[percents == 50]), y = 0, xend = min(total_average[percents == 50]), yend = 10), color = "dodgerblue2", linetype = "dashed", size = 1)+
  annotate("text", x = 8.6, y = 6, label = "Pizzerias ranked \n in top 2%", color = "black", fontface = 2)+
  geom_curve(aes(x = 8.75, y = 6, xend = 9, yend = 2),
             colour = "#555555", curvature = -.6, size = .8,
             arrow = arrow(length = unit(0.03, "npc"))) +
  
  #adding Pizzeria labels for top 2%
  geom_label_repel(data = subset(p_cooked, percents == 50),
                   aes(label = name, y = 2),
                   fontface = 'bold',
                   color = "mediumpurple",
                   xlim=c(8,9), 
                   ylim=c(0,15)
                    )+
  labs(x = "", y = " ", title = "Pizzerias rating distribution",
       subtitle = "", caption = "Only pizzerias with > 10 votes are shown \n Data: 'Barstool Sports' | AmitL")

#plot background
g+theme_minimal()+
  theme(plot.background = element_rect(fill = "gray92"),
        plot.title = element_text(size = 19, face = "italic", vjust = -4, hjust = 0.1),
        plot.caption = element_text(face = "italic"),
        axis.text.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(color = "black", size = 16,face = "plain"),
        axis.title.y = element_text(color = "black", size = 16,face = "plain")
        )

ggsave("Barstool_rating.png", width =10, height = 6)
