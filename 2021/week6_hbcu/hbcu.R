library(here)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggtext)
library(extrafont)
library(waffle)


hbcu <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv')

# a little data manipulation:
hbcu_tidy <- hbcu %>% 
    pivot_longer(cols = Males:Females, names_to = "gender", values_to = "enrolled") %>% 
    mutate(enrolled = round(enrolled / 1000, 0)) %>% 
    # unfortunately no data for 1985 :(
    filter(Year %% 5 == 0)

hbcu_tidy %>% 
  ggplot()+
  geom_pictogram(aes(values = enrolled, label = "gender", color = gender), n_rows = 10, flip = TRUE, size = 4, family = "Font Awesome 5 Free Solid")+
  facet_wrap(~ Year, nrow = 1, strip.position = "bottom")+
  scale_label_pictogram(name = NULL, values = c("graduation-cap"))+
  scale_y_continuous(breaks = c(10,20,30), labels = paste0(seq(100,300,100), "k"))+
  scale_color_manual(values = c("#72BC79", "gray70"))+
  coord_equal()+
  labs(title = "Student enrollment in Historically Black Colleges and Universities (HBCU)",
       subtitle = "HBCU institution enrollment since the 1980s. Much of this increase results from greater <span style='color:#72BC79'><b>female</b></span> participation than <span style='color:gray55'>**men**</span>.<br><br>
      <span style='font-family: \"Font Awesome 5 Free Solid\";color:gray35;'>&#xf19d;</span> = 1,000 students",
       caption = "\nData: Data.World | @Amit_Levinson")+
  theme_minimal(base_family = "Mukta Medium")+
  theme(legend.position = "none",
        plot.subtitle = element_markdown(size = 18, family = "Mukta Light"),
        plot.title = element_markdown(size = 22),
        plot.caption = element_text(size = 10, color = "gray45", face = "italic"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 13, color = "gray50"),
        strip.text = element_text(size = 14, color = "gray35"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(color = "gray85", linetype = "dashed"),
        plot.margin = margin (4,4,4,4, "mm"))
  ggsave(here("extra","images", "progress", "2021-week6", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo', height = 4, width = 6)  

  
ggsave(here("2021", "week6_hbcu", "hbcu.png"), width = 16, height = 12)  
