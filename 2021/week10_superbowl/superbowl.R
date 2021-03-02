library(tidyr)
library(dplyr)
library(ggplot2)
library(here)
library(ggtext)
library(patchwork)
library(extrafont)

youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')
# dir.create("extra/images/progress/2021-week10") # Directory for images showing the process

# Exploratory
skimr::skim(youtube)


# First plot of all brands except our outlier -----------------------------

  p1 <- youtube %>% 
  # Filter out the highest value (doritos)
  arrange(view_count) %>% 
  slice(1:(nrow(.)-1)) %>% 
  ggplot(aes(x = brand, y = view_count))+
  geom_jitter(width = 0.25, size = 0.8) +
  # Create breaks including a dot dot dot
  scale_y_continuous(limits = c(0,4e7), breaks = seq(0, 4e7, 1e7), labels = c(seq(0,30,10), ".\n."))+
  theme_minimal()+
  labs(title = "Brands' Super Bowl Commercials Youtube View Count",
       subtitle = "Each point represents an aired commercial Youtube view count. Majority of advertising content is characterized\nby 0-10 million views, with a few commercials gaining more than 20 million views. Data is based on \"233 ads\nfrom the 10 brands that aired the most spots in all 21 Super Bowls (FiveThirtyEight)\". Several commercials were\nmissing view counts.",
       x = "\nBrand name",
       y = "\n\nViews  (millions)",
       caption = "Data: FiveThirtyEight\nVisualization: @Amit_Levinson")+
  theme(text = element_text(family = "IBM Plex Sans"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 28, hjust = 0, vjust = 3, face = "bold", family = "Bodoni MT"),
        plot.caption = element_text(size = 10, color = "gray35"),
        plot.subtitle = element_text(size = 14, color = "gray25", vjust = 6),
        axis.title.y = element_text(size = 11, color = "gray35"),
        axis.title.x = element_text(size = 14, color = "gray35"),
        axis.text.x = element_text(size = 14, color = "gray30"),
        axis.text.y = element_text(size = 11, color = "gray30"))

# # Section two of only Doritors' commercial ------------------------------

  # Label for the doritors commercial:
doritos_label <- data.frame(
    x = 4.48,
    y = 163500000,
    label = "<b>Doritos'</b> 2012 *Sling Baby* has more<br>than **170 million** Youtube views")
  
  
  p2 <- youtube %>% 
    # Keep all groups so that the point aligns on x-axis
    group_by(brand) %>% 
    top_n(n = 1, wt = view_count) %>% 
    ungroup() %>% 
    # Remove values for all groups
    mutate(view_count = ifelse(brand == "Doritos", view_count, NA),
           # Convert the brand to numeric for easier plotting of curve arrow
           id = as.numeric(factor(brand))) %>% 
    ggplot(aes(x = id, y = view_count))+
    geom_point(size = 0.8)+
    # Start breaks where previous plot ends, though I don't think it matters
    scale_y_continuous(limits = c(3e7,1.8e8))+
    # Remove everything
    theme_void()+
    annotate(geom = "curve", x = 4.5, xend = 4.03, y = 170000000, yend = 176350200, curvature = .2, color = "grey35", size = 0.75, arrow = arrow(length = unit(1.5,"mm")))+
    geom_richtext(data = doritos_label, aes(x = x, y = y,label =label), fill = NA, label.color = NA, hjust = 0,family = "IBM Plex Sans Light", size = 4)
  

# Combining plots ---------------------------------------------------------

# Combine, give extra space for the top plot to highlight the distance
p2/p1+
  plot_layout(height = c(2,1))+
  ggsave(here("extra","images", "progress", "2021-week10", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo', height = 8, width = 13)  

ggsave(here("2021","week10_superbowl", "superbowl.png"), type = 'cairo', height = 8, width = 13)  
