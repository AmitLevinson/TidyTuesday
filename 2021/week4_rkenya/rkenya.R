library(sf)
library(ggplot2)
library(here)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(extrafont)
# devtools::install_github("Shelmith-Kariuki/rKenyaCensus")
library(rKenyaCensus)
library(ggtext)

gender <- rKenyaCensus::V1_T2.2

# Get map, convert it to sf and clean the County column
kenya_plot_crs <- st_as_sf(KenyaCounties_SHP) %>% 
  st_transform(4326) %>% 
  mutate(County = as.character(County),
         County = str_to_title(County))

# Look at Men to women ratio
kenya_plot <- gender %>% 
  mutate(dif = (Male/Total)*100) %>% 
  arrange(dif) %>% 
  right_join(kenya_plot_crs) 

# Colors to use and label values
RColorBrewer::brewer.pal(11, "BrBG")

# Add Kenya in title with colors
kenya_html <- "<span style='color:black'>KE</span><span style='color:#F50519'>NY</span><span style='color:#008C00'>A</span>"

# Add labels to the plot
label_df <- data.frame(
  label = c("<br><span style='color:#8C510A'><b>More men</b></span>",
            "<span style='color:#35978F'><b>More women</b></span>"),
  x = c(41.05, 33.7),
  y = c(1.35, -2),
  hjust = c(0,0)
)

ggplot(kenya_plot)+
  geom_sf(aes(geometry = geometry, fill = dif))+
  scale_fill_distiller(type = "div",palette = "BrBG",  name = "Percentage Men", breaks = c(45,50,55),
                       limit = c(45,55), labels = c("45%", "Even", "55%"),
                       guide = guide_colourbar(title.position = "top"))+
  geom_richtext(data = label_df, aes(x = x, y = y, label = label, hjust = hjust),label.color = NA,  label.padding = grid::unit(rep(0, 4), "pt"), fill = NA, size = 7)+
  # Add arrow to Siaya
  annotate(geom = "curve", x = 33.65, xend = 34.2 , y = -1.8, yend = -0.0621,
           curvature =-.2,  color = "#35978F", size = 0.75, arrow = arrow(length = unit(1.5, "mm")))+
  # ADD arrow to Garissa
   annotate(geom = "curve", x = 41.1, xend = 40.8 , y = 1, yend = -0.1,
           curvature =-.2,  color = "#8C510A", size = 0.75, arrow = arrow(length = unit(1.5, "mm")))+
  coord_sf(xlim = c(33.8, 42), clip = 'off')+
  labs(title = paste0("Gender Distribution in Counties Across ",kenya_html),
       caption = "Data: RKenyaCensus\n@Amit_Levinson")+
  theme_void()+
  theme(
    text = element_text(family = "IBM Plex Sans"),
    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 22),
    plot.caption = element_text(size = 9, face = 'italic', color = 'gray25'), 
    legend.direction = 'horizontal',
    legend.position=c(0.2,0.13),
    legend.background = element_blank(),
    legend.title.align = 0,
    legend.title = element_markdown(size = 13, color = "gray35"),
    legend.text = element_text(size = 11, color = "gray25"),
    legend.key.size = unit(8,"mm"),
    plot.margin = margin(6,2,6,2, "mm"))

ggsave(here("2021", "week4_rkenya", "rkenya.png"), width = 11, height = 11)
  