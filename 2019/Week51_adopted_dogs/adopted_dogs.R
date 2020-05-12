library(tidyverse)
library(treemapify)
library(RColorBrewer)
library(showtext)

#load data
dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

dog_sum <- dog_descriptions %>% 
  group_by(breed_primary) %>% 
  #getting total number of dogs per breed
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  #Filtering to have labels only for breeds with count >600
  mutate(name_label = case_when(n >600 ~ breed_primary,
                                TRUE ~ ""),
         #Adding another row for label name of breeds with long names
         name_label = case_when(name_label == "American Staffordshire Terrier" ~ "American Staffordshire \n Terrier",
                                name_label == "Australian Cattle Dog / Blue Heeler" ~ "Australian Cattle Dog /\n Blue Heeler",
                                TRUE ~ name_label))

#Add google font  
font_add_google("IBM Plex Sans", "IBM Plex Sans")
#having the font show
showtext_auto()
devtools::install_github("clauswilke/ggtext")

ggplot(data = dog_sum,aes(area = n, fill = n, label = name_label))+
  #Creates the treemap - note the start function for positioning the first box
  geom_treemap(fill = "gray75", color = "white",start = "topleft")+
  #Adding the text on the boxes
  geom_treemap_text(color = "black", place = "centre", size = 42, start = "topleft", family = "IBM Plex Sans")+
  labs(title = "Dog breeds for adoption in the US", subtitle = paste0("Only breeds with more than 600 counts are displayed"),
                                                                 caption = "Data: The Pudding | @Amit_Levinson")+
  theme(text = element_text(family = "IBM Plex Sans"),
        plot.title = element_text(size = 60, face = "bold"),
        plot.subtitle = element_text(size = 36),
        plot.caption = element_text(size = 26, face = "italic"))
        #plot.margin = margin(0.7,0.7,0.7,0.7,"mm"))

#Saving plot
ggsave("total_breed.png", width = 10, height = 6)

