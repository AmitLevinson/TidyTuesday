library(tidyverse)
library(stringr)
library(ggbeeswarm)
library(ggrepel)
library(extrafont)
library(ggthemr)
ggthemr("flat dark", type = 'outer')

horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

#Let's clean the data frame
rating_length <- horror_movies %>% 
  #I first want to have the minutes as numeric value
  mutate(movie_length = as.numeric(str_replace(movie_run_time," min", ""))) %>% 
  #once we did that, let's remove all NA's
  filter(!is.na(movie_length) & !is.na(review_rating)) %>% 
  #choosing the specific variables needed
  select(title, review_rating, movie_length) %>% 
  #creating length categories of the movies
  mutate(length_cat =  
         ifelse(movie_length <80, "Under 1:20",
         ifelse(movie_length >=80 & movie_length < 100, "1:20-1:40", "1:40+")),
         #Now let's reorder the factor level 
         length_cat = factor(length_cat, levels = c("Under 1:20","1:20-1:40", "1:40+")))
         
#creating a subset data frame with only top 3 rated for each group
top_3 <- 
    rating_length %>% 
    group_by(length_cat) %>% 
    top_n(n = 3, wt = review_rating) %>% 
#The titles had the year they were released, this created a messey display on the plot
    mutate(title = str_replace(title, " \\(.*\\)", ""))
    
  
s <- ggplot(rating_length, aes(y = review_rating, x = length_cat))+
  #Creating a beeswarm plot
  geom_quasirandom(alpha = 0.7, width = 0.3)+
  labs(x= "Length (in hours)",y= "Rating", title = "Prefer the shorter or longer horror movies?", 
       subtitle = "Only a slight advantage in ratings exists for the longer horror movies; Points represent mean rating \nvalue for each category. Titles and duration for top 3 rated movies in each category are displayed",
       caption = "Data: imdb.com | @Amit_Levinson")+
  #Adding a point representing the Mean value
  stat_summary(fun.y = "mean", geom = "point", size = 3, color = "black") +
  #Adding a line to show the difference in mean 
  stat_summary(fun.y = "mean", geom = "line", aes(group = 1), color = "black",
               size = 1.3)+
  #Let's add the movie titles for top 3
  geom_text_repel(top_3,
                  mapping = aes(label = paste0(title, "\n(", movie_length, " min)")),
                  family = "Miriam", color = "white",
                  segment.size = 0.8,
                  arrow = arrow(length = unit(0.01, 'npc')),
                  point.padding = unit(0.8, 'lines'),
                  box.padding = unit(0.9, 'lines'))

#Just adding the theme and a specific non-formal text for the occasion
s +theme(text=element_text(family = "MV Boli"),
         axis.text = element_text(size = 11),
         plot.title = element_text(size = 18),
         plot.subtitle = element_text(size = 12)
         )

#Save the plot
ggsave("horror_movie_length.png", width =10, height = 6)
