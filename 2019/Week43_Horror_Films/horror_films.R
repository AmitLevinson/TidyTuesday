setwd("C:/Users/amitl/OneDrive/Extra/R/TidyTuesday/Week43_Horror_Films")
library(tidyverse)
library(lubridate)
library(stringr)
library(ggimage)
library(gridGraphics)
library(grid)
library(png)
library(ggrepel)


horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

rating_length <- horror_movies %>% 
  mutate(movie_length = as.numeric(str_replace(movie_run_time," min", ""))) %>% 
  filter(!is.na(movie_length) & !is.na(review_rating)) %>% 
  mutate(new_date = lubridate::dmy(release_date),
         year = ifelse(is.na(new_date), release_date,year(new_date))) %>% 
  select(title, release_country, review_rating, movie_length, language, year) %>% 
  mutate(length_cat = ifelse(movie_length<90, "Under 90 min", ifelse(movie_length>=90 & movie_length <120, "90-120 minutes", "Over 2 hours")))
str(rating_length$review_rating)

avg_review <- mean(rating_length$review_rating, na.rm = T)

top_1 <- rating_length %>% 
  mutate(average_rating = mean(review_rating),
         label = paste0(title, "\n(", language, ")")) %>% 
  arrange(desc(review_rating)) %>% 
  filter(movie_length <=90 &
           review_rating >= quantile(review_rating, 0.995) |
           review_rating <= quantile(review_rating, 0.005))

    movie_length <=90 & movie_length >= 60 &
        review_rating >= quantile(review_rating, 0.975))

#review_rating >= quantile (review_rating, 0.99) &
  #movie_length >30
#image for the plot
top_1$tombstone <- "tomb3.png"

p <- ggplot(top_1 ,(aes(x = movie_length, y = review_rating)))+
  geom_image(aes(image = tombstone), size = 0.05)+
  geom_point()+
  scale_x_continuous(limits = c(60,90), breaks = seq(60,90,15),
                     labels = c("60 minutes", "1 hour", "1.5 hours"),
                     name = "Movie length")+
  geom_label_repel(top_1, mapping = aes(label = title), xlim=c(60,65),
                   force = 50, direction = "y")


                   p
                   
                   aes(label = name, y = 2),
                   fontface = 'bold',
                   color = "mediumpurple",
                   xlim=c(8,9), 
                   ylim=c(0,15))

  geom_text(aes(label = label), nudge_y = 0.22, hjust = 0, 
            nudge_x = -1.6,
            fontface= "bold")

p



         movie_length = as.numeric(movie_length)) %>% 
  f

   str(horror_movies$movie_length)

   mean(horror_movies$movie_length)

   clean_dates <- horror_movies %>% 
  mutate(new_date = lubridate::dmy(release_date),
         year = ifelse(is.na(new_date), release_date,year(new_date))
  )


clean_dates %>% group_by(year) %>% 
  count(year)


clean_dates$year  <-  ifelse(is.na(clean_dates$new_date), year(clean_dates$new_date),clean_dates$release_date)


year <- as.numeric(format(date,'%Y'))

  mutate(new_date[new_date == NA] = release_date)

mutate(Answer = ifelse(max(Answer, na.rm=TRUE)== -Inf, NA, 
                       as.integer(max(Answer, na.rm=TRUE))))

NAs <- clean_dates %>% 
  filter(is.na(new_date)) %>% 
  group_by(release_date) %>% 
  summarise(n = n())
  

str(clean_dates)

  
  
  mutate(date_release = as.Date(release_date, format =  "%d-%B-%y")) %>% 
  