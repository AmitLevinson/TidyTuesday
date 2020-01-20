library(tidyverse)
library(stopwords)
library(tidytext)
library(textdata)
library(scales)
library(showtext)

#Read the data
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

#unnesting words
word_track <- spotify_songs %>% 
  select(track_name) %>% 
  unnest_tokens(word, track_name) %>% 
  select(word)

#searching for sentiment matches
word_count <- word_track %>% 
  count(word, sort = T) %>% 
  #Matching with sentiment date.frame
  inner_join(get_sentiments("bing")) %>% 
  #reordering factor acording to frequency
  mutate(word = fct_reorder(word, n)) %>% 
  #If you decide to use 'nrc' sentiment, not neccessary for 'bing'
  filter(sentiment %in% c("positive", "negative")) %>% 
  #groupiing so we can slice the top 15 from each group
  group_by(sentiment) %>% 
  slice(1:15)

#Let's add a fun font for the plot:
font_add_google("Boogaloo", "Boogaloo")
showtext_auto()

ggplot(word_count,aes(word, n))+
  #Create the bars to match spotify logo
  geom_col(fill = "black")+
  coord_flip()+
  #'free_y' helps in organizing the facet_wrap neatly
  facet_wrap(. ~ sentiment,scales = "free_y")+
  #creating a log10 scale
  scale_y_log10(breaks = c(1,10,100,1000),
                labels = trans_format("log10", math_format(10^.x)))+
  labs(title = "Sentiment analysis of words in song titles",
       subtitle = paste0("Analysis was conducted on 5,000 songs from spotify.",
                         " Top 15 most frequent words are shown."),
       caption = "Data: @kaylinquest | plot: @Amit_Levinson")+
  theme_minimal()+
  theme(text = element_text(family = "Boogaloo"),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 32),
        axis.text.x = element_text(size = 26),
        strip.text = element_text(size = 30),
        plot.caption = element_text(size = 18, face = "italic"),
        plot.title = element_text(size = 46),
        plot.subtitle = element_text(size = 26),
        #adding a nice background to match spotify logo
        panel.background = element_rect(fill = "lightgreen"),
        plot.background = element_rect(fill = "lightgreen"),
        panel.grid.major = element_line(color = "grey70", size = 0.2),
        panel.grid.minor = element_line(color = "grey70", size = 0.2))
        
ggsave("spotify2.png", height = 4, width = 6)

#instead of the unnest_token you can use the stringr::str_extract_all approach
#(for some reason it gives us 10 less words, plus it seems a little more complicated):

#word_track_stringr <- tibble(word = unlist(stringr::str_extract_all(spotify_songs$track_name, boundary("word"))))