library(tidyverse)
library(tidytext)
library(wordcloud2)
library(htmlwidgets)

# Load data
beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')

b_words <- beyonce_lyrics %>% 
  # Unnest to single words
  unnest_tokens(word, line) %>% 
  count(word, sort = T) %>% 
  anti_join(stop_words) %>%
  filter(n > 4) %>% 
  # normalize words
  mutate(n = n * 0.8,
         # Normalize really high freq words
         n = ifelse(n > 500, n - 300, n))

# Save as ojbect
p <- wordcloud2(b_words, figPath = "2020/week_40_b_t/note.png", size = 1.8, color = "black")

# Save as html, open and then save as image
saveWidget(p, here::here("2020", "week_40_b_t", "p.html"),selfcontained = F)
