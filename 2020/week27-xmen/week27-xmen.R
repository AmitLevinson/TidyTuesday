library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
covers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/covers.csv')


covers

chars_df <- covers %>% 
  #split according to space between words, used specific regex to keep names complete
  mutate(char = str_split(characters_visualized,"\r\n")) %>% 
  # unnest the new list column
  unnest(char) %>% 
  select(issue, char) %>%
  group_by(issue) %>% 
  # create new column to further expand
  mutate(char2 = char) %>% 
  # create combinations using expand
  expand(char, char2) %>% 
  #filter identical words
  filter(char != char2) %>% 
  ungroup() %>% 
  mutate(dual_char = paste0(char, " & ", char2)) %>% 
  add_count(dual_char, name = "n", sort = T)


  

new <- chars_df %>%
  expand(word, word2)

?expand.grid

?complete

library(ggraph)


