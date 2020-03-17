library(schrute)
library(tidyverse)
library(tidytext)
library(stopwords)
library(igraph)
library(ggraph)
library(extrafont)

head(theoffice)

set.seed(1234)

# Pattern to remove some irrelevant words
str_pattern <- "(^hey$|^yeah$|^ha$|^mm$|^mmm$|^na$|^uh$)"

office_bigram <- 
  theoffice %>% 
  select(season,episode, text) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ", remove = FALSE) %>%
  # remove stop words:
  filter(!word1 %in% stop_words$word, 
         !word2 %in% stop_words$word, 
         # filter bigram with the same word:
         word1 != word2) %>%
  # filter words like hey, yeah, mmm from the regex above:
  filter(!str_detect(word1, str_pattern),
         !str_detect(word2, str_pattern)) %>% 
  count(word1, word2, sort = T) %>% 
  slice(1:30) %>% 
  # Prepare the data for plotting from the {igraph} 
  graph_from_data_frame()

# adding arrows:
p_arrow <- arrow(type = "closed", length = unit(.10, "inches"))

# plot from the {ggraph}
ggraph(office_bigram, layout = "fr")+
  geom_edge_link(aes(edge_alpha = n), arrow = p_arrow, end_cap =circle(.04, "inches"), show.legend = FALSE)+
  geom_node_point(color = "lightblue", size = 3)+
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  labs(title = "Top 30 frequent bigrams throughout 'The office'",
       caption = paste0("bigrams (pair of words) with dupliacted words, stop words and words such as 'yeah', 'uh', mm' were removed. \n",
                        "Darker arrows indicate a higher frequency. Data from: R schrute package | @Amit_Levinson"))+
  theme_void()+
  theme(
        plot.title = element_text(family = "Roboto Condensed", face= "bold", hjust = 0.5, size = 20),
        plot.caption = element_text(face = "italic", family = "Roboto Condensed", hjust = 0.01))

# Save
ggsave("tt_schrute.png", width = 10, height = 6)


# This was made along with the fantastic tidytext book for text analysis
# Check it out here: 
# https://www.tidytextmining.com/