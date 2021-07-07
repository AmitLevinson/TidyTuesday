library(dplyr)
library(ggplot2)
library(tidytext)
library(tidylo)

departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

departures %>% 
  count(fyear, departure_code) %>% View()
  ggplot(aes(x = fyear, y = n, group = departure_code))+
  geom_line()

  
notes_departures <- departures %>% 
  unnest_tokens(text, notes, token = "words")

log_notes <- notes_departures %>% 
  filter(!is.na(departure_code)) %>% 
  count(departure_code, text, sort = T)
  # bind_log_odds(departure_code, bigram, n) %>% 
  # arrange(-log_odds_weighted)

  log_notes %>% 
  anti_join(stop_words, by = c("text" = "word")) %>% 
  group_by(text) %>% 
  filter(sum(n) >= 10) %>% 
  ungroup() %>% 
  pivot_wider(names_from = departure_code, values_from = n, values_fill = 0) %>% 
  mutate(across(where(is.numeric), list(~(. + 1) / (sum(.) + 1)))) %>%
  
  mutate(logratio = log(David / Julia)) %>%
    arrange(desc(logratio))
    
    # 
    # as.matrix() %>% 
    # psych::pca(nfactors = 8)

  library(tidyr)
log_notes %>% 
  anti_join(stop_words, by = c("bigram" = "word")) %>% 
  group_by(departure_code) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(bigram = reorder(bigram, log_odds_weighted)) %>%
  ggplot(aes(bigram, log_odds_weighted, fill = departure_code)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~departure_code, scales = "free") +
  coord_flip() +
  labs(x = NULL)


word_ratios <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, person) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  pivot_wider(names_from = person, values_from = n, values_fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(David / Julia)) %>%
  arrange(desc(logratio))