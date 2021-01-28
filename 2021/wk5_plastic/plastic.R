library(here)
library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(ggplot2)
# Load data for the week
tuesdata <- tt_load(2021, week = 5)
plastics <- tuesdata$plastics


plastics %>% 
  filter( !parent_company %in% c("Grand Total", "Unbranded", "null", "NULL"), year == 2020) %>% 
  select(country, parent_company, pp) %>% 
  group_by(country) %>% 
  filter(n() >= 100, pp != 0) %>% 
  ggplot()+
  geom_histogram(aes(x = pp), bins = 30)+
  facet_wrap(~country, scales = "free")
  

library(ggridges)
plastics %>% 
  filter( !parent_company %in% c("Grand Total", "Unbranded", "null", "NULL"), year == 2020) %>% 
  pivot_longer(cols = hdpe:pvc, names_to = "category") %>% 
  group_by(category) %>% 
  top_n(n = 10, value) %>% 
  ggplot(aes(x = value,  y = parent_company))+
  geom_col()+
  facet_wrap(~ category, scales = "free_y")
  
psych::pca(nfactors = 3)
  


  
  select(year, parent_company, ldpe) %>% 
  arrange(-ldpe)




plastics %>%  
  pivot_longer(cols = hdpe:pvc, names_to = "category") %>% 
  filter(country == "United States of America", parent_company != "Grand Total") %>% 
  group_by(category, year) %>% 
  filter(!is.na(value)) %>% 
  top_n(n = 10, value) %>% 
  ggplot(aes(x = value,  y = parent_company))+
  geom_col()+
  facet_wrap( year ~ category, scales = "free")+
  ggsave(here("extra","images", "progress", "wk5_plastic", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), type = 'cairo', height = 4, width = 6)


