library(tidyverse)
library(wordcloud2)
library(hrbrthemes)
library(cowplot)
#devtools::install_github("lchiffon/wordcloud2")


cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")
head(cran_code)
dim(cran_code)

cran_code %>% 
  count(pkg_name,version,sort = T)
options(digits = 0)

cran_code

#New attempt
clean_avg <- cran_code %>% 
  add_count(language) %>% 
  filter(n > 20) %>% 
  group_by(language) %>% 
  summarise(count = n(),
    avg = median(code)) %>% 
  mutate(language = fct_reorder(language, avg)) %>% 
  arrange(desc(avg))

clean_avg



ggplot(clean_avg, aes(x = language, y = avg))+
  geom_segment(aes(x = language, xend = language, y= 0, yend = avg), color = "grey45")+
  geom_point(aes(size = count), color = "coral1")+
  coord_flip()+
  labs(title = "Median number of code lines from programming languages used in R packages", subtitle = "Point size represents frequency of programming language across packages. Only languages used in more than \n15 packages are displayed.",
       y = "Median lines of code" ,x = "Programming language", colour = "Language Frequecy")+
  theme_cowplot(font_size = 12)+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.title = element_blank(),
    plot.background = element_rect(fill = "ivory")
    #panel.grid.minor = element_blank()
  )

#Try ggridges?
ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges2()

clean_ggridges <- 
  cran_code %>% 
  add_count(language) %>% 
  filter(n >2000) %>% 
  mutate (code_new = log(code),
          language = as.factor(language))

str(clean_ggridges$language)

clean_ggridges
  clean_ggridges %>% 
  count(language)

library(ggridges) 

  ggplot(clean_ggridges, aes(x = code, y = language))+ 
  geom_density_ridges2(rel_min_height = )





ggsave("cran.png", width =10, height = 6)
  
theme_set(theme_cowplot())
library(cowplot)

view(filtered)
high <- cran_code %>% 
  arrange(desc(code))
view(clean_avg)

clean <- cran_code %>% 
  group_by(language) %>% 
  summarise(count = n()) %>%
  filter(language != "R") %>% 
  arrange(desc(count))

view(clean)
clean
wordcloud2(clean, size = 1.6)
wordcloud2(clean, size = 1.5, figPath = "test.jpeg")

letterCloud(clean,"R")
