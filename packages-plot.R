library(purrr)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# Read all R or Rmd files
files <- list.files(path = "C:/Users/amitl/R_code/tidytuesday", pattern = "\\.R$|.Rmd$", recursive = TRUE)

# Remove the packages.R file
files <- files[-length(files)]


# Get names
file_names <- str_extract(files, '[^/]+(?=\\.)')

#Read all files
file_lines <- map(files, readLines)

# Get the names
names(file_lines) <- file_names

# Get packages with regex
file_packages <- map_dfr(file_lines, ~ tibble(package = str_extract(.x, "((?<=library\\().+(?=\\))|\\w+(?=::))")),.id = "tidytuesday") %>% 
  dplyr::filter(!is.na(package))

file_packages2 %>% 
  anti_join(file_packages)

# Plot
file_packages %>% 
  count(package, sort = T) %>%
  slice(1:15) %>% 
  ggplot()+
  geom_col(aes(y= fct_reorder(package,n), x = n))+
  labs(title = "Top 15 frequently used packages in #Tidytuesday",
       subtitle = "Plot is automated on every GitHub push this repository",
       x = "Number of times used", y = "Package name",
       caption = paste0("Last updated:\n",format(Sys.Date(), "%b %d, %Y")))+
  theme_minimal()+
  theme(
    text = element_text(family = "Calibri"),
    plot.title = element_text(size = 16),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 12, color = "gray20"),
    plot.caption = element_text(color = "gray30", face = "italic"),
    axis.title = element_text(color = "gray40"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 12),
    plot.margin = unit(c(4,2,2,4), "mm")
  )

ggsave("extra/packages-used.png", width = 8, height = 5)
# usethis::use_github_action("render-readme.yaml")
# usethis::use_github_actions()
# https://fromthebottomoftheheap.net/2020/04/30/rendering-your-readme-with-github-actions/