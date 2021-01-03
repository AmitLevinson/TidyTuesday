library(purrr)
library(stringr)
library(dplyr)
library(ggplot2)
library(showtext)

font_add_google("Roboto Condensed", "Roboto")
showtext_auto()

# Read all R or Rmd files
files <- list.files( pattern = "\\.R$|.Rmd$", recursive = TRUE)

# Remove the packages-plot.R file
files <- files[!str_detect(files, "packages-plot.R")]

# Get names
file_names <- str_extract(files, '[^/]+(?=\\.)')

#Read all files
file_lines <- map(files, readLines)

# Get the names
names(file_lines) <- file_names

# Get packages with regex
file_packages <- map_dfr(file_lines, ~ tibble(package_name = str_extract(.x, "((?<=library\\().+(?=\\))|\\w+(?=::))")),.id = "tidytuesday") %>% 
  filter(!is.na(package_name))%>% 
  distinct(tidytuesday, package_name)


# Plot
file_packages %>% 
  count(package_name, sort = T) %>%
  mutate(package_name = factor(package_name, levels = rev(package_name))) %>% 
  slice(1:15) %>% 
  ggplot()+
  geom_col(aes(y= package_name, x = n), fill = "gray45")+
  labs(title = "Frequently used packages in #Tidytuesday",
       subtitle = "Plot is rendered on every 'initial commit' to this repository, showing my 15 most frequently\nused packages in #TidyTuesday",
       x = "Number of times used", y = "Package name",
       caption = paste0("Total scripts: ", length(file_names),
                        "\nLast updated",format(Sys.Date(), "%b %d, %Y")))+
  theme_minimal()+
  theme(
    text = element_text(family = "Roboto"),
    plot.title = element_text(size = 20),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 13, color = "gray20"),
    plot.caption = element_text(color = "gray30", face = "italic"),
    axis.title = element_text(color = "gray40", size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    plot.margin = unit(c(4,2,2,4), "mm")
  )

ggsave("extra/packages-used.png", width = 8, height = 5)