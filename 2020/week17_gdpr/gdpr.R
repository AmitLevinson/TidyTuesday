# Load packages and prepare data -----------------------------------------------------------
library(tidyverse) # for data wrangling
library(highcharter) # See below for a useful tutorial to work with highcharter maps
library(scales) # creating a Euro value scale
library(RColorBrewer) # Choosing a color palette
library(htmlwidgets) # Saving our html file

# Get Tidytuesday data:
gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
glimpse(gdpr_violations)

# Check map from Highchart if we need another column to join by
get_data_from_map(download_map_data("custom/europe")) %>% 
  glimpse() 

# Function to turn our values into Euro character in our final analysis
euro <- dollar_format(prefix = "", suffix = "\u20ac")

# Manipulate data ---------------------------------------------------------
gdpr <- gdpr_violations %>% 
  filter(price != 0) %>% 
  group_by(name) %>%
  summarise(count = n(),
            median_gdpr = median(price)) %>%
  ungroup() %>%
  # Reformat our values for nice display
  mutate(price_eur = euro(median_gdpr))
# Create map --------------------------------------------------------------

price_map <- hcmap(map = "custom/europe",
                   data = gdpr,
                   value = "median_gdpr", # value that our gradient scale will be mapped by
                   joinBy = c("name", "name")) %>%  # Join our data by column that match
  hc_mapNavigation(enabled = TRUE) %>%
  hc_legend(layout = 'horizontal',
            align = 'center',
            valueDecimals = 0) %>% 
  hc_tooltip(formatter = JS("function() {
  return ('<br><b>Country:</b> ' +this.point.name +
          '<br><b>Total issued:</b> ' + this.point.count +
          '<br><b>Median fine:</b> '+ this.point.price_eur)}"))

# Look at the color color list to retrieve Minimum and Maxium:
brewer.pal(name = "YlOrRd", n = 9)

# Add color, title and credits:
price_map <- price_map %>%
  hc_colorAxis(minColor = "#FFFFCC", maxColor = "#800026") %>% 
  hc_title(text = "General Data Protection Regulation median fines") %>%
  hc_subtitle(text = "Data excludes violations of zero value fines") %>% 
  hc_credits(enabled = TRUE,
             text = "Data: Privacy Affairs | @Amit_Levinson",
             href = "https://github.com/AmitLevinson/TidyTuesday/tree/master/2020/week17_gdpr")


# Fine tuning of fonts and theme:
my_theme <- hc_theme(chart = list(backgroundColor = "white"),
                     title = list(style = list(fontFamily = "Roboto Condensed")),
                     subtitle = list(style = list(fontFamily = "Roboto Condensed", color = "gray")),
                     legend = list(itemStyle = list(fontFamily = "Roboto Condensed")),
                     itemHoverStyle = list(color = "gray"))

final_map <- price_map %>% 
  hc_add_theme(my_theme)

saveWidget(final_map, "hc_gdpr.html", selfcontained = TRUE, title = "Tidytuesday GDPR violations", knitrOptions = list(out.width = 40))

# Please check out the following blog post for a thourough explanation to use maps in {highcharter}:
# https://kcuilla.netlify.app/post/maps-in-r/