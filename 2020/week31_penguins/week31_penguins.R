library(tidyverse)
library(highcharter)

penguins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')

penguins_column <- penguins %>% 
  group_by(species) %>% 
  summarise(across(c(bill_length_mm, body_mass_g), .fns = list(~ mean(.x, na.rm = T), ~ sd(.x, na.rm = T)), .names = "{col}_fn{fn}"))

penguins_drilldown <- penguins %>%
  select(species, bill_length_mm, body_mass_g) %>% 
  group_nest(species) %>% 
  mutate(id = species,
         type = 'column')

dat_boxplot <- data_to_boxplot(penguins, bill_length_mm, group_var = species, drilldown = species)


highchart() %>% 
  hc_xAxis(type = "category", color = "grey") %>% 
  hc_add_series_list(dat_boxplot) %>% 
  hc_drilldown(allowPointDrilldown = TRUE,
               series = list_parse(penguins_drilldown))

?data_to_boxplot()
hchart(dat_boxplot, "boxplot", hcaes(x = bill_length_mm,  name = species))

  library(highcharter)
  data(gapminder, package = "gapminder")

  gapminder_column,
  "boxplot",
  hcaes(x = continent, y = pop, name = continent, drilldown = continent),
  name = "Population",
  colorByPoint = TRUE

  
  gapminder2007 <- gapminder %>% 
  filter(year == max(year)) %>% 
  select(-year) %>% 
  mutate(pop = pop/1e6) %>% 
  arrange(desc(pop))
  
  
  gapminder_column <- gapminder2007 %>%
    group_by(continent) %>% 
    summarise(
      lifeExp = weighted.mean(lifeExp, pop),
      gdpPercap = weighted.mean(gdpPercap, pop),
      pop = sum(pop)
    ) %>% 
    mutate_if(is.numeric, round) %>% 
    arrange(desc(pop)) %>% 
    mutate(continent = fct_inorder(continent))
  
  gapminder_drilldown <- gapminder2007 %>% 
  group_nest(continent) %>% 
  mutate(
    id = continent,
    type = "column",
    # in the drilldown we'll give the mapping via creating the columns
    data = map(data, mutate, name = country, y  = pop),
    data = map(data, list_parse)
  )
  