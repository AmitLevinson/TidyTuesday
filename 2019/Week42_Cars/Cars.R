library(tidyverse)

big_epa_cars <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv")

str(big_epa_cars)
view(big_epa_cars)


ford <- 
  big_epa_cars %>% 
  group_by(make, model) %>% 
  filter(make =="Ford") %>% 
  count(n())
  
V_class <- 
  big_epa_cars %>% 
  select(VClass, barrels08, fuelCost08,city08, highway08, co2TailpipeGpm, fuelCost08) %>% 
group_by(VClass) %>% 
  summarise(meancity = mean(city08), meanhighway = mean(highway08), meanco2 = mean(co2TailpipeGpm),
            n = n()) %>% 
  arrange(desc(n))


fuel_type <- 
  big_epa_cars %>% 
  select(year, model, make, VClass, barrels08, fuelCost08,city08, highway08, co2TailpipeGpm, fuelCost08) %>% 
  group_by(make,year) %>% 
  summarise(MPG = mean(barrels08),n = n()) %>% 
  arrange(desc(n))

filter(n >= 100)

compact_cars <- 
  big_epa_cars %>% 
  select(year, model, make, VClass, barrels08, fuelCost08,city08, highway08, co2TailpipeGpm, fuelCost08) %>% 
  group_by(VClass) %>% 
  mutate(pricepermile = fuelCost08/15000) %>% 
  summarise(Highway = mean(highway08), City = mean(city08), pricepermile = mean(pricepermile),
            n = n()) %>% 
  arrange(desc(n))


meancities <- mean(compact_cars$City)
meanhighway <- mean(compact_cars$Highway)


sd_cities <- sd(compact_cars$City)
sd_cities

dis_from_mean_fuel <- 
  compact_cars %>% 
  mutate(Milesforcity = City - meancities, Mileforhighway = Highway - meanhighway) %>% 
  select(VClass, Milesforcity, Mileforhighway)




  arrange(desc(n, CO2_Average))


 
  gather("catgeories", "Value", 2:3)

p <- compact_cars %>% 
  ggplot(aes(x = make, y = Value))+
  geom_bar(stat = "identity")

p+  facet_grid(. ~ catgeories)


meancity = mean(city08), meanhighway = mean(highway08)

  group_by(year) %>%
  summarise(n= n())

over_1000 <- big_epa_cars %>% 
  group_by(make) %>% 
  count() %>% 
  arrange(desc(n))

mean_make <- big_epa_cars %>% 
  group_by(make) %>% 
  summarise(mea = mean(co2TailpipeGpm, na.rm = T))

