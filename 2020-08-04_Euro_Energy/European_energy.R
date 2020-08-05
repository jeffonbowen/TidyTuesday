#### Tidy Tuesday European Energy ----

library(tidyverse)
library(tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2020-08-04')

energy_types <- tuesdata$energy_types

country_totals <- tuesdata$country_totals

# EDA

country_totals %>%
  filter(type == "Energy supplied") %>% 
  ggplot() +
  geom_col(aes(country, `2018`, fill = country)) +
  coord_flip() +
  theme(legend.position = "none") 

# Check totals match country_totals  
totals <- energy_types %>% 
  filter(level == "Level 1") %>% 
  group_by(country_name) %>% 
  summarize(Total = sum(`2018`))

# How has the proportion of renewable energy production changed since between 2017 and 2019 for European countries.

renew_calcs <- energy_types %>%
  filter(level == "Level 1") %>% 
  mutate(renewable = ifelse(type %in% c("Hydro", "Wind", "Solar", "Geothermal"),
                            "Y", "N")) %>% 
  group_by(country) %>% 
  summarize(renew_tot_2016 = sum(`2016`[renewable == "Y"]),
            renew_tot_2017 = sum(`2017`[renewable == "Y"]),
            renew_tot_2018 = sum(`2018`[renewable == "Y"]),
            renew_prop_2016 = sum(`2016`[renewable == "Y"])/sum(`2016`),
            renew_prop_2017 = sum(`2017`[renewable == "Y"])/sum(`2017`),
            renew_prop_2018 = sum(`2018`[renewable == "Y"])/sum(`2018`))

%>% 
  pivot_longer(-country, names_to = c(".value", "set"), names_pattern = "(.)(.)")

hist(renew_prop$renew_prop_2016)  
hist(renew_prop$renew_prop_2017)  
hist(renew_prop$renew_prop_2018)  
