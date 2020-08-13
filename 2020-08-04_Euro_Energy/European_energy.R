#### Tidy Tuesday European Energy ----

### DIDN'T GET THIS DONE

library(tidyverse)
library(tidytuesdayR)
library(ggrepel)        # Helper for label placement
library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)


# Load the data
tuesdata <- tidytuesdayR::tt_load('2020-08-04')
energy_types <- tuesdata$energy_types
country_totals <- tuesdata$country_totals

# Countries with missing names
country_totals %>% 
  filter(is.na(country_name))
# Add UK name
country_totals <- country_totals %>% 
  mutate(country_name = 
           case_when(
             country == "UK" ~ "United Kingdom",
             TRUE ~ country_name
           )
  )

# Do country totals and energy types have all the same country?
anti_join(country_totals, energy_types, by = "country")

# Visualize the total energy generated (supplied)
country_totals %>%
  filter(type == "Energy supplied") %>% 
  arrange(country_name) %>% 
  ggplot() +
  geom_col(aes(country_name, `2018`, fill = country_name)) +
  coord_flip() +
  theme(legend.position = "none") 


# How has the proportion of renewable energy production changed since between 2017 and 2019 for European countries.
renew_calcs <- energy_types %>%
  filter(level == "Level 1") %>% 
  mutate(renewable = ifelse(type %in% c("Hydro", "Wind", "Solar", "Geothermal"),
                            "Y", "N")) %>% 
  group_by(country_name) %>% 
  summarize(renew_tot_2016 = sum(`2016`[renewable == "Y"]),
            renew_tot_2017 = sum(`2017`[renewable == "Y"]),
            renew_tot_2018 = sum(`2018`[renewable == "Y"]),
            renew_prop_2016 = sum(`2016`[renewable == "Y"])/sum(`2016`)*100,
            renew_prop_2017 = sum(`2017`[renewable == "Y"])/sum(`2017`)*100,
            renew_prop_2018 = sum(`2018`[renewable == "Y"])/sum(`2018`)*100)

# There is probably a way to pivot longer and create both GWh and Proportion,
# but I can't quite figure it out. So will do it the longer way. 
renew_gwh <- renew_calcs %>% 
  select(country_name, starts_with("renew_tot")) %>% 
  pivot_longer(-country_name, names_to = "Year", values_to = "GWh") %>% 
  mutate(Year = str_sub(Year, -4))

renew <- renew_calcs %>% 
  select(country_name, starts_with("renew_prop")) %>% 
  pivot_longer(-country_name, names_to = "Year", values_to = "Proportion") %>% 
  mutate(Year = str_sub(Year, -4)) %>% 
  left_join(renew_gwh, by = c("country_name", "Year"))

renew$Year <- factor(renew$Year)
renew$GWh <- as.numeric(renew$GWh)

ggplot(renew, aes(x = Year, y = Proportion, group = country_name, colour = country_name)) +
  geom_line() +
  geom_point() +
  theme_light() +
  geom_text_repel(
    data = renew %>% filter(Year == "2018"),
    aes(label = country_name),
    direction = "y",
#    nudge_x = 20,
    size = 3,
    hjust = -1) +
  theme(legend.position = "none") +
  labs(
    title = "Percent Renewable of Total Energy Production",
    x = " ",
    y = " "
    )

# Make a map showing percent renawable by country. countries have had an increase in renewable energy. 
# Much of this below gleaned from beeb22 https://github.com/beeb22/tidytuesday/blob/master/Energy%20usage.R

# I already know that the country names do not all match up with the 
# country names in the {rnaturalearth} data. So just take care of it now.

# Load country outlines
europe <- ne_countries(continent = "europe", scale = "medium", returnclass = "sf") %>% 
  select(name_long, geometry)

renew %>%
  filter(Year == "2018") %>%
  anti_join(europe, by = c("country_name" = "name_long"))



europe %>% 
  filter(name_long != c("Russia", "Iceland")) %>% 
  ggplot() +
  geom_sf() +    
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("Europe map", subtitle = paste0("(", length(unique(europe$name_long)), " countries)"))


#clean up country totals
country_totals_clean <-
  country_totals %>%
  mutate(
    country_name = case_when(
      country_name == "Czech Republic" ~ "Czech Republic",
      country == "CY" ~ "Cyprus",
      country == "MT" ~ "Malta",
      country == "UK" ~ "United Kingdom",
      country == "North Macedonia" ~ "Macedonia",
      country_name == "TR" ~ "Turkey",
      country_name == "Bosnia & Herzegovina" ~ "Bosnia and Herzegovina",
      country == "GE" ~ "Georgia",
      TRUE ~ country_name
    )
  ) 



