# Palmer Penguins

library(tidyverse)
library(tidytuesdayR)
library(ggpairs)


tuesdata <- tidytuesdayR::tt_load('2020-07-28')

penguins <- tuesdata$penguins

penguins

table(penguins$sex)

penguins <- na.omit(penguins)

# Basic distribution

penguins %>% 
  ggplot(aes(body_mass_g, fill = sex)) +
  geom_histogram(position = position_dodge(),
                 binwidth = 90) +
  facet_grid(species ~ .)

penguins %>% 
  ggplot(aes(body_mass_g, fill = sex)) +
  geom_boxplot() +
  facet_grid(species ~ .)

penguins %>% 
  ggplot(aes(body_mass_g, fill = sex)) +
  geom_density() +
  facet_grid(species ~ .)





# Scatter
penguins %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm, colour = sex)) +
  geom_point() +
  geom_smooth(method = "lm")
  facet_grid(species ~ .)
