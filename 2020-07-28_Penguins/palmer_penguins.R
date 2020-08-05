### Tidy Tuesday Palmer Penguins ---


# I posted a plotly 3d Scatterplot for Tidy Tuesday. The code below also includes some other exploration. 


# Data from the PalmerPenguins package. 
#   Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: Palmer
#   Archipelago (Antarctica) penguin data. R package version 0.1.0.
#   https://allisonhorst.github.io/palmerpenguins/. doi:
#   10.5281/zenodo.3960218.


library(tidyverse)
library(tidytuesdayR)
library(GGally)
library(vegan)
library(rpart)
library(rpart.plot)
library(plotly)

# Load data
tuesdata <- tidytuesdayR::tt_load('2020-07-28')
penguins <- tuesdata$penguins
head(penguins)

penguins <- na.omit(penguins)

# Basic exploration
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

ggpairs(data = penguins, columns = 3:6, ggplot2::aes(colour = species))

# Scatterplots
penguins %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm, colour = sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(species ~ .)

penguins %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm, colour = sex, size = body_mass_g)) +
  geom_point() +
  facet_grid(species ~ .)

# 3D Scatterplot using Plotly

# legend parameters
l <- list(
  font = list(
    family = "sans-serif",
    size = 14,
    color = "#000"),
  bgcolor = "#E2E2E2",
  bordercolor = "#FFFFFF",
  borderwidth = 2)

# Make the plot
p <- plot_ly(penguins, 
             x = ~bill_length_mm, y = ~bill_depth_mm, z = ~flipper_length_mm) %>%
  add_markers(color = ~species,
              symbol = ~sex,
              symbols = c('circle','x'),
              marker = list(size = 4)) %>%  
  layout(scene = list(xaxis = list(title = 'Bill Length',
                                   linewidth = 3,
                                   zerolinewidth = 2),
                      yaxis = list(title = 'Bill Depth',
                                   linewidth = 3,
                                   zerolinewidth = 2),
                      zaxis = list(title = 'Flipper Length',
                                   linewidth = 3,
                                   zerolinewidth = 2)),
         legend = l)
p                               

# Used ScreenToGif for motion capture GIF. https://www.screentogif.com/

# How about a predictor tool? Using regression trees.

# Approach from https://github.com/jack-davison/TidyTuesday/blob/master/R/2020_07_28_Penguins.R

fitr <- rpart(data = penguins, formula = species ~ bill_length_mm + bill_depth_mm, 
              flipper_length_mm + body_mass_g)
fitr

summary(fitr)

# Print table of optimal prunings
printcp(fitr)

plot(fitr)
text(fitr)

tree <- rpart.plot(fitr, 
                   type = 5, 
                   under = TRUE, 
                   fallen.leaves = TRUE,
                   cex.main = 1.1,
                   main = "Classification of Three Penguin Species Using Bill Measurements")

write(data = tree, file = "penguin_class_tree.png")
