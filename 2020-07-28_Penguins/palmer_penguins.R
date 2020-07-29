# Palmer Penguins

library(tidyverse)
library(tidytuesdayR)
library(GGally)
library(vegan)
library(rpart)
library(rpart.plot)


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


ggpairs(data = penguins, columns = 3:6, ggplot2::aes(colour = species))


# Scatter
penguins %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm, colour = sex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(species ~ .)

penguins %>% 
  ggplot(aes(bill_length_mm, bill_depth_mm, colour = sex, size = body_mass_g)) +
  geom_point() +
  facet_grid(species ~ .)


# Questions
  # Can you predict the species based on the morphometric data?

# Try NMDS
pengv <- penguins %>% 
  select(3:6)

nmds <- metaMDS(pengv, distance = "bray")

# extract NMDS scores (x and y coordinates)
data.scores = as.data.frame(scores(nmds))

# add columns to data frame 
data.scores$species <- penguins$species
data.scores$sex <- penguins$sex

n_plot <- ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 2, aes(shape = sex, colour = species))+ 
  theme(axis.text.y = element_text(colour = "black", size = 10, face = "bold"), 
        axis.text.x = element_text(colour = "black", face = "bold", size = 10), 
        legend.text = element_text(size = 10, face ="bold", colour ="black"), 
        legend.position = "right", axis.title.y = element_text(face = "bold", size = 11), 
        axis.title.x = element_text(face = "bold", size = 10, colour = "black"), 
        legend.title = element_text(size = 11, colour = "black", face = "bold"), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        legend.key=element_blank()) + 
  labs(x = "NMDS1", colour = "Species", y = "NMDS2", shape = "Sex") 
n_plot

ggsave(plot = n_plot, "nmds_plot.png")

# How about a predictor tool?

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
                   fallen.leaves = TRUE)

ggsave(data = tree, file = "penguin_class_tree.png")
