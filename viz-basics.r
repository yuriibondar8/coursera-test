


# 2022-10-10 Visualization basics in R and tidyverse

# Visualization packages, most popular:
# ggplot2, gganimate, ggridges
# Leaflet, Highcharter, Patchwork, Plotly, Lattice, RGL, Dygraphs

library(ggplot2)
library(ggthemes)
library(bslib)
library(RColorBrewer)
library(wesanderson)
library(palmerpenguins)


# Load penguins dataset
data(penguins)
View(penguins)

penguins_clean <- penguins %>%
 drop_na()


# Creating a viz using ggplot, both variations create the same plot
ggplot(data = penguins) +
 geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g))

ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
 geom_point()




ggplot(data = penguins_clean) +
 geom_point(
  mapping = aes(x = flipper_length_mm, y = body_mass_g, colour = sex)
 ) +
 geom_smooth(
  mapping = aes(x = flipper_length_mm, y = body_mass_g, colour = sex),
  method = lm,
  se = FALSE
 ) +
 facet_wrap(~species) +
 scale_color_brewer(name = "", palette = "Dark2") +
 xlab("Flipper length (mm)") +
 ylab("Body mass (g)") +
 theme_minimal()


# Boxplot
ggplot(data = penguins_clean) +
 geom_boxplot(
  mapping = aes(
   x = species, y = flipper_length_mm, fill = species
  )
 ) +
 scale_fill_brewer(name = "", palette = "Set2") +
 xlab("Species") +
 ylab("Flipper length (mm)") +
 theme_minimal()

# Violin
ggplot(data = penguins_clean) +
 geom_violin(
  mapping = aes(
   x = species,
   y = body_mass_g,
   fill = species
  )
 ) +
 scale_fill_brewer(name = "", palette = "Set2") +
 xlab("Species") +
 ylab("Body mass (g)") +
 theme_minimal()

# Combine
# install.packages("ggpubr")
# library(ggpubr)

install.packages("ggpattern")
library(ggpattern)

ggplot(data = penguins_clean) +
 geom_violin_pattern(
  mapping = aes(x = species, y = body_mass_g, pattern = species),
  pattern_fill = "black",
  pattern_colour = "black",
  pattern_density = 0.2
 ) +
theme_bw()



# scale_color_manual(values = wes_palette(n = 2, name = "Royal1"))


#