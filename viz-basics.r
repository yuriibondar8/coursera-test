


# 2022-10-10 Visualization basics in R and tidyverse

# Visualization packages, most popular:
# ggplot2, gganimate, ggridges
# Leaflet, Highcharter, Patchwork, Plotly, Lattice, RGL, Dygraphs

library(ggplot2)
library(palmerpenguins)


# Load penguins dataset
data(penguins)
View(penguins)


# Creating a viz using ggplot, both variations create the same plot
ggplot(data = penguins) +
 geom_point(mapping = aes(x = flipper_length_mm, y = body_mass_g))

ggplot(data = penguins, mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
 geom_point()






#