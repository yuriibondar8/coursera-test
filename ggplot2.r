


# 2022-10-17 Hands-On Activity: Using ggplot
library(tidyverse)
library(palmerpenguins)
library(ggthemes)

data(diamonds)
data(penguins)


# Load hotel bookings data from a csv file
setwd("/Users/yuriibondar/Desktop/c")
hbookings <- read.csv("Lesson2_hotel_bookings.csv")
glimpse(hbookings)


# Create a plot
ggplot(data = hbookings) +
  geom_point(
    mapping = aes(x = lead_time, y = children)
  )

ggplot(data = hbookings) +
  geom_point(
    mapping = aes(x = stays_in_weekend_nights, y = children)
  )


# Geom smooth
ggplot(data = penguins) +
  geom_smooth(
    mapping = aes(
      x = flipper_length_mm,
      y = body_mass_g,
      linetype = species,
      color = species
    ),
    method = "loess",
    span = 1
    # se = FALSE -- removes confidence bands
  )
# Geom point
ggplot(data = penguins) +
  geom_point(
    mapping = aes(
      x = flipper_length_mm,
      y = body_mass_g,
      colour = species,
      shape = species,
      size = species,
      alpha = species
    )
  )
# Combined
ggplot(data = penguins) +
  geom_jitter(
    mapping = aes(
      x = flipper_length_mm,
      y = body_mass_g,
      shape = sex
    )
  ) +
  geom_smooth(
    mapping = aes(
      x = flipper_length_mm,
      y = body_mass_g,
      linetype = species,
      color = species
    )
  ) +
  facet_wrap(~species)
# Geom bar
ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(
      x = cut,
      fill = clarity
    )
  )

# Faceting
# Facet wrap -- use for singe variable
ggplot(data = penguins) +
  geom_point(
    mapping = aes(
      x = flipper_length_mm,
      y = body_mass_g,
      color = species
    )
  ) +
  facet_wrap(~species)

ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = color, fill = cut)
  ) +
  facet_wrap(~cut)

# Facet grid -- to facet with two variables
ggplot(data = penguins) +
  geom_point(
    mapping = aes(
      x = flipper_length_mm,
      y = body_mass_g,
      color = species
    )
  ) +
  facet_grid(sex~species)


# Annotation
ggplot(data = penguins) +
  geom_point(
    mapping = aes(
      x = flipper_length_mm,
      y = body_mass_g,
      color = species
    )
  ) +
  xlab("Flipper Lenght") +
  ylab("Body Mass") +
  labs(
    title = "Palmer Penguins: Body Mass vs Flipper Lenght",
    subtitle = "Sample of Three Penguin Species",
    caption = "Data collected by Dr.Kristen Gorman"
  ) +
  annotate(
    "text",
    x = 220,
    y = 3500,
    label = "The Gentoos are the largest",
    color = "purple",
    fontface = "bold",
    size = 4.5,
    angle = 25
  )


# Saving your visualizations
ggsave("Three Penguins Species.png")







#