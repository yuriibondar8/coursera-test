


# 2022-10-17 Hands-On Activity: Aesthetics and visualizations
library(tidyverse)


# Import data
setwd("/Users/yuriibondar/Desktop/c")
hbookings <- read.csv("Lesson3_hotel_bookings.csv")
glimpse(hbookings)


# Making a bar chart
ggplot(data = hbookings) +
  geom_bar(
    mapping = aes(
      x = distribution_channel,
      fill = market_segment
    )
  ) +
  facet_grid(deposit_type~market_segment) +
  theme(axis.text.x = element_text(angle = 45))

ggplot(data = hbookings) +
  geom_tile(
    mapping = aes(
      x = distribution_channel,
      y = market_segment,
      alpha = stays_in_week_nights
    )
  )



#