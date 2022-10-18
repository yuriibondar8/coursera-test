


# 2022-10-17 Hands-On Activity: Filters and plots
library(tidyverse)
library(dplyr)


# Load data
hotel_bookings <- read.csv("hotel_bookings.csv")


# Build charts
ggplot(data = hotel_bookings) +
 geom_point(
   mapping = aes(
     x = lead_time,
     y = children
   )
 )

ggplot(data = hotel_bookings) +
  geom_bar(
    mapping = aes(
      x = market_segment,
      fill = market_segment
    )
  ) +
  facet_wrap(~hotel)


# Filtering data
# 1st method
online_ta_city_hotels <- filter(
  hotel_bookings,
  (hotel == "City Hotel" & market_segment == "Online TA")
)
# 2nd method
online_ta_city_hotels_v2 <- hotel_bookings %>%
  filter(hotel == "City Hotel") %>%
  filter(market_segment == "Online TA") %>%
  drop_na()
# Both outputs are identical
View(online_ta_city_hotels)
View(online_ta_city_hotels_v2)

ggplot(data = online_ta_city_hotels_v2) +
  geom_point(
    mapping = aes(
      x = lead_time,
      y = children
    )
  )



#