


# 2022-10-17 Hands-On Activity: Annotating and saving visualizations
library(tidyverse)


# Load data
hotel_bookings <- read.csv("hotel_bookings.csv")
mindate <- min(hotel_bookings$arrival_date_year)
maxdate <- max(hotel_bookings$arrival_date_year)


# Annotate chart
ggplot(data = hotel_bookings) +
  geom_bar(
    mapping = aes(
      x = market_segment,
      fill = market_segment
    )
  ) +
  facet_wrap(~hotel) +
  labs(
    title = "Hotel Type vs Market Segment",
    caption = paste0("Data from: ", mindate, " to ", maxdate),
    x = "Market Segment",
    y = "Number of Bookings"
  ) +
  theme(axis.text.x = element_text(angle = 45))


# Saving a chart
ggsave("hotel_bookings_chart.png")



#