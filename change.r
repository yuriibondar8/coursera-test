


# 2022-10-10 Lesson 3: Changing your data

library(tidyverse)
library(skimr)
library(janitor)

setwd("/Users/yuriibondar/Desktop/c")


# Loading hotel_booking and applying basic functions
hotel_bookings <- read_csv("Lesson3_hotel_bookings.csv")
head(hotel_bookings)
str(hotel_bookings)
glimpse(hotel_bookings)
colnames(hotel_bookings)

arrange(hotel_bookings, lead_time) # arrange() ASC by default
arrange(hotel_bookings, desc(lead_time)) # arrange() DESC
max(hotel_bookings$lead_time)
min(hotel_bookings$lead_time)
mean(hotel_bookings$lead_time)


# Creating new dataset with sorting by lead time
hotel_bookings_v2 <- arrange(hotel_bookings, desc(lead_time))
head(hotel_bookings_v2)
mean(hotel_bookings_v2$lead_time)


# Creating new dataset with filtering by hotel type
hotel_bookings_city <- filter(
 hotel_bookings,
 hotel_bookings$hotel == "City Hotel"
)
head(hotel_bookings_city)
mean(hotel_bookings_city$lead_time)


# Comparing averages in lead time from different datasets
mean(hotel_bookings$lead_time)
mean(hotel_bookings_v2$lead_time)
mean(hotel_bookings_city$lead_time)


# Creating summary dataset
hotel_summary <- hotel_bookings %>%
 group_by(hotel) %>%
 summarise(
  avg_lead_time = mean(lead_time),
  min_lead_time = min(lead_time),
  max_lead_time = max(lead_time)
 )
head(hotel_summary)



#