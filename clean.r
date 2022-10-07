


# 2022-10-07 Lesson 3: Cleaning data

library(tidyverse)
library(skimr)
library(janitor)

bookings_dframe <- read_csv("hotel_bookings-1.csv")
head(bookings_dframe)
str(bookings_dframe)
glimpse(bookings_dframe)
colnames(bookings_dframe)

skim_without_charts(bookings_dframe)

trimmed_bookings <- bookings_dframe %>%
 select(
  "hotel",
  "is_canceled",
  "lead_time",
  "arrival_date_day_of_month",
  "arrival_date_month",
  "arrival_date_year",
  "adults",
  "children",
  "babies"
 ) %>%
 mutate(guests = adults + children + babies) %>%
 # transmute
 unite(
  "arrival_month_year",
  c("arrival_date_year", "arrival_date_month", "arrival_date_day_of_month"),
  sep = "-"
 ) %>%
 rename(
  "hotel_type" = "hotel",
  "date_arrival" = "arrival_month_year"
 )

head(trimmed_bookings)
View(trimmed_bookings)



summary_bookings <- bookings_dframe %>%
 select(
  "is_canceled",
  "lead_time"
 ) %>%
 summarise(
  number_cancelled = sum(is_canceled == 1),
  average_lead_time = mean(lead_time)
 )

head(summary_bookings)

#