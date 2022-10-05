library(tidyverse)
setwd("/Users/yuriibondar/Desktop/c")
bookings_df <- read_csv("hotel_bookings.csv")

head(bookings_df)
str(bookings_df)
colnames(bookings_df)

new_df <- select(bookings_df, `adr`, adults)
mutate(new_df, total = `adr` / adults)

coffee_df <- read_csv("coffee.csv")
str(coffee_df)
coffe_order <- order(coffee_df, `Open`, decreasing = TURE)

