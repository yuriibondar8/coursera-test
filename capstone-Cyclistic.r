


# 2022-11-03 Capstone project -- Cyclistic

# Load packages for data cleaning
library(tidyverse)
library(skimr)
library(janitor)


# Preapring the data
# Importing 12 datasets
setwd("/Users/yuriibondar/Desktop/c/capstone - case 1/csv")
trips_2021_09 <- read_csv("2021-09-divvy-tripdata.csv")
trips_2021_10 <- read_csv("2021-10-divvy-tripdata.csv")
trips_2021_11 <- read_csv("2021-11-divvy-tripdata.csv")
trips_2021_12 <- read_csv("2021-12-divvy-tripdata.csv")
trips_2022_01 <- read_csv("2022-01-divvy-tripdata.csv")
trips_2022_02 <- read_csv("2022-02-divvy-tripdata.csv")
trips_2022_03 <- read_csv("2022-03-divvy-tripdata.csv")
trips_2022_04 <- read_csv("2022-04-divvy-tripdata.csv")
trips_2022_05 <- read_csv("2022-05-divvy-tripdata.csv")
trips_2022_06 <- read_csv("2022-06-divvy-tripdata.csv")
trips_2022_07 <- read_csv("2022-07-divvy-tripdata.csv")
trips_2022_08 <- read_csv("2022-08-divvy-tripdata.csv")
trips_2022_09 <- read_csv("2022-09-divvy-tripdata.csv")

# Exploring of the imported data
str(trips_2021_09)
skim_without_charts(trips_2021_09)
glimpse(trips_2021_09)

# Comparing data types in columns across 12 sets
compare_df_cols(
 trips_2021_09,
 trips_2021_10,
 trips_2021_11,
 trips_2021_12,
 trips_2022_01,
 trips_2022_02,
 trips_2022_03,
 trips_2022_04,
 trips_2022_05,
 trips_2022_06,
 trips_2022_07,
 trips_2022_08,
 trips_2022_09
)

# Merging 12 sets into one
trips_df <- bind_rows(
 trips_2021_09,
 trips_2021_10,
 trips_2021_11,
 trips_2021_12,
 trips_2022_01,
 trips_2022_02,
 trips_2022_03,
 trips_2022_04,
 trips_2022_05,
 trips_2022_06,
 trips_2022_07,
 trips_2022_08,
 trips_2022_09
)
glimpse(trips_df)


# Processing the data
# Adding columns to original trips_df
# Removing na and negative values
trips_clean <- trips_df %>%
  mutate(ride_length = ended_at - started_at) %>%
  mutate(day_of_week = weekdays(started_at)) %>%
  subset(ride_length > 0)
str(trips_clean)

trips <- trips_clean %>% drop_na()






#