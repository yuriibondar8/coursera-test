


#####################################################################
# 2022-11-17 Capstone project -- Cyclistic
#####################################################################


# Load packages for data cleaning
library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(lubridate)
library(hydroTSM)


#####################################################################
# Preapring the data

# Importing monthly records from prevoius 12 month [Sep 2021/2022]
# Checking data integrity across imported sets
# Merging all sets into one
#####################################################################

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

glimpse(trips_2021_09)

#####################################################################
# Checking data integrity before merging data into one set

compare_df_cols_same(
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
  trips_2022_08
)

#####################################################################
# Merging monthly records into one large set

trips_wip <- bind_rows(
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
  trips_2022_08
)
glimpse(trips_wip)


#####################################################################
# Processing and cleaning data

# Removing records with empty values and negative ride length
# Adding columns with metrics for further analysis
#####################################################################

trips_clean <- trips_wip %>%
  drop_na() %>%
  mutate(
    path_id = paste(start_station_id, end_station_id, sep = "$"),
    season = time2season(started_at, out.fmt = "seasons"),
    month = month(started_at, label = TRUE),
    weekday = wday(started_at, label = TRUE),
    start_hour = hour(started_at),
    ride_length_s = ended_at - started_at
  ) %>%
  mutate(ride_length_m = as.integer(ride_length_s / 60)) %>%
  subset(ride_length_m > 0)
glimpse(trips_clean)

View(trips_clean)

# 5,883,043 records before cleaning
# 4,488,333 records after cleaning

write.csv(
  trips_clean,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/project files/trips_clean.csv", # nolint
  row.names = FALSE
)

remove(
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
  trips_wip
)


#####################################################################
# Extracting subsets of data for further analysis

# General top level metrics
# Seasonal changes
# Weekday, hourly ride stats
# Top destinations
#####################################################################

#####################################################################
# General top level metrics [number of trips, average length, bike demand]
summary_users <- trips_clean %>%
  group_by(member_casual) %>%
  summarise(
    total_trips = n_distinct(ride_id),
    avg_trip_length = mean(ride_length_m)
  )
glimpse(summary_users)

summary_bikes <- trips_clean %>%
  group_by(member_casual, rideable_type) %>%
  summarise(
    total_trips = n_distinct(ride_id),
    avg_trip_length = mean(ride_length_m)
  )
glimpse(summary_bikes)

write.csv(
  summary_users,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/project files/summary_users.csv", # nolint
  row.names = FALSE
)

write.csv(
  summary_bikes,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/project files/summary_bikes.csv", # nolint
  row.names = FALSE
)

#####################################################################
# Seasonal changes [number of trips, avg length; grupped by season, user type]
monthly_summary <- trips_clean %>%
  group_by(season, month, member_casual) %>%
  summarise(
    total_trips = n_distinct(ride_id),
    avg_trip_length = mean(ride_length_m)
  )
monthly_summary

write.csv(
  monthly_summary,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/project files/monthly_summary.csv", # nolint
  row.names = FALSE
)

#####################################################################
# Weekday trends [number of trips, avg time grouped by day; hour]
weekday_summary <- trips_clean %>%
  group_by(weekday, start_hour, member_casual) %>%
  summarise(
    total_trips = n_distinct(ride_id),
    avg_trip_length = mean(ride_length_m)
  )
View(weekday_summary)

write.csv(
  weekday_summary,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/project files/weekday_summary.csv", # nolint
  row.names = FALSE
)

#####################################################################
# Top destinations [start/end stations by number of trips]

# Create stations list to use as lookup table
station_lookup_start <- trips_clean %>%
  select(
    start_station_id,
    start_station_name,
    start_lat,
    start_lng
  ) %>%
  rename(
    "station_id" = start_station_id,
    "station_name" = start_station_name,
    "lat" = start_lat,
    "lng" = start_lng
  ) %>%
  distinct(station_id, .keep_all = TRUE)

station_lookup_end <- trips_clean %>%
  select(
    end_station_id,
    end_station_name,
    end_lat,
    end_lng
  ) %>%
  rename(
    "station_id" = end_station_id,
    "station_name" = end_station_name,
    "lat" = end_lat,
    "lng" = end_lng
  ) %>%
  distinct(station_id, .keep_all = TRUE)

compare_df_cols_same(station_lookup_start, station_lookup_end)

stations <- bind_rows(station_lookup_start, station_lookup_end)
stations <- stations %>% distinct(station_id, .keep_all = TRUE)
glimpse(stations)

remove(station_lookup_start, station_lookup_end)

# Summary of most used path between stations by user group
path_summary_wip <- trips_clean %>%
  group_by(
    path_id,
    member_casual,
    start_station_id,
    end_station_id
  ) %>%
  summarise(total_trips = n_distinct(ride_id) / 2) %>%
  arrange(desc(total_trips)) %>%
  pivot_longer(
    3:4, names_to = "start_end", values_to = "station_id"
  ) %>%
  head(400)
glimpse(path_summary_wip)

path_summary <- path_summary_wip %>% left_join(stations, by = "station_id")
View(path_summary)

remove(path_summary_wip)

write.csv(
  path_summary,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/project files/path_summary.csv", # nolint
  row.names = FALSE
)



#