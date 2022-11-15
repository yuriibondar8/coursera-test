


# 2022-11-03 Capstone project -- Cyclistic

# Load packages for data cleaning
library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)
library(hydroTSM)


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

# Reamoving epty values and checking dates
trips <- trips_clean %>% drop_na()
glimpse(trips)
str(trips)
min(trips$started_at)
max(trips$started_at)

# Saving clean df on disk
write.csv(
  trips,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/trips.csv",
  row.names = FALSE
)


# Analyzing the data
# Creating summary df with important stats
trips_sumary <- trips %>%
  group_by(day_of_week, member_casual, rideable_type) %>%
  summarise(
    Tmean = mean(ride_length),
    Tmedian = median(ride_length),
    Nrides = n_distinct(ride_id)
  )
View(trips_sumary)

# Sorting summary by day of the week
trips_sumary$day_of_week <- ordered(
  trips_sumary$day_of_week, levels = c(
    "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
  )
)

write.csv(
  trips_sumary,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/trips_sumary.csv",
  row.names = FALSE
)

# Exploring data stats
mean(trips$ride_length)
median(trips$ride_length)
max(trips$ride_length)
min(trips$ride_length)

aggregate(
  trips$ride_length ~ trips$member_casual + trips$day_of_week,
  FUN = median
)

all_trips <- trips %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(
    number_of_rides = n_distinct(ride_id),
    average_duration = mean(ride_length)
  ) %>%
  arrange(member_casual, weekday)

# Creating data visualizations
# Number of riders by rider type
ggplot(data = all_trips) +
  geom_col(
    mapping = aes(
      x = weekday,
      y = number_of_rides,
      fill = member_casual
    ),
    position = "dodge"
  )

ggplot(data = trips_sumary) +
  geom_col(
    mapping = aes(
      x = day_of_week,
      y = Nrides,
      fill = member_casual
    ),
    position = "dodge"
  ) +
  facet_wrap(~rideable_type)

# Average duration
ggplot(data = all_trips) +
  geom_col(
    mapping = aes(
      x = weekday,
      y = average_duration,
      fill = member_casual
    ),
    position = "dodge"
  )

ggplot(data = trips_sumary) +
  geom_col(
    mapping = aes(
      x = day_of_week,
      y = Tmedian,
      fill = member_casual
    ),
    position = "dodge"
  ) +
  facet_wrap(~rideable_type)









trips_clean_wip <- trips_clean %>%
  drop_na() %>%
  mutate(
    day_of_week = wday(started_at, label = TRUE),
    start_date = format(as.POSIXct(started_at), format = "%Y-%m-%d"),
    start_time = format(as.POSIXct(started_at), format = "%H:%M:%S"),
    end_date = format(as.POSIXct(ended_at), format = "%Y-%m-%d"),
    end_time = format(as.POSIXct(ended_at), format = "%H:%M:%S"),
    trip_duration = format(as.duration(ended_at - started_at)),
    duration_sec = ended_at - started_at
  )
View(trips_clean_wip)
str(trips_clean_wip)
sapply(trips_clean_wip, class)

write.csv(
  trips_clean_wip,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/trips_clean_wip.csv", # nolint
  row.names = FALSE
)

trips_clean_wip$start_date <- as.Date(
  trips_clean_wip$start_date,
  format = "%Y-%m-%d"
)
trips_clean_wip$end_time <- hms(trips_clean_wip$end_time)
typeof(trips_clean_wip$duration_min)
typeof(trips_clean_wip$duration_sec)
typeof(trips_clean_wip$ride_length)



trips_clean_wip <- trips_clean_wip %>%
  mutate(
    season = time2season(start_date, out.fmt = "seasons"),
    month = month(started_at, label = TRUE),
    weekday = wday(started_at, label = TRUE),
    start_hour = hour(started_at),
    duration_min = as.integer(ride_length / 60)
  )

trips_clean_wip <- trips_clean_wip %>%
  filter(
    duration_min > 0
  )


write.csv(
  trips_clean_wip,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/trips_clean.csv", # nolint
  row.names = FALSE
)



# 5,094,974
# 5,013,662































#