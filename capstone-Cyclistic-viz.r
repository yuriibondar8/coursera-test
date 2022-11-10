


# 2022-11-10 Capstone project -- Cyclistic pt.2 analysis & visualization

# Load packages
library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)


# Load clean dataset
setwd("/Users/yuriibondar/Desktop/c/capstone - case 1/csv")
trips <- read_csv("trips_clean.csv")
View(trips)


# general
# distribution of riders, bike types

# Stats for members
riders_distribution <- trips %>%
  group_by(member_casual) %>%
  summarise(
    rider_trips = n_distinct(ride_id),
    rider_duration = sum(duration_min),
    rider_median_duration = median(duration_min),
    rider_avg_duration = mean(duration_min),
    rider_min_duration = min(duration_min),
    rider_max_duration = max(duration_min)
  )

write.csv(
  riders_distribution,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/riders_distribution.csv", # nolint
  row.names = FALSE
)

# Stats for types of bikes
bikes_distribution <- trips %>%
  group_by(rideable_type) %>%
  summarise(
    bike_trips = n_distinct(ride_id),
    bike_duration = sum(duration_min),
    bike_median_duration = median(duration_min),
    bike_avg_duration = mean(duration_min),
    bike_min_duration = min(duration_min),
    bike_max_duration = max(duration_min)
  )

write.csv(
  bikes_distribution,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/bikes_distribution.csv", # nolint
  row.names = FALSE
)

# Summary for members & bikes
riders_vs_bikes <- trips %>%
  group_by(member_casual, rideable_type) %>%
  summarise(
    trips = n_distinct(ride_id),
    duration = sum(duration_min),
    median_duration = median(duration_min),
    avg_duration = mean(duration_min),
    min_duration = min(duration_min),
    max_duration = max(duration_min)
  )

write.csv(
  riders_vs_bikes,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/riders_vs_bikes.csv", # nolint
  row.names = FALSE
)

ggplot(data = riders_vs_bikes) +
  geom_col(
    mapping = aes(
      x = member_casual,
      y = median_duration,
      fill = rideable_type
    ),
    position = "dodge"
  )


# logistics
#most popular stations by trips , then divide member / casual

# Lookup tabels for stations
start_station_lookup <- trips %>%
  select(
    start_station_name,
    start_station_id,
    start_lat,
    start_lng
  ) %>%
  distinct(start_station_name, .keep_all = TRUE)

end_station_lookup <- trips %>%
  select(
    end_station_name,
    end_station_id,
    end_lat,
    end_lng
  ) %>%
  distinct(end_station_name, .keep_all = TRUE)


# Start station total trips
start_station_rank <- trips %>%
  group_by(start_station_name) %>%
  summarise(
    trips = n_distinct(ride_id)
  ) %>%
  arrange(desc(trips))

# Start station member trips
start_station_member <- trips %>%
  filter(member_casual == "member") %>%
  group_by(start_station_name) %>%
  summarise(
    member_trips = n_distinct(ride_id)
  ) %>%
  arrange(desc(member_trips))

# Start station casual trips
start_station_casual <- trips %>%
  filter(member_casual == "casual") %>%
  group_by(start_station_name) %>%
  summarise(
    casual_trips = n_distinct(ride_id)
  ) %>%
  arrange(desc(casual_trips))

# Merge total; members; casual trips
start_rank_merge <- merge(
  start_station_rank,
  start_station_member,
  by = "start_station_name"
)
start_rank_merge <- merge(
  start_rank_merge,
  start_station_casual,
  by = "start_station_name"
)
start_rank_merge <- start_rank_merge %>%
  arrange(desc(trips))

# Merging trips table with start stations data
start_station_summary <- merge(
  start_rank_merge,
  start_station_lookup,
  by = "start_station_name"
)
start_station_summary <- start_station_summary %>% arrange(desc(trips))
View(start_station_summary)

remove(start_rank_merge)
remove(start_station_rank)
remove(start_station_member)
remove(start_station_casual)

write.csv(
  start_station_summary,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/start_station_summary.csv", # nolint
  row.names = FALSE
)


# End station total trips
end_station_rank <- trips %>%
  group_by(end_station_name) %>%
  summarise(
    trips = n_distinct(ride_id)
  ) %>%
  arrange(desc(trips))

# End station member trips
end_station_member <- trips %>%
  filter(member_casual == "member") %>%
  group_by(end_station_name) %>%
  summarise(
    member_trips = n_distinct(ride_id)
  ) %>%
  arrange(desc(member_trips))

# End station casual trips
end_station_casual <- trips %>%
  filter(member_casual == "casual") %>%
  group_by(end_station_name) %>%
  summarise(
    casual_trips = n_distinct(ride_id)
  ) %>%
  arrange(desc(casual_trips))

# Merge total; members; casual trips
end_rank_merge <- merge(
  end_station_rank,
  end_station_member,
  by = "end_station_name"
)
end_rank_merge <- merge(
  end_rank_merge,
  end_station_casual,
  by = "end_station_name"
)
end_rank_merge <- end_rank_merge %>%
  arrange(desc(trips))

# Merging trips table with start stations data
end_station_summary <- merge(
  end_rank_merge,
  end_station_lookup,
  by = "end_station_name"
)
end_station_summary <- end_station_summary %>% arrange(desc(trips))
View(end_station_summary)

remove(end_rank_merge)
remove(end_station_rank)
remove(end_station_member)
remove(end_station_casual)

write.csv(
  end_station_summary,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/end_station_summary.csv", # nolint
  row.names = FALSE
)


# demand
# months, weekdays, time of the day

# Monthly stats
# Monthly total
monthly_total <- trips %>%
  group_by(month) %>%
  summarise(total_trips = n_distinct(ride_id))

# Monthly members
monthly_summary_members <- trips %>%
  filter(member_casual == "member") %>%
  group_by(month) %>%
  summarise(
    member_trips = n_distinct(ride_id),
    med_member_duration = median(duration_min),
    avg_member_duration = mean(duration_min)
  )

# Monthly casual
monthly_summary_casual <- trips %>%
  filter(member_casual == "casual") %>%
  group_by(month) %>%
  summarise(
    casual_trips = n_distinct(ride_id),
    med_casual_duration = median(duration_min),
    avg_casual_duration = mean(duration_min)
  )

# Monthly summary
monthly_summary <- merge(
  monthly_total,
  monthly_summary_members,
  by = "month"
)
monthly_summary <- merge(
  monthly_summary,
  monthly_summary_casual,
  by = "month"
)
monthly_summary <- monthly_summary %>% arrange(month)
View(monthly_summary)

remove(monthly_total)
remove(monthly_summary_members)
remove(monthly_summary_casual)

write.csv(
  monthly_summary,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/monthly_summary.csv", # nolint
  row.names = FALSE
)


# Weekdays stats
# Weekdays total
weekdays_total <- trips %>%
  group_by(weekday) %>%
  summarise(total_trips = n_distinct(ride_id))

# Weekdays member
weekdays_members <- trips %>%
  filter(member_casual == "member") %>%
  group_by(weekday) %>%
  summarise(
    member_trips = n_distinct(ride_id),
    med_member_duration = median(duration_min),
    avg_member_duration = mean(duration_min)
  )

# Weekdays casual
weekdays_casual <- trips %>%
  filter(member_casual == "casual") %>%
  group_by(weekday) %>%
  summarise(
    casual_trips = n_distinct(ride_id),
    med_casual_duration = median(duration_min),
    avg_casual_duration = mean(duration_min)
  )

# Weekdays summary
weekdays_summary <- merge(
  weekdays_total,
  weekdays_members,
  by = "weekday"
)
weekdays_summary <- merge(
  weekdays_summary,
  weekdays_casual,
  by = "weekday"
)
weekdays_summary <- weekdays_summary %>% arrange(weekday)
View(weekdays_summary)

remove(weekdays_total)
remove(weekdays_members)
remove(weekdays_casual)

write.csv(
  weekdays_summary,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/weekdays_summary.csv", # nolint
  row.names = FALSE
)


# Hourly stats
hourly_total <- trips %>%
  group_by(start_hour) %>%
  summarise(total_trips = n_distinct(ride_id))

# Hourly members
hourly_members <- trips %>%
  filter(member_casual == "member") %>%
  group_by(start_hour) %>%
  summarise(
    member_trips = n_distinct(ride_id),
    med_member_duration = median(duration_min),
    avg_member_duration = mean(duration_min)
  )

# Hourly casual
hourly_casual <- trips %>%
  filter(member_casual == "casual") %>%
  group_by(start_hour) %>%
  summarise(
    casual_trips = n_distinct(ride_id),
    med_casual_duration = median(duration_min),
    avg_casual_duration = mean(duration_min)
  )

# Hourly summary
hourly_summary <- merge(
  hourly_total,
  hourly_members,
  by = "start_hour"
)
hourly_summary <- merge(
  hourly_summary,
  hourly_casual,
  by = "start_hour"
)
View(hourly_summary)

remove(hourly_total)
remove(hourly_members)
remove(hourly_casual)

write.csv(
  hourly_summary,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/hourly_summary.csv", # nolint
  row.names = FALSE
)



ggplot(data = hourly_summary) +
  geom_col(
    mapping = aes(
      x = start_hour,
      y = total_trips,
      fill = member_trips
    )
  )



#