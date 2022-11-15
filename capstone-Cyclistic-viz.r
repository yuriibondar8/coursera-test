


# 2022-11-10 Capstone project -- Cyclistic pt.2 analysis & visualization

# Load packages
library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)
library(hydroTSM)

# Load clean dataset
setwd("/Users/yuriibondar/Desktop/c/capstone - case 1/csv")
trips <- read_csv("trips_clean.csv")
View(trips)


#####################################################################
# General
# distribution of riders, bike types
#####################################################################

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


#####################################################################
# logistics
#most popular stations by trips , then divide member / casual
#####################################################################

# Lookup tabels for stations
start_station_lookup <- trips %>%
  select(
    start_station_name,
    start_station_id,
    start_lat,
    start_lng
  ) %>%
  distinct(start_station_id, .keep_all = TRUE)

end_station_lookup <- trips %>%
  select(
    end_station_name,
    end_station_id,
    end_lat,
    end_lng
  ) %>%
  distinct(end_station_id, .keep_all = TRUE)


# Start station total trips
start_station_rank <- trips %>%
  group_by(start_station_id) %>%
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
  group_by(end_station_id) %>%
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


#####################################################################
# demand
# months, weekdays, time of the day
#####################################################################

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



###########################################

daily_hourly_m <- trips %>%
  filter(member_casual == "member") %>%
  group_by(start_hour, day_of_week) %>%
    summarise(
    member_trips = n_distinct(ride_id),
    med_member_duration = median(duration_min),
    avg_member_duration = mean(duration_min)
  )

write.csv(
  daily_hourly_m,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/daily_hourly_m.csv", # nolint
  row.names = FALSE
)


daily_hourly_c <- trips %>%
  filter(member_casual == "casual") %>%
  group_by(start_hour, day_of_week) %>%
    summarise(
    casual_trips = n_distinct(ride_id),
    med_casual_duration = median(duration_min),
    avg_casual_duration = mean(duration_min)
  )

write.csv(
  daily_hourly_c,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/daily_hourly_c.csv", # nolint
  row.names = FALSE
)


daily_hourly_summary <- merge(
  daily_hourly_m,
  daily_hourly_c,
  by = "start_hour"
)

daily_hourly_total <- trips %>%
  group_by(start_hour, day_of_week) %>%
    summarise(
    total_trips = n_distinct(ride_id),
    med_total_duration = median(duration_min),
    avg_total_duration = mean(duration_min)
  )


seasonal <- trips %>%
  group_by(
    season,
    month,
    member_casual
  ) %>%
  summarise(
    total_trips = n_distinct(ride_id),
    avg_duration = mean(duration_min),
    med_duration = median(duration_min)
  )


hourly <- trips %>%
  group_by(
    weekday,
    start_hour,
    member_casual
  ) %>%
  summarise(
    total_trips = n_distinct(ride_id),
    avg_duration = mean(duration_min),
    med_duration = median(duration_min)
  )

#   - start/end station id / name / coordinates / n trips for each user g
routes <- trips %>%
  group_by(
    start_station_name,
    end_station_name,
    member_casual
  ) %>%
  summarise(
    total_trips = n_distinct(ride_id)
  ) %>%
  arrange(desc(total_trips)) %>%
  head(15)

routes_member <- trips %>%
  filter(member_casual == "member") %>%
  group_by(
    start_station_name,
    end_station_name,
    member_casual
  ) %>%
  summarise(
    total_trips = n_distinct(ride_id)
  ) %>%
  arrange(desc(total_trips)) %>%
  head(50)

routes_casual <- trips %>%
  filter(member_casual == "casual") %>%
  group_by(
    start_station_name,
    end_station_name,
    member_casual
  ) %>%
  summarise(
    total_trips = n_distinct(ride_id)
  ) %>%
  arrange(desc(total_trips)) %>%
  head(50)

destinations <- rbind(routes_member, routes_casual)
destinations_summary <- merge(
  destinations,
  start_station_lookup,
  by = "start_station_name"
)
destinations_summary <- merge(
  destinations_summary,
  end_station_lookup,
  by = "end_station_name"
)


destinations_temp_1 <- destinations_summary %>%
  mutate(
    path_id = paste(start_station_id, end_station_id ,sep = "-")
  )

destinations_temp_summary <- destinations_temp_1 %>%
  pivot_longer(
    1:2, names_to = "start_end", values_to = "station_name"
  )

destinations_temp_summary_2 <- destinations_temp_summary %>%
  select(
    station_name,
    start_end,
    member_casual,
    total_trips,
    path_id
  )


destinations_temp_summary_3 <- merge(
  destinations_temp_summary_2,
  stations,
  by = "station_name"
)

write.csv(
  destinations_temp_summary_3,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/destinations_temp_summary_3.csv", # nolint
  row.names = FALSE
)



# Stations data lookup
start <- start_station_lookup %>%
  rename(
    "station_name" = "start_station_name",
    "station_id" = "start_station_id",
    "lat" = "start_lat",
    "lon" = "start_lng"
  )

end <- end_station_lookup %>%
  rename(
    "station_name" = "end_station_name",
    "station_id" = "end_station_id",
    "lat" = "end_lat",
    "lon" = "end_lng"
  )

stations <- rbind(start, end)


destinations_temp <- destinations %>%
  pivot_longer(
    1:2, names_to = "start_end", values_to = "station_name"
  )

destinations_temp_2 <- destinations_temp %>%
  select(
    station_name,
    start_end,
    member_casual,
    total_trips,
    path_id
  )

destinations_temp_3 <- merge(
  destinations_temp_2,
  stations,
  by = "station_name"
)

write.csv(
  destinations_temp_3,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/destinations_temp_3.csv", # nolint
  row.names = FALSE
)


write.csv(
  destinations,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/destinations.csv", # nolint
  row.names = FALSE
)

start_member <- trips %>%
  filter(member_casual == "member") %>%
  group_by(
    start_station_name,
    member_casual
  ) %>%
  summarise(
    total_trips = n_distinct(ride_id)
  ) %>%
  arrange(desc(total_trips)) %>%
  head(15)

start_casual <- trips %>%
  filter(member_casual == "casual") %>%
  group_by(
    start_station_name,
    member_casual
  ) %>%
  summarise(
    total_trips = n_distinct(ride_id)
  ) %>%
  arrange(desc(total_trips)) %>%
  head(15)

start_top_30 <- rbind(start_member, start_casual)
start_top_30 <- start_top_30 %>%
  rename(
    "station_name" = "start_station_name"
  )

start_top <- merge(
  start_top_30,
  stations,
  by = "station_name"
)

start_top <- start_top %>%
  distinct(total_trips, .keep_all = TRUE)

write.csv(
  start_top,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/start_top.csv", # nolint
  row.names = FALSE
)


end_member <- trips %>%
  filter(member_casual == "member") %>%
  group_by(
    end_station_name,
    member_casual
  ) %>%
  summarise(
    total_trips = n_distinct(ride_id)
  ) %>%
  arrange(desc(total_trips)) %>%
  head(15)

end_casual <- trips %>%
  filter(member_casual == "casual") %>%
  group_by(
    end_station_name,
    member_casual
  ) %>%
  summarise(
    total_trips = n_distinct(ride_id)
  ) %>%
  arrange(desc(total_trips)) %>%
  head(15)

end_top_30 <- rbind(end_member, end_casual)
end_top_30 <- end_top_30 %>%
  rename(
    "station_name" = "end_station_name"
  )

end_top <- merge(
  end_top_30,
  stations,
  by = "station_name"
)

end_top <- end_top %>%
  distinct(total_trips, .keep_all = TRUE)

write.csv(
  end_top,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/end_top.csv", # nolint
  row.names = FALSE
)



top_stations_member <- rbind(top_stations_member_s, top_stations_member_e)

top_stations_member_s <- trips %>%
  filter(member_casual == "member") %>%
  group_by(start_station_name, member_casual) %>%
  summarise(trips = n_distinct(ride_id)) %>%
  distinct(start_station_name, .keep_all = TRUE) %>%
  arrange(desc(trips)) %>%
  rename("station_name" = "start_station_name") %>%
  head(30)

top_stations_member_e <- trips %>%
  filter(member_casual == "member") %>%
  group_by(end_station_name, member_casual) %>%
  summarise(trips = n_distinct(ride_id)) %>%
  distinct(end_station_name, .keep_all = TRUE) %>%
  arrange(desc(trips)) %>%
  rename("station_name" = "end_station_name") %>%
  head(30)


top_stations_casual <- rbind(top_stations_casual_s, top_stations_casual_e)

top_stations_casual_s <- trips %>%
  filter(member_casual == "casual") %>%
  group_by(start_station_name, member_casual) %>%
  summarise(trips = n_distinct(ride_id)) %>%
  distinct(start_station_name, .keep_all = TRUE) %>%
  arrange(desc(trips)) %>%
  rename("station_name" = "start_station_name") %>%
  head(30)

top_stations_casual_e <- trips %>%
  filter(member_casual == "casual") %>%
  group_by(end_station_name, member_casual) %>%
  summarise(trips = n_distinct(ride_id)) %>%
  distinct(end_station_name, .keep_all = TRUE) %>%
  arrange(desc(trips)) %>%
  rename("station_name" = "end_station_name") %>%
  head(30)

top_stations <- rbind(top_stations_member, top_stations_casual)

top_s <- merge(
  top_stations,
  stations,
  by = "station_name"
)
top_s <- top_s %>%
  distinct(trips, .keep_all = TRUE)

write.csv(
  top_s,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/top_s.csv", # nolint
  row.names = FALSE
)

s_station <- start_station_rank %>%
  rename("statiton_id" = "start_station_id")
e_station <- end_station_rank %>%
  rename("statiton_id" = "end_station_id")

trips_path_id <- trips %>%
  mutate(
    path_id = paste(start_station_id, end_station_id, sep = "++")
  ) %>%
  group_by(
    path_id,
    start_station_id,
    end_station_id,
    member_casual
  ) %>%
  summarise(trips = n_distinct(ride_id)) %>%
  arrange(desc(trips))

trips_path_id_2 <- trips_path_id %>%
  pivot_longer(2:3, names_to = "station", values_to = "station_id")

trips_path_id_3 <- merge(
  trips_path_id_2,
  stations,
  by = "station_id"
)

trips_path_id_3 <- trips_path_id_3 %>%
  group_by(path_id) %>%
  arrange(desc(trips))

path_m <- trips_path_id_3 %>%
  filter(member_casual == "member") %>%
  head(500)

path_c <- trips_path_id_3 %>%
  filter(member_casual == "casual") %>%
  head(500)

path_c_m <- rbind(path_m, path_c)


write.csv(
  path_c_m,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/path_c_m.csv", # nolint
  row.names = FALSE
)









tsm_s <- trips %>%
  filter(member_casual == "member") %>%
  group_by(start_station_id, member_casual) %>%
  summarise(trips = n_distinct(ride_id)) %>%
  distinct(start_station_id, .keep_all = TRUE) %>%
  arrange(desc(trips)) %>%
  rename("station_id" = "start_station_id") %>%
  head(100)

tsm_e <- trips %>%
  filter(member_casual == "member") %>%
  group_by(end_station_id, member_casual) %>%
  summarise(trips = n_distinct(ride_id)) %>%
  distinct(end_station_id, .keep_all = TRUE) %>%
  arrange(desc(trips)) %>%
  rename("station_id" = "end_station_id") %>%
  head(100)

tsm <- rbind(tsm_s, tsm_e)


tsc_s <- trips %>%
  filter(member_casual == "casual") %>%
  group_by(start_station_id, member_casual) %>%
  summarise(trips = n_distinct(ride_id)) %>%
  distinct(start_station_id, .keep_all = TRUE) %>%
  arrange(desc(trips)) %>%
  rename("station_id" = "start_station_id") %>%
  head(100)

tsc_e <- trips %>%
  filter(member_casual == "casual") %>%
  group_by(end_station_id, member_casual) %>%
  summarise(trips = n_distinct(ride_id)) %>%
  distinct(end_station_id, .keep_all = TRUE) %>%
  arrange(desc(trips)) %>%
  rename("station_id" = "end_station_id") %>%
  head(100)

tsc <- rbind(tsc_s, tsc_e)




ts <- rbind(tsm, tsc)

ts_summary <- merge(
  ts,
  stations,
  by = "station_id"
)



ts_summary <- ts_summary %>%
  distinct(trips, .keep_all = TRUE)

write.csv(
  ts_summary,
  file = "/Users/yuriibondar/Desktop/c/capstone - case 1/csv/ts_summary.csv", # nolint
  row.names = FALSE
)







































#