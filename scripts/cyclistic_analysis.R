cyclistic_analysis.R
library(tidyverse)
library(lubridate)
library(janitor)
q1_2019 <- read_csv("Data_raw/Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Data_raw/Divvy_Trips_2020_Q1.csv")
q1_2019 <- q1_2019 %>% clean_names()
q1_2020 <- q1_2020 %>% clean_names()
names(q1_2019)
names(q1_2020)
q1_2019 <- q1_2019 %>%
  rename(
    ride_id = trip_id,
    started_at = start_time,
    ended_at = end_time,
    start_station_name = from_station_name,
    start_station_id = from_station_id,
    end_station_name = to_station_name,
    end_station_id = to_station_id
  ) %>%
  mutate(
    rideable_type = "docked_bike",
    member_casual = if_else(usertype == "Subscriber", "member", "casual"),
    start_lat = NA,
    start_lng = NA,
    end_lat = NA,
    end_lng = NA
  ) %>%
  select(
    ride_id,
    rideable_type,
    started_at,
    ended_at,
    start_station_name,
    start_station_id,
    end_station_name,
    end_station_id,
    start_lat,
    start_lng,
    end_lat,
    end_lng,
    member_casual
  )

q1_2020 <- q1_2020 %>%
  select(
    ride_id,
    rideable_type,
    started_at,
    ended_at,
    start_station_name,
    start_station_id,
    end_station_name,
    end_station_id,
    start_lat,
    start_lng,
    end_lat,
    end_lng,
    member_casual
  )
names(q1_2019)
names(q1_2020)
all_trips <- bind_rows(q1_2019, q1_2020)

glimpse(all_trips)
q1_2019$ride_id <- as.character(q1_2019$ride_id)
q1_2020$ride_id <- as.character(q1_2020$ride_id)
# Fix ride_id type mismatch
q1_2019$ride_id <- as.character(q1_2019$ride_id)
q1_2020$ride_id <- as.character(q1_2020$ride_id)

# Combine datasets
all_trips <- bind_rows(q1_2019, q1_2020)

glimpse(all_trips)

# Create time-based and duration fields
all_trips <- all_trips %>%
  mutate(
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins")),
    day_of_week = wday(started_at, label = TRUE),   # Sun, Mon, ...
    month       = month(started_at, label = TRUE),  # Jan, Feb, ...
    hour        = hour(started_at)                  # 0–23
  )

# Quick check
summary(all_trips$ride_length)

# Remove invalid ride lengths
all_trips_clean <- all_trips %>%
  filter(
    !is.na(ride_length),
    ride_length > 0,
    ride_length < 1440    # less than 24 hours
  )

# Check cleaned data
glimpse(all_trips_clean)
summary(all_trips_clean$ride_length)

write_csv(all_trips_clean, "data_clean/all_trips_clean.csv")

# Compare average ride length between members and casual riders
all_trips_clean %>%
  group_by(member_casual) %>%
  summarize(
    mean_ride_length = mean(ride_length),
    median_ride_length = median(ride_length),
    max_ride_length = max(ride_length)
  )

all_trips_clean %>%
  group_by(member_casual, day_of_week) %>%
  summarize(num_rides = n(),
            avg_duration = mean(ride_length)) %>%
  arrange(member_casual, day_of_week)

all_trips_clean %>%
  group_by(member_casual, month) %>%
  summarize(num_rides = n()) %>%
  arrange(member_casual, month)

all_trips_clean %>%
  group_by(member_casual, hour) %>%
  summarize(num_rides = n())

library(ggplot2)
# Average ride length chart (Members vs Casual)
avg_ride_length <- all_trips_clean %>%
  group_by(member_casual) %>%
  summarize(mean_length = mean(ride_length))

ggplot(avg_ride_length, aes(x = member_casual, y = mean_length, fill = member_casual)) +
  geom_col() +
  labs(
    title = "Average Ride Length: Members vs Casual Riders",
    x = "Rider Type",
    y = "Average Ride Length (minutes)"
  ) +
  scale_fill_manual(values = c("casual" = "steelblue", "member" = "orange")) +
  theme_minimal()

ggsave("outputs/avg_ride_length.png", width = 8, height = 5)

# Number of rides by day of week (Members vs Casual)
rides_by_day <- all_trips_clean %>%
  group_by(member_casual, day_of_week) %>%
  summarize(num_rides = n(), .groups = "drop")

ggplot(rides_by_day, aes(x = day_of_week, y = num_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Number of Rides by Day of Week",
    x = "Day of the Week",
    y = "Number of Rides",
    fill = "Rider Type"
  ) +
  scale_fill_manual(values = c("casual" = "steelblue", "member" = "orange")) +
  theme_minimal()

ggsave("outputs/rides_by_day.png", width = 10, height = 5)

# Number of rides by month (Members vs Casual)
rides_by_month <- all_trips_clean %>%
  group_by(member_casual, month) %>%
  summarize(num_rides = n(), .groups = "drop")

ggplot(rides_by_month, aes(x = month, y = num_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(
    title = "Number of Rides by Month (Q1 Only)",
    x = "Month",
    y = "Number of Rides",
    fill = "Rider Type"
  ) +
  scale_fill_manual(values = c("casual" = "steelblue", "member" = "orange")) +
  theme_minimal()

ggsave("outputs/rides_by_month.png", width = 10, height = 5)

# Number of rides by hour of day (Members vs Casual)
rides_by_hour <- all_trips_clean %>%
  group_by(member_casual, hour) %>%
  summarize(num_rides = n(), .groups = "drop")

ggplot(rides_by_hour, aes(x = hour, y = num_rides, color = member_casual)) +
  geom_line(size = 1.2) +
  labs(
    title = "Number of Rides by Hour of Day",
    x = "Hour of Day (0–23)",
    y = "Number of Rides",
    color = "Rider Type"
  ) +
  scale_color_manual(values = c("casual" = "steelblue", "member" = "orange")) +
  theme_minimal()

ggsave("outputs/rides_by_hour.png", width = 10, height = 5)



