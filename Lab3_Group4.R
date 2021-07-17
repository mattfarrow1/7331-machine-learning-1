
# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(arules)
library(arulesViz)
library(gt)

# Load Data ---------------------------------------------------------------

# Read in the data directly
airlines <- read_csv(here("Data", "airlines.csv"))
airports <- read_csv(here("Data", "airports.csv"))
df       <- read_csv(here("Data", "flights.csv"))

# Read in the data directly from GitHub
# airlines <- read_csv('https://raw.githubusercontent.com/mattfarrow1/7331-machine-learning-1/main/Data/airlines.csv')
# airports <- read_csv('https://raw.githubusercontent.com/mattfarrow1/7331-machine-learning-1/main/Data/airports.csv')
# df       <- read_csv('https://media.githubusercontent.com/media/mattfarrow1/7331-machine-learning-1/main/Data/flights.csv')

# Clean up column names
airlines <- airlines %>% 
  clean_names() %>% 
  rename(airline_code = iata_code)
airports <- clean_names(airports)
df <- df %>% 
  clean_names() %>% 
  rename(airline_code = airline)

# Merge data together
df <- left_join(df, airlines)

# Subset to DFW Area
df <- df %>% filter(origin_airport == "DFW" | origin_airport == "DAL")

# Drop cancelled flights
# df <- df %>% filter(cancelled == 0)

# Create New Variables ----------------------------------------------------

# Convert times into buckets for morning, afternoon, and evening as most models cannot handle timestamps.
time_labels = c('overnight', 'morning', 'afternoon', 'evening')
time_bins   = c(0, 559, 1159, 1759, 2359)

df$scheduled_departure_time <- cut(as.numeric(df$scheduled_departure),
                                   breaks = time_bins,
                                   labels = time_labels)

df$actual_departure_time    <- cut(as.numeric(df$departure_time),
                                   breaks = time_bins,
                                   labels = time_labels)

df$scheduled_arrival_time   <- cut(as.numeric(df$scheduled_arrival),
                                   breaks = time_bins,
                                   labels = time_labels)

df$actual_arrival_time <-   cut(as.numeric(df$arrival_time),
                                breaks = time_bins,
                                labels = time_labels)

# Bucket Flight Distance
distance_labels <- c('Short', 'Medium', 'Long')
distance_bins   <- c(1, 100, 1000, Inf)

df$distance_bucket <- cut(df$distance,
                          breaks = distance_bins,
                          labels = distance_labels,
                          include.lowest = TRUE)

# Create a new column where the arrival_delay > 0 means it's delayed(=1) and if <= 0 it's not delayed(=0)
df <- df %>% 
  mutate(delayed = if_else(arrival_delay > 0, 1, 0))

# Look at our data with the buckets
head(df)

# Process Dates & Times ---------------------------------------------------

# Combine the Year, Month & Day columns into a single flight date
df$flight_date <- as.Date(with(df, paste(df$year, df$month, df$day, sep="-"), "%Y-%m-%d"))

# Define the scheduled departure and arrival datetimes
df$scheduled_departure_dt <-
  parse_date_time(paste(
    df$year,
    df$month,
    df$day,
    df$scheduled_departure,
    sep = "-"
  ),
  "%Y-%m-%d-%H%M")

df$scheduled_arrival_dt <-
  parse_date_time(paste(
    df$year,
    df$month,
    df$day,
    df$scheduled_arrival,
    sep = "-"
  ),
  "%Y-%m-%d-%H%M")

# Define the time columns
df$scheduled_departure <- parse_date_time(df$scheduled_departure, "%H%M")
df$departure_time      <- parse_date_time(df$departure_time, "%H%M")
df$scheduled_arrival   <- parse_date_time(df$scheduled_arrival, "%H%M")
df$arrival_time        <- parse_date_time(df$arrival_time, "%H%M")
df$wheels_on           <- parse_date_time(df$wheels_on, "%H%M")
df$wheels_off          <- parse_date_time(df$wheels_off, "%H%M")

# Append Dallas-Area Weather ----------------------------------------------

# Read in the data
weather <- read_csv(here("Data", "dfw_weather.csv"))

# Create a datetime column, rounded to the nearest 30 minutes
weather$date_time <- readr::parse_datetime(weather$dt_iso,
                                           format = "%Y-%m-%d %H:%M:%S %z %Z")

# Drop unnecessary columns
weather <- weather %>%
  select(
    -c(
      dt,
      dt_iso,
      timezone,
      city_name,
      lat,
      lon,
      feels_like,
      temp_min,
      temp_max,
      sea_level,
      grnd_level,
      weather_icon,
      weather_description
    )
  )

# Filter weather to only 2015
weather <- weather %>% 
  filter(year(date_time) == 2015)

# Round the scheduled departure datetime to the nearest hour
df$scheduled_departure_dt <- round_date(df$scheduled_departure_dt, "1 hour")

# Join df & weather data
df <- left_join(df, weather, by = c("scheduled_departure_dt" = "date_time"))

# Missing Values ----------------------------------------------------------

# Remove non-critical columns WHEELS_ON and WHEELS_OFF
df <- df %>% select(-c(wheels_on,
                       wheels_off))

# Missing value check
tibble(variable = names(colSums(is.na(df))),
       missing = colSums(is.na(df))) %>%
  gt()

# # Fill missing values with 'N' for 'N/A' and missing departure time to 0
# df <- df %>% replace_na(list(ACTUAL_DEPARTURE_TIME = 'N', 
#                        ACTUAL_ARRIVAL_TIME= 'N',
#                        CANCELLATION_REASON = 'N',
#                        DEPARTURE_TIME = 0))

# Convert departure_time to a binary value - departed
df <- df %>% 
  rename(departed = departure_time) %>% 
  mutate(departed = if_else(!is.na(departed), 1, 0)) 

# Convert arrival_time to a binary value - arrived
df <- df %>% 
  rename(arrived = arrival_time) %>% 
  mutate(arrived = if_else(!is.na(arrived), 1, 0)) 

# Fill missing values with 0
df <- df %>% replace_na(list(air_system_delay = 0, 
                             security_delay = 0,
                             airline_delay = 0,
                             late_aircraft_delay = 0,
                             weather_delay = 0,
                             rain_1h = 0,
                             rain_3h = 0,
                             snow_1h = 0,
                             snow_3h = 0))

# Drop the cancellation reason column since no flights are cancelled
# df <- df %>% select(-cancellation_reason)

# # Change remaining null values to 0 if flight was cancelleD
# df <- df %>%
#   mutate(DEPARTURE_DELAY = replace(DEPARTURE_DELAY, CANCELLED == 1, 0),
#          TAXI_OUT = replace(TAXI_OUT, CANCELLED == 1, 0),
#          ELAPSED_TIME = replace(ELAPSED_TIME, CANCELLED == 1, 0),
#          AIR_TIME = replace(AIR_TIME, CANCELLED == 1, 0),
#          TAXI_IN = replace(TAXI_IN, CANCELLED == 1, 0),
#          ARRIVAL_DELAY = replace(ARRIVAL_DELAY, CANCELLED == 1, 0))

# Missing value check
tibble(variable = names(colSums(is.na(df))),
       missing = colSums(is.na(df))) %>%
  gt()

# # Drop remaining missing values
# df <- drop_na(df)

# Delete date columns ahead of modeling
df <- df %>% select(-c(flight_date,
                       scheduled_departure_dt,
                       scheduled_arrival_dt))

# Association Rule Mining -------------------------------------------------

# Convert day of week to factor
day_of_week <- tibble(number = c(1, 2, 3, 4, 5, 6, 7),
                      weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
df <- left_join(df, day_of_week, by = c("day_of_week" = "number"))

# Convert month to a factor
df <- df %>% mutate(month = month(df$month, label = TRUE))

# Convert numeric variables to range factors
df <- df %>% 
  mutate(temp_range = cut(df$temp, 10),  # temperature
         pressure_range = cut(df$pressure, 10),  # barometric pressure
         wind_speed_range = cut(df$wind_speed, 10),  # wind speed
         rain_1h_range = cut(df$rain_1h, 10),  # rain_1h
         rain_3h_range = cut(df$rain_3h, 10)  # rain_3h
  )


# Select a subset for rule mining
df_arm <- df %>% 
  select(airline_code,
         tail_number,
         scheduled_departure_time,
         month,
         weekday,
         distance_bucket,
         temp_range,
         pressure_range,
         wind_speed_range,
         rain_1h_range,
         rain_3h_range,
         weather_main)

# Convert everything to a factor
df_arm <- df_arm %>% 
  mutate_if(is.character,as.factor)

# Convert to a transactional data set
df_arm_transactional <- as(df_arm, "transactions")

# Create rules
df_rules <- apriori(df_arm_transactional, 
                    parameter = list(support = 0.01,
                                     confidence = 0.8))

# Inspect rules
inspectDT(df_rules)

# Plot rules
plot(df_rules, engine = "html")
plot(head(df_rules, n = 50, by = "lift"), method = "graph")

# Rule Explorer
ruleExplorer(df_rules)

# Create sorted list of rules
df_rules_sorted <- sort(df_rules, by = "lift")
plot(df_rules_sorted, method = "grouped")
