
# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(arules)
library(arulesViz)
library(gt)
library(cluster)
library(factoextra)

# Load Data ---------------------------------------------------------------

# Read in the data directly
#airlines <- read_csv(here("Data", "airlines.csv"))
#airports <- read_csv(here("Data", "airports.csv"))
#df       <- read_csv(here("Data", "flights.csv"))

airlines <- read_csv(here("airlines.csv"))
airports <- read_csv(here("airports.csv"))
#df       <- read_csv(here("flights.csv"))
flights  <- read_csv(here("flights.csv"))

# Read in the data directly from GitHub
# airlines <- read_csv('https://raw.githubusercontent.com/mattfarrow1/7331-machine-learning-1/main/Data/airlines.csv')
# airports <- read_csv('https://raw.githubusercontent.com/mattfarrow1/7331-machine-learning-1/main/Data/airports.csv')
# df       <- read_csv('https://media.githubusercontent.com/media/mattfarrow1/7331-machine-learning-1/main/Data/flights.csv')

# Clean up column names
airlines <- airlines %>% 
  clean_names() %>% 
  rename(airline_code = iata_code)
airports <- clean_names(airports)
df <- flights %>% 
  clean_names() %>% 
  rename(airline_code = airline)


# Merge data together
df <- left_join(df, airlines, by ="airline_code")

# Subset to DFW Area
df <- df %>% filter(origin_airport == "DFW" | origin_airport == "DAL")

# Drop cancelled flights
# df <- df %>% filter(cancelled == 0)

# Create New Variables ----------------------------------------------------

# Convert times into buckets for morning, afternoon, and evening as most models cannot handle timestamps.
#time_labels = c('overnight', 'morning', 'afternoon', 'evening')
#time_bins   = c(0, 559, 1159, 1759, 2359)

#df$scheduled_departure_time <- cut(as.numeric(df$scheduled_departure),
#                                   breaks = time_bins,
#                                   labels = time_labels)

#df$actual_departure_time    <- cut(as.numeric(df$departure_time),
#                                   breaks = time_bins,
#                                   labels = time_labels)

#df$scheduled_arrival_time   <- cut(as.numeric(df$scheduled_arrival),
#                                   breaks = time_bins,
#                                   labels = time_labels)

#df$actual_arrival_time <-   cut(as.numeric(df$arrival_time),
#                                breaks = time_bins,
#                                labels = time_labels)

# Bucket Flight Distance
#distance_labels <- c('Short', 'Medium', 'Long')
#distance_bins   <- c(1, 100, 1000, Inf)

#df$distance_bucket <- cut(df$distance,
#                         breaks = distance_bins,
#                         labels = distance_labels,
#                          include.lowest = TRUE)

# Look at our data with the buckets
#head(df)

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
#df$scheduled_departure <- parse_date_time(df$scheduled_departure, "%H%M")
#df$departure_time      <- parse_date_time(df$departure_time, "%H%M")
#df$scheduled_arrival   <- parse_date_time(df$scheduled_arrival, "%H%M")
#df$arrival_time        <- parse_date_time(df$arrival_time, "%H%M")
#df$wheels_on           <- parse_date_time(df$wheels_on, "%H%M")
#df$wheels_off          <- parse_date_time(df$wheels_off, "%H%M")

# Append Dallas-Area Weather ----------------------------------------------

# Read in the data
#weather <- read_csv(here("Data", "dfw_weather.csv"))
weather <- read_csv(here("dfw_weather.csv"))

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

#convert times to numeric for clustering later on
df$scheduled_departure <- as.numeric(df$scheduled_departure)
df$scheduled_arrival <- as.numeric(df$scheduled_arrival)
df$departure_time <- as.numeric(df$departure_time)
df$arrival_time <- as.numeric(df$arrival_time)


df <- df %>% replace_na(list(cancellation_reason = 'N',
                        departure_time = 0))

# Convert departure_time to a binary value - departed
df <- df %>% 
  mutate(departure_time = replace(departure_time, departure_time != 0, 1)) %>%
  rename(departed = departure_time)

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
df <- df %>%
   mutate(departure_delay = replace(departure_delay, cancelled == 1, 0),
          taxi_out = replace(taxi_out, cancelled == 1, 0),
          elapsed_time = replace(elapsed_time, cancelled == 1, 0),
          air_time = replace(air_time, cancelled == 1, 0),
          taxi_in = replace(taxi_in, cancelled == 1, 0),
          arrival_delay = replace(arrival_delay, cancelled == 1, 0))

# Create a new column where the arrival_delay > 0 means it's delayed(=1) and if <= 0 it's not delayed(=0)
df <- df %>% 
  mutate(delayed = if_else(arrival_delay > 0, 1, 0))


# Missing value check
tibble(variable = names(colSums(is.na(df))),
       missing = colSums(is.na(df))) %>%
  gt()

# # Drop remaining missing values
df <- drop_na(df)

# Delete date columns ahead of modeling
df <- df %>% select(-c(flight_date,
                       scheduled_departure_dt,
                       scheduled_arrival_dt))

#remove flights to save RAM
rm(flights)

# Clustering --------------------------------------------------------------

### scheduled_departure

#visualize the distribution before we cluster
ggplot(df, aes(x = scheduled_departure)) + 
  geom_histogram() +
  ggtitle("Distribution of Scheduled Departure Times (24H time)")

#find optimal numbers of clusters for k-meansv- not enough memory to use
#fviz_nbclust(as.matrix(df$scheduled_departure), kmeans, method = "wss") +
#  geom_vline(linetype = 2)

#run kmeans with k=5, meaning we will create 5 distinct groups for scheduled_departure
set.seed(1234)
kmeans_sched_depart <- kmeans(as.matrix(df$scheduled_departure), 5, nstart = 15)
print(kmeans_sched_depart)

#add to df
df$sched_depart_group <- kmeans_sched_depart$cluster
df$sched_depart_group <- as.factor(df$sched_depart_group)

#visualize cluster
ggplot(df, aes(x = scheduled_departure)) + 
  geom_histogram(aes(fill = sched_depart_group)) +
  ggtitle("Distribution of Scheduled Departure Times (24H time)")

### scheduled_arrival

#visualize the distribution before we cluster
ggplot(df, aes(x = scheduled_arrival)) + 
  geom_histogram() +
  ggtitle("Distribution of Scheduled Arrival Times (24H time)")

#run kmeans with k=5, meaning we will create 5 distinct groups for scheduled_arrival
kmeans_sched_arrive <- kmeans(as.matrix(df$scheduled_arrival), 5, nstart = 15)
print(kmeans_sched_arrive)

#add to df
df$sched_arrive_group <- kmeans_sched_arrive$cluster
df$sched_arrive_group <- as.factor(df$sched_arrive_group)

#visualize cluster
ggplot(df, aes(x = scheduled_arrival)) + 
  geom_histogram(aes(fill = sched_arrive_group)) +
  ggtitle("Distribution of Scheduled Arrival Times (24H time)")

### distance

#visualize the distribution before we cluster
ggplot(df, aes(x = distance)) + 
  geom_histogram() +
  ggtitle("Distribution of Distance")

#we have quite a few outliers here, but this should not be an issue ultimately in association rule mining, assuming
#that kmeans clustering groups them together

#run kmeans with k=4, meaning we will create 4 distinct groups
kmeans_dist <- kmeans(as.matrix(df$distance), 4, nstart = 15)
print(kmeans_dist)

#add to df
df$distance_bucket <- kmeans_dist$cluster
df$distance_bucket <- as.factor(df$distance_bucket)

#visualize cluster
ggplot(df, aes(x = distance)) + 
  geom_histogram(aes(fill = distance_bucket)) +
  ggtitle("Distribution of Distance")

ggplot(df, aes(x = rain_1h)) + 
  geom_histogram()

kmeans_rain_1h <- kmeans(as.matrix(df$rain_1h), 6, nstart = 15)
print(kmeans_rain_1h)

#add to df
df$rain_1h_bucket <- kmeans_rain_1h$cluster
df$rain_1h_bucket <- as.factor(df$rain_1h_bucket)

ggplot(df, aes(x = rain_1h)) + 
  geom_histogram(aes(fill = rain_1h_bucket)) +
  scale_y_log10()

ggplot(df, aes(x = rain_1h)) + 
  geom_histogram(aes(fill = rain_1h_range)) +
  scale_y_log10() 

#assign human readable versions of cluster



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
inspect(head(df_rules_sorted, 3))



# arules: Cancelled on LHS ------------------------------------------------


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
         weather_main,
         cancelled)

df_arm <- df_arm %>% 
  mutate(cancelled = if_else(df_arm$cancelled == 0, "Not Cancelled", "Cancelled"))

# Convert everything to a factor
df_arm <- df_arm %>% 
  mutate_if(is.character,as.factor)

# Convert to a transactional data set
df_arm_transactional <- as(df_arm, "transactions")

cancelled <- grep("cancelled", itemLabels(df_arm_transactional), value = TRUE)
cancelled

# Create rules
df_rules <- apriori(df_arm_transactional, 
                    appearance = list(lhs = cancelled),
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
inspect(head(df_rules_sorted, 3))
