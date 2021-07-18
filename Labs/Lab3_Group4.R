
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

#convert times to numeric for clustering later on
df$scheduled_departure <- as.numeric(df$scheduled_departure)
df$scheduled_arrival <- as.numeric(df$scheduled_arrival)


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

# # Change remaining null values to 0 if flight was cancelled
df <- df %>%
   mutate(departure_delay = replace(departure_delay, cancelled == 1, 0),
          taxi_out = replace(taxi_out, cancelled == 1, 0),
          elapsed_time = replace(elapsed_time, cancelled == 1, 0),
          air_time = replace(air_time, cancelled == 1, 0),
          taxi_in = replace(taxi_in, cancelled == 1, 0),
          arrival_delay = replace(arrival_delay, cancelled == 1, 0))

# Create a new column where the arrival_delay > 0 means it's delayed(=1) and if <= 0 it's not delayed(=0)
df <- df %>% 
  mutate(delayed_on_arrival = if_else(arrival_delay > 0, "Yes", "No")) %>%
  mutate(delayed_on_depart = if_else(departure_delay > 0, "Yes", "No"))


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

# Clustering & Creating New Variables ----------------------------------------------------

### SCHEDULED_DEPARTURE ###

#visualize the distribution before we cluster
ggplot(df, aes(x = scheduled_departure)) + 
  geom_histogram() +
  ggtitle("Distribution of Scheduled Departure Times (24H time)")

#find optimal numbers of clusters for k-means- not enough memory to use
#fviz_nbclust(as.matrix(df$scheduled_departure), kmeans, method = "wss") +
#  geom_vline(linetype = 2)

#run kmeans with k=5, meaning we will create 5 distinct groups for scheduled_departure
set.seed(1234)
kmeans_sched_depart <- kmeans(as.matrix(df$scheduled_departure), 5, nstart = 15)
print(kmeans_sched_depart)

#add to df
df$sched_depart_group <- kmeans_sched_depart$cluster
df$sched_depart_group <- as.factor(df$sched_depart_group)

#visualize cluster to see if 5 was appropriate
ggplot(df, aes(x = scheduled_departure)) + 
  geom_histogram(aes(fill = sched_depart_group)) +
  ggtitle("Distribution of Scheduled Departure Times (24H time)")

#check min and max
summary(df$scheduled_departure)

#perform grouping based on estimating cluster values
df$scheduled_departure_group <- cut(as.numeric(df$scheduled_departure),
                                    breaks = c(0, 930, 1300, 1630, 1950, 2359),
                                    labels = c("early morning", "morning/early afternoon", "afternoon", "evening",
                                               "overnight"))

ggplot(df, aes(x = scheduled_departure)) + 
  geom_histogram(aes(fill = scheduled_departure_group)) +
  ggtitle("Distribution of Scheduled Departure Times (24H time)")


### SCHEDULED_ARRIVAL ###

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

#check min and max
summary(df$scheduled_arrival)

#perform grouping based on estimating cluster values
df$scheduled_arrival_group <- cut(as.numeric(df$scheduled_arrival),
                                    breaks = c(0, 500, 1200, 1600, 2000, 2359),
                                    labels = c("early morning", "morning/early afternoon", "afternoon", "evening",
                                               "overnight"))

ggplot(df, aes(x = scheduled_arrival)) + 
  geom_histogram(aes(fill = scheduled_arrival_group)) +
  ggtitle("Distribution of Scheduled Arrival Times (24H time)")

### DISTANCE ###

#visualize the distribution before we cluster
ggplot(df, aes(x = distance)) + 
  geom_histogram() +
  ggtitle("Distribution of Distance")

#run kmeans with k=4, meaning we will create 4 distinct groups
kmeans_dist <- kmeans(as.matrix(df$distance), 4, nstart = 15)
print(kmeans_dist)

#add to df
df$distance_bucket <- kmeans_dist$cluster
df$distance_bucket <- as.factor(df$distance_bucket)

#visualize cluster
ggplot(df, aes(x = distance)) + 
  geom_histogram(aes(fill = distance_bucket)) +
  ggtitle("Distribution of Flight Distance")

#check min and max
summary(df$distance)

#perform grouping based on estimating cluster values
df$distance_group <- cut(as.numeric(df$distance),
                                  breaks = c(0, 575, 990, 2500, 4000),
                                  labels = c("< 575 miles", "Btwn 575 and 990 miles", "Btwn 990 and 2500 miles", 
                                             "> 4000 miles"))

ggplot(df, aes(x = distance)) + 
  geom_histogram(aes(fill = distance_group)) +
  ggtitle("Distribution of Flight Distance")

#remove now unused variables to save memory
df <- df %>%
  select(-c('sched_depart_group', 'sched_arrive_group', 'distance_bucket'))

#check data
str(df)

# Prepare for ARM ---------------------------------------------------------

#update for human readability
df <- df %>% 
  mutate(delayed_on_arrival = if_else(arrival_delay > 0, "Yes", "No")) %>%
  mutate(delayed_on_depart = if_else(departure_delay > 0, "Yes", "No"))

# Create binary variables for association rules
df <- df %>% 
  mutate(air_system_delay = if_else(air_system_delay > 0, "Yes", "No")) %>%
  mutate(security_delay = if_else(security_delay > 0, "Yes", "No")) %>%
  mutate(airline_delay = if_else(airline_delay > 0, "Yes", "No")) %>%
  mutate(late_aircraft_delay = if_else(late_aircraft_delay > 0,"Yes", "No")) %>%
  mutate(weather_delay = if_else(weather_delay > 0, "Yes", "No"))

df <- df %>% 
  mutate(diverted = if_else(diverted == 1, "Yes", "No")) %>%
  mutate(cancelled = if_else(cancelled == 1, "Yes", "No")) %>%
  mutate(departed = if_else(departed == 1, "Yes", "No")) %>%
  mutate(arrived = if_else(arrived == 1, "Yes", "No"))

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

# Convert everything to a factor
df <- df %>% 
  mutate_if(is.character,as.factor)

#check data
str(df)

# Association Rule Mining -------------------------------------------------

# Select a subset for rule mining
df_arm <- df %>% 
  select(airline_code,
         tail_number,
         scheduled_departure_group,
         month,
         weekday,
         distance_group,
         temp_range,
         pressure_range,
         wind_speed_range,
         rain_1h_range,
         rain_3h_range,
         weather_main)

# Convert to a transactional data set
df_arm_transactional <- as(df_arm, "transactions")

# Create rules
df_rules <- apriori(df_arm_transactional, 
                    parameter = list(support = 0.01,
                                     confidence = 0.8))

# Inspect rules
inspectDT(df_rules)

inspect(head(df_rules, n = 10, by = "lift"))

# Plot rules
plot(df_rules, engine = "html")
plot(head(df_rules, n = 50, by = "lift"), method = "graph")

# Rule Explorer
ruleExplorer(df_rules)

# Create sorted list of rules
df_rules_sorted <- sort(df_rules, by = "lift")
plot(df_rules_sorted, method = "grouped")
inspect(head(df_rules_sorted, 3))



# arules: Cancelled on RHS ------------------------------------------------


# Select a subset for rule mining
df_arm <- df %>% 
  select(month,
         airline_code,
         tail_number,
         origin_airport,
         destination_airport,
         weekday,
         air_system_delay,
         security_delay,
         airline_delay,
         late_aircraft_delay,
         weather_delay,
         weather_main,
         delayed_on_arrival,
         delayed_on_depart,
         scheduled_departure_group,
         scheduled_arrival_group,
         distance_group,
         temp_range,
         pressure_range,
         wind_speed_range,
         rain_1h_range,
         rain_3h_range,
         cancelled)

# Convert to a transactional data set
df_arm_transactional <- as(df_arm, "transactions")

cancelled <- grep("cancelled", itemLabels(df_arm_transactional), value = TRUE)
cancelled

# Create rules
df_rules <- apriori(df_arm_transactional, 
                    appearance = list(rhs = cancelled),
                    parameter = list(support = 0.01,
                                     confidence = 0.8,
                                     minlen = 3))

# Inspect rules
inspectDT(df_rules)

inspect(head(df_rules, n = 10, by = "lift"))

# Plot rules
plot(df_rules, engine = "html")
plot(head(df_rules, n = 50, by = "lift"), method = "graph")

# Rule Explorer
ruleExplorer(df_rules)

# Create sorted list of rules
df_rules_sorted <- sort(df_rules, by = "lift")
plot(df_rules_sorted, method = "grouped")
inspect(head(df_rules_sorted, 3))

# arules: Delayed_on_arrival on RHS ------------------------------------------------


# Select a subset for rule mining
df_arm <- df %>% 
  select(month,
         airline_code,
         tail_number,
         origin_airport,
         destination_airport,
         weekday,
         air_system_delay,
         security_delay,
         airline_delay,
         late_aircraft_delay,
         weather_delay,
         weather_main,
         delayed_on_depart,
         scheduled_departure_group,
         scheduled_arrival_group,
         distance_group,
         temp_range,
         pressure_range,
         wind_speed_range,
         rain_1h_range,
         rain_3h_range,
         delayed_on_arrival)

# Convert to a transactional data set
df_arm_transactional <- as(df_arm, "transactions")

delayed_on_arrival <- grep("delayed_on_arrival", itemLabels(df_arm_transactional), value = TRUE)
delayed_on_arrival

# Create rules
df_rules <- apriori(df_arm_transactional, 
                    appearance = list(rhs = delayed_on_arrival),
                    parameter = list(support = 0.01,
                                     confidence = 0.8,
                                     minlen = 3))

# Inspect rules
inspectDT(df_rules)

inspect(head(df_rules, n = 10, by = "lift"))

# Plot rules
plot(df_rules, engine = "html")
plot(head(df_rules, n = 50, by = "lift"), method = "graph")

# Rule Explorer
ruleExplorer(df_rules)

# Create sorted list of rules
df_rules_sorted <- sort(df_rules, by = "lift")
plot(df_rules_sorted, method = "grouped")
inspect(head(df_rules_sorted, 3))

# arules: Delayed_on_departure on RHS ------------------------------------------------


# Select a subset for rule mining
df_arm <- df %>% 
  select(month,
         airline_code,
         tail_number,
         origin_airport,
         destination_airport,
         weekday,
         air_system_delay,
         security_delay,
         airline_delay,
         late_aircraft_delay,
         weather_delay,
         weather_main,
         delayed_on_arrival,
         scheduled_departure_group,
         scheduled_arrival_group,
         distance_group,
         temp_range,
         pressure_range,
         wind_speed_range,
         rain_1h_range,
         rain_3h_range,
         delayed_on_depart)

# Convert to a transactional data set
df_arm_transactional <- as(df_arm, "transactions")

delayed_on_depart <- grep("delayed_on_depart", itemLabels(df_arm_transactional), value = TRUE)
delayed_on_depart

# Create rules
df_rules <- apriori(df_arm_transactional, 
                    appearance = list(rhs = delayed_on_depart),
                    parameter = list(support = 0.01,
                                     confidence = 0.8,
                                     minlen = 3))

# Inspect rules
inspectDT(df_rules)

inspect(head(df_rules, n = 10, by = "lift"))

# Plot rules
plot(df_rules, engine = "html")
plot(head(df_rules, n = 50, by = "lift"), method = "graph")

# Rule Explorer
ruleExplorer(df_rules)

# Create sorted list of rules
df_rules_sorted <- sort(df_rules, by = "lift")
plot(df_rules_sorted, method = "grouped")
inspect(head(df_rules_sorted, 3))