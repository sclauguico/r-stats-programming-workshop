# Install packages 
install.packages("dplyr")

library(dplyr) # for tabular data manipulation


wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/1-r-data-manipulation/1-dplyr-data-manipulation/02-aggregating-data")

# Read the airline passenger satisfaction csv file
airline_statisfaction <- read.csv("Airline+Passenger+Satisfaction/airline_passenger_satisfaction.csv")

# Take a look at the dataset
head(airline_satisfaction)
glimpse(airline_statisfaction)
airline_statisfaction <- na.omit(airline_statisfaction)

# 1. --- COUNT --- 
# Count allows you to perform counting the number of values of a specific field


# How many departures were delayed by minutes?
airline_statisfaction %>%
  # Filter departure delays that are more than 0 minutes
  filter(Departure.Delay > 0) %>%
  # Get the number of delays by departure delays in minutes
  count(Departure.Delay, sort = TRUE)



# 2.--- SUMMARIZE --- 
# Summarize allows you to collapse a large dataset to a single observation

# What is the range of flight delays and average flight distance?
airline_statisfaction%>%
  # Filter departure delays that are more than 0 minutes
  filter(Departure.Delay > 0) %>%
  # Summarize to find minimum inflight service, maximum departure delay, and average flight distance
  summarize(min_departure_delay = min(Departure.Delay),
            max_departure_delay = max(Departure.Delay),
            average_flight_distance = mean(Flight.Distance))



# What class have high average satisfaction?
airline_statisfaction %>%
  group_by(Class) %>%
  summarize(ave_fad = sum(Food.and.Drink),
            ave_bh = sum(Baggage.Handling),
            ave_sc = sum(Seat.Comfort),
            ave_lrs = sum(Leg.Room.Service),) %>%
  # Add a ave_sat column
  mutate(ave_sat = (ave_fad + ave_bh + ave_sc + ave_lrs)/4) %>%
  # Sort by average satisfaction in descending order
  arrange(desc(ave_sat))


# What is the total flight distance by Class and Type of Travel?
airline_statisfaction %>%
  # Group and summarize to find the total distance
  group_by(Class, Type.of.Travel) %>%
  summarize(total_distance = sum(Flight.Distance))


# SLICE_MAX/MIN
# slice_max and slice_min allow you to get N max and min value/s respectively depending on the number of records
# you specified to show for a certain group

# What are the top 2 longest flight distance by age?
highest_distance <- airline_statisfaction %>%
  # Group by class
  group_by(Age) %>%
  # Find the class with the highest ease of online booking satisfaction
  slice_max(Flight.Distance, n = 2) %>%
  # Sort by age in descending order
  arrange(desc(Age))

print(highest_distance)


# At what age is the least average flight distance by gender?
airline_statisfaction %>%
  group_by(Gender, Age) %>%
  # Calculate average flight distance
  summarize(average_flight_distance = mean(Flight.Distance)) %>%
  # Find the lowest flight distance for each gender
  slice_min(average_flight_distance, n = 1) %>%
  # Sort by age in descending order
  arrange(desc(Age))
  

# How many passengers at a certain age travel longer for business than personal?
airline_statisfaction %>%
  # Find the total flight distance for each combination of age and type of travel
  group_by(Age, Type.of.Travel) %>%
  summarize(total_flight_distance = sum(Flight.Distance)) %>%
  # Extract the longest flight distance for each age
  slice_max(total_flight_distance, n = 1) %>%
  # Count the age with long flight distances for different types of travel
  ungroup() %>% 
  count(Type.of.Travel)

# 58 passenger ages travel longer for business purposes, while only 17 passenger ages travel longer for business purposes

