# Install packages 
install.packages("dplyr")

library(dplyr) # for tabular data manipulation


wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/1-r-data-manipulation/1-dplyr-data-manipulation/02-aggregating-data")

# Read the airline passenger satisfaction csv file
airline_satisfaction <- read.csv("Airline+Passenger+Satisfaction/airline_passenger_satisfaction.csv")

# Take a look at the dataset
head(airline_satisfaction)
glimpse(airline_satisfaction)
airline_satisfaction <- na.omit(airline_satisfaction)

# 1. --- COUNT --- 
# Count allows you to perform counting the number of values of a specific field (no. of rows for a specific column)
# SYNTAX: tibble pipe-operator 
#         count(parameters)


# How many departures were delayed?
airline_satisfaction %>%
  # Filter departure delays that are more than 0 minutes
  filter(Departure.Delay > 0) %>%
  # Get the number of delays by departure delays in minutes
  count(Departure.Delay, sort = TRUE)


# Another way of counting
airline_satisfaction %>%
  # Group by allows you to groups samples / rows of data based of 1 or 2 columns
  group_by(Age) %>%
  summarize(number_of_passengers = n())



# 2.--- SUMMARIZE --- 
# Summarize allows you to collapse a large dataset to a single observation
# Summarize is similar to Aggregation
# SYNTAX: tibble pipe-operator 
#         summarize(parameters)

# What is the range of flight delays and the average of flight distances?
airline_satisfaction%>%
  # Filter departure delays that are more than 0 minutes
  filter(Departure.Delay > 0) %>%
  # Summarize to find minimum inflight service, maximum departure delay, and average flight distance in miles
  summarize(min_departure_delay = min(Departure.Delay),
            max_departure_delay = max(Departure.Delay),
            average_flight_distance = mean(Flight.Distance))



# What class have high average satisfaction in terms of food and drink, baggage handling, and leg room service?
airline_satisfaction %>%
  # Group by allows you to groups samples / rows of data based of 1 or 2 columns
  group_by(Class) %>%
  summarize(ave_fad = mean(Food.and.Drink),
            ave_bh = mean(Baggage.Handling),
            ave_sc = mean(Seat.Comfort),
            ave_lrs = mean(Leg.Room.Service),) %>%
  # Add a ave_sat column
  mutate(ave_sat = (ave_fad + ave_bh + ave_sc + ave_lrs)/4) %>%
  # Sort by average satisfaction in descending order
  arrange(desc(ave_sat))



# Multiple group by is also possible
# What is the total flight distance by Class and Type of Travel?
airline_satisfaction %>%
  # Group and summarize to find the total distance
  group_by(Class, Type.of.Travel) %>%
  summarize(total_distance = sum(Flight.Distance))


# SLICE_MAX/MIN
# slice_max and slice_min allow you to get the N max and min value/s respectively depending on the number of records
# you specified to show for a certain group

# What are the top 2 longest flight distance by age?
highest_distance <- airline_satisfaction %>%
  # Group by age
  group_by(Age) %>%
  # Get the two highest flight distances (by age)
  slice_max(Flight.Distance, n = 2) %>%
  # Sort by age in descending order
  arrange(-Age)

print(highest_distance)

# The two highest flight distances for 79-year olds were 3924 and 3626 miles



# What are the 4 highest flight distances for 21-year olds?
airline_satisfaction %>%
  # Group by age
  group_by(Age) %>%
  # Get the 4 highest flight distances (by age)
  slice_max(Flight.Distance, n = 4) %>%
  # Show only customers who are 21 years old
  filter(Age == 21) %>%
  # Sort by flight distance in desccending order
  arrange(Flight.Distance)



# At what age is the least average flight distance by gender?
airline_satisfaction %>%
  group_by(Gender, Age) %>%
  # Calculate average flight distance
  summarize(average_flight_distance = mean(Flight.Distance)) %>%
  # Find the lowest flight distance for each gender
  slice_min(average_flight_distance, n = 1) %>%
  # Sort by age in descending order
  arrange(desc(Age))
  


# How many passenger ages travel longer for business than personal?
airline_satisfaction %>%
  # Find the total flight distance for each combination of age and type of travel
  group_by(Age, Type.of.Travel) %>%
  summarize(total_flight_distance = sum(Flight.Distance)) %>%
  # Extract the longest flight distance for each age
  slice_max(total_flight_distance, n = 1) %>%
  # Count the age with long flight distances for different types of travel
  ungroup() %>% 
  count(Type.of.Travel)

# 58 passenger ages travel longer for business purposes, while only 17 passenger ages travel longer for business purposes



# What is the average overall satisfaction per customer type, who are also female, traveling for business, and who are between the ages of 25 and 35?
airline_satisfaction %>%
  # Create a new column overall satisfaction from the average of all survey metrics
  mutate(Overall.Satisfaction = rowMeans(select(., 10:23) %>% mutate(across(where(is.character), as.numeric, .names = "numeric_{.col}")))) %>% 
  # Filter female passengers, traveling for business, aged 25 to 35
  filter(Gender == "Female" & Type.of.Travel == "Business" & Age >= 25 & Age <= 35) %>%
  # Group by customer type
  group_by(Customer.Type) %>%
  # Create a summarized/agrregated view 
  summarize(average_satisfaction = mean(Overall.Satisfaction)) %>%
  # Sort descendingly by average satisfaction
  arrange(-average_satisfaction)

