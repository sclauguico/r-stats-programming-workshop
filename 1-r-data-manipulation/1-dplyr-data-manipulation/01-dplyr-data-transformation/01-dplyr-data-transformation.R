# Install packages 
install.packages("dplyr")

library(dplyr) # for tabular data manipulation


wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/1-r-data-manipulation/1-dplyr-data-manipulation/01-dplyr-data-transformation")

# Read the space mission csv file
space_mission <- read.csv("Space+Missions/space_missions.csv")


# SELECT COLUMNS
# Not all fields in a given table are needed in analysis. To prepare the data, you can simply SELECT the columns that you need

# Take a look at the dataset
glimpse(space_mission)

# Convert "Price" column from character to float in the dataframe
space_mission <- space_mission %>% 
  mutate(Price = as.numeric(Price))

# Check the class of the converted column
class(space_mission$Price)

space_mission %>%
  # Select the columns
  select(Date, Rocket, Mission, MissionStatus, Price) # These 4 columns were selected as compared to all the columns you have seen using the glimpse function


# FILTER and ARRANGE
# Filter allows you to select a subset of data that meets specific criteria or conditions
# Arrange allows you to organize it in a specific order, making it easier to analyze, visualize, or present the information effectively

# What were the most expensive missions?
space_mission_selected <- space_mission %>%
  select(Date, Rocket, Mission, MissionStatus, Price)

space_mission_selected %>%
  # Add a verb to sort in descending order of price
  arrange(desc(Price)) 


# What were the four missions that spend more than $120M?
space_mission_selected %>%
  # Filter for space missions with a price above $120M
  filter(Price > 120)


# What were the four missions that failed despite spending more than $120M?
space_mission_selected %>%
  # Filter for counties with a price above $120M, but the mission was a Failure
  filter(Price > 120, MissionStatus=="Failure") 


# What were the successful missions that only spend less than $80M? 
# And What were the 5 cheapest successful missions that succeeded?

space_mission_selected %>%
  # Filter for Successful missions spending only less than $80M?
  filter(MissionStatus == "Success", Price < 80) %>%
  
  # Sort in descending order of Price
  arrange(Price)

# MUTATE
# Mutate allows you to create new columns by specifying the name of the new column and its corresponding value

# Convert 'date' column to Date format
space_mission_selected <- space_mission_selected %>% 
  mutate(Date = as.Date(Date),
         Date = format(Date, "%m-%d-%Y"))

print(space_mission_selected)

library(lubridate)
space_mission_2000 <- space_mission_selected %>%
  filter(year(Date) == 2000)


print(space_mission_2000)


# What were the three most expensive mission in 2000 and what are their equivalent purchasing power in 2023?
# $1 in 2000 is equivalent in purchasing power to about $1.77 today

print(space_mission_2000)
space_mission_2000 <- space_mission_2000 %>%
  mutate(PriceNow = Price * 1.77) %>%
  arrange(desc(PriceNow))
  
print(space_mission_2000)

space_mission_2000 <- na.omit(space_mission_2000)



# If we are are to allocate a budget now to different missions, what would be the distribution of that budget based on its expenses?

space_mission_2000 %>%
  # Select  Mission, Missionstatus, Price
  select(Mission, MissionStatus, Price) %>%
  # Add the PriceNow variable
  mutate(PriceNow = Price * 1.77) %>%
  # Determine the distribution of price
  mutate(PriceDistribution = PriceNow / sum(PriceNow)) %>%
  # Filter for PriceDistribution more than 2%
  filter(PriceDistribution >= 0.02) %>%
  # Arrange distribution of men in descending order 
  arrange(desc(PriceDistribution))