# Install the dplyr package
install.packages("dplyr")

# Dplyr is a grammar of data manipulation in R that provides a consistent set of verbs 
# to help you solve the most common data manipulation challenges.
# Syntax: tibble pipe-operator
#             verb(parameters)

library(dplyr) # for tabular data manipulation

# Determine the current working directory
wd <- getwd()
# Set the proper working directory
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/1-r-data-manipulation/1-dplyr-data-manipulation/01-dplyr-data-transformation")

# Read the space mission csv file
space_mission <- read.csv("Space+Missions/space_missions.csv")

# Take a look at the dataset
View(space_mission)

# Determine the dimension of the dataset
dim(space_mission)

# Take a glimpse of the data type
glimpse(space_mission)


# 0 --- PRELIMINARY DATA TRANSFORMATION ---
# A. RENAMING VARIABLES
space_mission <- space_mission %>% # shift + ctrl + m
  rename("SpaceShuttleStatus" = "RocketStatus") # First argument is the new column name, the second one is the original column

# Other way of taking a look at the dataset
head(space_mission)

# B. REORDER VARIABLES
space_mission_reordered <- space_mission %>%
  select(Date, Mission, MissionStatus, Rocket, Price)

# C. CHANGE A VARIABLE TYPE
# Take a look at the class of MissionStatus column
class(space_mission$MissionStatus)
# Further verify the class of MissionStatus
glimpse(space_mission)

# Change the variable type or class as factor
space_mission$MissionStatus <- as.factor(space_mission$MissionStatus) # factors are a data type that represent categorical variables.
# Further verify the NEW class of MissionStatus
glimpse(space_mission)

# D. OBTAIN UNIQUE DATA
unique(space_mission$Company)

# A tibble is a modern data frame in R that is designed to be easy to read, print, and manipulate.

# Tibbles are a subclass of data frames, so they inherit all of the functionality of data frames.
# Tibbles have a few additional features that make them more useful for data analysis, such as:
# They do not automatically convert strings to factors.(In R, factors are a data type that represent categorical variables. Factors have a predefined set of unique values, and R assigns an integer level to each unique value.)
# They print out in a human-readable format.
# They are consistent with the tidyverse philosophy of data wrangling.
# Tibbles are available in the dplyr and tibble packages.

# Convert "Price" column from character to numeric in the data frame or tibble.
space_mission <- space_mission %>% # called the pipe operator in R. It is a special operator that allows you to chain together functions. This can be very useful for data manipulation, as it allows you to write code that is more concise and readable.
  # mutate is used for creating or changing a column in R
  mutate(Price = as.numeric(Price)) # The as.numeric() function in R is used to convert a vector of characters or factors to a vector of numeric values

# Check the class or data type of the converted column
class(space_mission$Price)

# Further verify by using the glimpse function
glimpse(space_mission)

# dbl refers to a double data type. A double is a double-precision floating point number, 
# which means that it can represent a wide range of values with a high degree of accuracy



# --- 1. SELECT ---

# Not all fields in a given table are needed in the analysis. 
# To prepare the data, you can simply SELECT the columns that you need based on your use cases.

space_mission %>%
  # Add the verb select to select the columns: Date, Rocket, Mission, MissionStatus, Price
  select(Date, Rocket, Mission, MissionStatus, Price) # These 5 columns were selected as compared to all the columns you have seen using the glimpse function

# Can you save the tibble with the selected columns as space_mission selected?
space_mission_selected <- space_mission %>%
  select(Company, Date, Rocket, Mission, MissionStatus, Price)


# You can also select using indexes for obtaining consecutive columns
names(space_mission) # For getting the column names

space_mission %>%
  # select the columns from the third to the seventh 
  select(3:7)

# --- 2. FILTER and ARRANGE ---

# Filter allows you to select a subset of data that meets specific criteria or conditions
# Arrange allows you to organize it in a specific order, making it easier to analyze, visualize, 
# or present the information effectively

# What were the missions with the most expensive rockets?
space_mission_selected %>%
  # Add the verb arrange to order the price column in descending order
  arrange(desc(Price)) 


# What were the missions that spend more than $120M on rockets?
space_mission_selected %>%
  # Add the verb filter to obtain the samples with rockets amounting above $120M
  filter(Price > 120) %>%
  #Another way of arranging in descending order
  arrange(-Price)


# What were the missions that failed despite spending more than $120M on rockets?
space_mission_selected %>%
  # Having more than one condition for filtering can be done by separating them via a comma
  # Comma represents an AND. If you want to do an OR, use | (or operator)
  # Filter for rockets with a price above $120M, but the mission was a Failure
  filter(Price > 120, MissionStatus=="Failure") 


# What were the successful missions that only spend less than $80M on rockets? 
# And What were the successful missions that had the cheapest rockets?
space_mission_selected %>%
  # Filter for Successful missions spending only less than $80M on rockets
  filter(MissionStatus == "Success", Price < 80) %>%
  # Sort in descending order of Price
  arrange(Price)


# What were the rockets launched that succeeded from the US Navy or the Sputnik-1 mission?
library(stringr)
# Usually this should be added on the top of the R source file

space_mission_selected %>%
  select(Rocket, Company, Mission, MissionStatus) %>% 
  # Filter for Successful missions from the US Navy company or Sputnik-1 mission
  filter((Company == "US Navy" | Mission == "Sputnik-1") & str_starts(MissionStatus, "S")) # str_starts contains samples of specified column that starts with the specified character/s


# What were the rockets launched that failed and partially failed from the US Navy or NASA?
space_mission_selected %>%
  select(Rocket, Company, Mission, MissionStatus) %>% 
  # Filter failed and partially failed missions from the US Navy or NASA
  filter(Company %in% c("US Navy", "NASA") & str_detect(MissionStatus, "Failure")) %>% # str_detect obtains samples of specified column that contains the specified character/s
  arrange(MissionStatus)
  
# 3. --- MUTATE ---

# Mutate allows you to create new columns by specifying the name of the new column and its corresponding value

# Convert 'date' column to Date format
space_mission_date_formatted <- space_mission_selected %>% 
  # Add the verb mutate to create a column Date from the original Date column follow: MM-DD-YYYY
  mutate(Date = as.Date(Date))
  

glimpse(space_mission_selected)
glimpse(space_mission_date_formatted)

# Another way of taking a look at the dataset
print(space_mission_date_formatted)

# RECODING A VARIABLE

space_mission_selected <- space_mission_selected %>%
  # Modify MissionStatus column based on conditions
  mutate(MissionStatus = case_when(
    # If MissionStatus is "Success", assign 1
    MissionStatus == "Success" ~ 1,
    # If MissionStatus contains "Failure", assign 0
    str_detect(MissionStatus, "Failure") ~ 0,
    # For other cases, assign NA
    TRUE ~ NA_integer_
  ))

head(space_mission_selected)


# What were the mission in the year 2000?

# Convert date column as date format

# Lubridate is an R package that provides a comprehensive and user-friendly way to work with dates and times in R.
library(lubridate) 
# Usually this should be added on the top of the R source file

space_mission_2000 <- space_mission_selected %>%
  filter(year(Date) == 2000) # year function is from lubridate not dplyr

head(space_mission_2000)

# What were the three most expensive rockets in 2000 and what are their equivalent prices in 2023?

space_mission_2000 <- space_mission_2000 %>%
  # $1 in 2000 is equivalent to purchasing power of about $1.77 today
  mutate(PriceNow = Price * 1.77) %>%
  arrange(desc(PriceNow))

# Take a look at the new space_mission_2000  
head(space_mission_2000)


# What is the price rocket expense distribution? 
# What missions are those with more than 20% of the distribution?

# First we have to remove the null values
space_mission_2000 <- na.omit(space_mission_2000)


space_mission_2000 %>%
  # Select Mission, Missionstatus, Price
  select(Mission, MissionStatus, Price) %>%
  # Determine the distribution of price
  mutate(PriceDistribution = Price / sum(Price)) %>%
  # Format the Price Distribution in percentages with two decimal places
  mutate(PriceDistribution = paste0(round(PriceDistribution * 100, 2), "%")) %>%
  # Filter for PriceDistribution more than 2%
  filter(PriceDistribution >= 0.02) %>%
  # Arrange distribution of rocket expenses in descending order
  arrange(desc(PriceDistribution))
