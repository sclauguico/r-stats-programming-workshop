# Install packages 
install.packages("tidyverse")
install.packages("dplyr")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("lubridate")


library(tidyverse) # for data tidying
library(dplyr) # for tabular data manipulation
library(magrittr)
library(ggplot2) # for visualization
library(lubridate) # for handling date fields


wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/2-r-statistics-fundamentals/1-r-intro-to-statistics/01-summary-statistics")

# MEASURES OF CENTER


# Measures of center: These are statistical values that represent the central tendency or average of a dataset, such as the mean, median, or mode.
# Normal distribution: It is a symmetric probability distribution where the majority of data points cluster around the mean, resulting in a bell-shaped curve.
# Skewness: It is a measure of the asymmetry of a distribution, indicating whether the data is skewed to the left (negative skewness) or to the right (positive skewness).

# Symmetric Curve:
#   
# Mean: The mean is a measure of center that represents the average value of the data. In a symmetric curve, the mean is equal to the median, as both values lie at the center of the distribution.
# Median: The median is the middle value in a dataset when it is arranged in ascending or descending order. In a symmetric curve, the median is the same as the mean, as both values are located at the center of the distribution.
# Skewed Curve:
#   
# Mean: The mean is influenced by extreme values in a skewed curve. If the curve is positively skewed (skewed to the right), the mean is pulled towards the higher values. If the curve is negatively skewed (skewed to the left), the mean is pulled towards the lower values.
# Median: The median is less affected by extreme values in a skewed curve. It represents the middle value, unaffected by the skewness. In a positively skewed curve, the median is usually less than the mean, while in a negatively skewed curve, the median is typically greater than the mean.

# Mean and Median
# Read the S&P 500 csv file
sp500 <- read.csv("S&P 500 Stock Prices 2014-2017.csv")

# Get the data types of each column
column_types <- sapply(sp500, class)

# Print the data types
print(column_types)

# library(dplyr)
# Convert 'date' column to Date format
sp500 <- mutate(sp500, date = as.Date(date, format = "%d/%m/%Y"))

# Create a new column 'year' based on 'date'
sp500 <- mutate(sp500, year = format(date, "%Y"))

# Print the updated dataframe
print(sp500)

# Show unique values in the "year" column of the sp500
unique_years <- unique(sp500$year)

# Print the unique values
print(unique_years)

# Filter for 2014 data
closing_2014 <- sp500 %>%
  filter(year == '2014')

# Filter for 2015 data
closing_2015 <- sp500 %>%
  filter(year == '2015')

# Filter for 2016 data
closing_2016 <- sp500 %>%
  filter(year == '2016')

# Filter for 2017 data
closing_2017 <- sp500 %>%
  filter(year == '2017')

# Calculate mean and median of close for year 2014
mean(closing_2014$close)
median(closing_2014$close)

# Calculate mean and median of close for year 2015
mean(closing_2015$close)
median(closing_2015$close)

# Calculate mean and  median of close for year 2016
mean(closing_2016$close)
median(closing_2016$close)

# Calculate mean and  median of close for year 2017
mean(closing_2017$close)
median(closing_2017$close)


sp500 %>%
  # Filter for closing price of years 2014 to 2017
  filter(year %in% c("2014", "2015","2016","2017")) %>%
  # Group by size
  group_by(year) %>%
  # Get mean_close and median_close
  summarize(mean_close = mean(close),
            median_close = median(close))


# Mean vs Median
sp500 %>%
  # Create histogram of close
  ggplot(aes(close)) +
  geom_histogram(fill = "royalblue4")

# Q: How do you think the mean is compared to the median with the histogram?

sp500 %>%
  # Get mean_close and median_close
  summarize(mean_close = mean(close),
            median_close = median(close))

# A: Mean is greater than the Median because the data is skewed to the right. 
# Extreme outliers on the right are pulling the Mean to have a greater value.

# Q: When to use mean and median?

# A:
# Use the mean when you have normally distributed data or when you want to capture the overall average value. 
# Use the median when you have skewed data, outliers, or when you want a measure that is less affected by extreme values.




# MEASURES OF SPREAD
# Describes how spread apart or close together the data points are

# Variance - distance from each data point to the mean.
# variance - the sum of the square of each data point distance to the mean, divided by the no. of data points
# ⬆️ variance, ⬆️ spread

# Standard Deviation (SD) - square root of the variance. 2 hours (sd) is easier to understand than 4 hours^s (variance)

# Mean Absolute Deviation (MAD) - Absolute distance of each data point to the mean

# Standard Deviation vs Mean Absolute Deviation
# Similar, but not the same
# - SD squares distances, penalizes longer distances more than shorter ones
# - MAD penalizes each distance equally
# One is not better, but SD is more commonly used than MAD

# Quartiles - are a way to divide a set of numbers into four equal parts. 
# Imagine you have a group of people lined up based on their heights. 
# The first quartile (Q1) would be the height where a quarter of the people are shorter than that, and three-quarters are taller. 
# The second quartile (Q2) is the middle height, where half of the people are shorter and half are taller. 
# The third quartile (Q3) is the height where three-quarters of the people are shorter and one-quarter are taller. 
# It's like splitting the line of people into four equal sections to understand the distribution of heights.

# Quantiles - also known ar percentiles, are a generalized version of quartile
# For splitting the data into more pieces or parts

# Interquartile Range (IQR) - Difference of Q3 and Q1 (upper and lower whiskers of the box plot)
# Box plots are used for representing quartiles

# Outliers - Outliers are values that are significantly different from the other values in a dataset.
# They can be much larger or much smaller than the majority of the data points.
# Outliers stand out from the typical pattern of the data.
# They can affect the overall analysis or interpretation of the data.
# It is important to identify and consider outliers separately.



# Quartiles, Quantiles, and Quintiles
# Calculate the quartiles of the closing close
quantile(sp500$close)

# Calculate the six quantiles that split up the closing close data into 5 pieces
quantile(sp500$close, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))

# Calculate the deciles of the closing close
quantile(sp500$close, probs = seq(0, 1, 0.1))


# Variance and Standard Deviation

# Calculate variance and sd of the closing close for each year
sp500 %>% 
  group_by(year) %>% 
  summarize(var_close = var(close),
            sd_close = sd(close))

# Create subgraphs for each year: histogram of closing close
ggplot(sp500, aes(close)) +
  # Create a histogram
  geom_histogram(fill = "royalblue4") +
  # Create a separate sub-graph for each closing close
  facet_wrap(~ year)




#Finding outliers using IQR

# Calculate average closing per day: ave_closing_by_year
ave_closing_by_year <- sp500 %>%
  group_by(year) %>%
  summarize(ave_close = mean(close))

print(ave_closing_by_year)

# Compute the first and third quartiles and IQR of ave_close
q1 <- quantile(ave_closing_by_year$ave_close, 0.25)
q3 <- quantile(ave_closing_by_year$ave_close, 0.75)
iqr <- q3 - q1

print(iqr)

# Calculate the lower and upper cutoffs for outliers
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

print(lower)
print(upper)

# Filter ave_closing_by_year to find outliers
ave_closing_by_year %>%
  filter(ave_close < lower | ave_close > upper)



# Create a new column 'month_year' based on 'date'
sp500 <- mutate(sp500, month_year = format(date, "%b %Y"))

print(sp500)

# Show unique values in the "year" column of the sp500
unique_month_years <- unique(sp500$month_year)

# Print the unique values
print(unique_month_years)


# Create subgraphs for each year: histogram of closing close
ggplot(sp500, aes(close)) +
  # Create a histogram
  geom_histogram(fill = "royalblue4") +
  # Create a separate sub-graph for each closing close
  facet_wrap(~ month_year)


#Finding outliers using IQR

# Calculate average closing per day: ave_closing_by_year
ave_closing_by_month_year <- sp500 %>%
  group_by(month_year) %>%
  summarize(ave_close = mean(close))

print(ave_closing_by_month_year)

# Compute the first and third quartiles and IQR of ave_close
q1 <- quantile(ave_closing_by_month_year$ave_close, 0.25)
q3 <- quantile(ave_closing_by_month_year$ave_close, 0.75)
iqr <- q3 - q1

print(iqr)

# Calculate the lower and upper cutoffs for outliers
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

print(lower)
print(upper)

# Filter ave_closing_by_year to find outliers
ave_closing_by_month_year %>%
  filter(ave_close < lower | ave_close > upper)