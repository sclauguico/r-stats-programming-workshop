# Install packages 
install.packages("tidyverse")
# install.packages("lubridate")
# install.packages("dplyr")
# install.packages("magrittr")
# install.packages("ggplot2")

library(tidyverse) # for data tidying
# library(lubridate) # for handling date fields
# library(dplyr) # for tabular data manipulation
# library(magrittr)
# library(ggplot2) # for visualization
 
# The tidyverse is a collection of R packages that are designed to work together seamlessly. The tidyverse packages share a common design philosophy, grammar, and data structures. This makes it easy to learn and use the packages, and it also makes it easy to combine them to create powerful data analysis workflows.
 
# Here are some of the most popular tidyverse packages:
   
# ggplot2: A powerful visualization package that makes it easy to create beautiful and informative plots.
# dplyr: A powerful data manipulation package that makes it easy to wrangle and clean data.
# tidyr: A package for tidying data, which means formatting it in a way that is easy to work with.
# readr: A package for reading data from different file formats.
# purrr: A package for functional programming in R.
# tibble: A modern re-imagining of data frames in R.
# stringr: A package for working with strings in R.
# forcats: A package for working with factors in R.
# lubridate: A package for working with dates and times in R.


wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/2-r-statistics-fundamentals/1-r-intro-to-statistics/01-summary-statistics")

# What is Statistics?
# Statistics is the study of how to collect, analyze, and interpret data.

#  Data collection: The process of gathering information about a population or phenomenon.
#  Data analysis: The process of using statistical methods to summarize and understand data.
#  Data interpretation: The process of drawing conclusions from data and communicating those conclusions to others.

# Statistics is a powerful tool that can be used to answer a wide variety of questions, such as:
  
# What is the average height of American adults?
#   What is the probability of winning the lottery?
#   How does the effectiveness of a new drug compare to existing treatments?
#   Statistics is used in a wide variety of fields, including business, government, healthcare, and education. 

# It is a valuable tool for making informed decisions and understanding the world around us.




# There are two main types of statistics: descriptive statistics and inferential statistics.

# Descriptive statistics summarize data and describe its main features. 
# They are used to describe the data itself, without making any inferences about the population from which the data was 
# collected. Some common descriptive statistics include:
 
# Mean: The average of a set of data.
# Median: The middle value in a set of data when arranged in order from least to greatest.
# Mode: The most frequent value in a set of data.
# Range: The difference between the highest and lowest values in a set of data.
# Variance: A measure of how spread out the data is.
# Standard deviation: A measure of how much variation there is from the mean.


# Inferential statistics are used to make inferences about a population based on data collected from a sample. 
# They are used to test hypotheses, estimate population parameters, and make predictions. 
# Some common inferential statistical methods include:

# Hypothesis testing: A statistical method used to determine whether there is enough evidence to reject a null hypothesis.
# Estimation: A statistical method used to estimate the value of a population parameter.
# Prediction: A statistical method used to predict the value of a variable based on other variables.




# MEASURES OF CENTER (MEASURES OF CENTRAL TENDENCY)

# Measures of center: These are statistical values that represent the central tendency or average of a data set, 
# such as the mean, median, or mode.

# Normal distribution: It is a symmetric probability distribution where the majority of data points 
# cluster around the mean, resulting in a bell-shaped curve.

# Skewness: It is a measure of the asymmetry of a distribution, indicating whether the data is skewed to the left 
# (negative skewness) or to the right (positive skewness).



# Symmetric Curve:
   
# Mean: The mean is a measure of center that represents the average value of the data. 
# In a symmetric curve, the mean is equal to the median, as both values lie at the center of the distribution.

# Median: The median is the middle value in a data set when it is arranged in ascending or descending order. 
# In a symmetric curve, the median is the same as the mean, as both values are located at the center of the distribution.

# Mode: The most frequent value in a set of data.For a normal distribution, the mode is the same value as the mean and median. 
# This is because a normal distribution is symmetrical, with the highest point of the curve (the mode) at the center.



# Skewed Curve:
   
# Mean: The mean is influenced by extreme values (outliers) in a skewed curve. 
# If the curve is positively skewed (skewed to the right, data piled on left), the mean is pulled towards the higher values. 
# If the curve is negatively skewed (skewed to the left, data piled on right), the mean is pulled towards the lower values.

# Median: The median is less affected by extreme values in a skewed curve. 
# It represents the middle value, unaffected by the skewness. 
# In a positively skewed curve, the median is usually less than the mean, 
# while in a negatively skewed curve, the median is typically greater than the mean.


# Mode: The point at which the curve reaches its highest point.
# If the curve is positively skewed (skewed to the right, data piled on left), the mode is on left of median. 
# If the curve is negatively skewed (skewed to the left, data piled on right), the mode is on right of median. 



# Mean and Median
# Read the S&P 500 csv file
sp500 <- read.csv("S&P 500 Stock Prices 2014-2017.csv")

# Get the data types of each column
column_types <- sapply(sp500, class)

# Print the data types
print(column_types)

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
  filter(year %in% c("2014", "2015", "2016", "2017")) %>%
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

# Q: When to use  the mean and median?

# A:
# Use the mean when you have normally distributed data or when you want to capture the overall average value. 
# Use the median when you have skewed data, outliers, or when you want a measure that is less affected by extreme values.


# Q: Given the mean and media, what do you think the mode will be?
# A. More than the mean
# B. In between the mean and median
# C. Less than the median

# Mode 
mode(sp500$close) # Shows the data type of close


table(sp500$close) # Counts the number of times a value shows

# Print the value and frequency of the mode
mode_ind <- which.max(table(sp500$close)) # Shows the which value appeared the most time (mode), and at which column of the table
mode_ind

mode_close <- names(table(sp500$close))[which.max(table(sp500$close))]
mode_close
#: C. Less than the median because the mean is pulled at the right tail, higher values of the curve
# The mode is 34.5, found at column 4877

# How many times did mode_close appear in the data set?
mode_frequency <- max(table(sp500$close))
mode_frequency





# MEASURES OF SPREAD
# Describes how spread apart or close together the data points are

# Variance - distance from each data point to the mean.
# variance - the sum of the square of each data point distance to the mean, divided by the no. of data points
# ⬆️ variance, ⬆️ spread  

# Standard Deviation (SD) - square root of the variance. 2 hours (sd) is easier to understand than 4 hours^s (variance)

# Mean Absolute Deviation (MAD) - sum of of absolute distance of each data point to the mean, divided by the no. of data points

# Standard Deviation vs Mean Absolute Deviation
# Similar, but not the same
# - SD squares distances, penalizes longer distances more than shorter ones
# - MAD penalizes each distance equally
# One is not better, but SD is more commonly used than MAD

# Why?
# Mathematical properties: Standard deviation is a more powerful measure of variability than mean absolute deviation. This is because standard deviation is based on the squared deviations from the mean, which gives it more weight to outliers. Mean absolute deviation, on the other hand, is based on the absolute deviations from the mean, which gives it less weight to outliers.
# Interpretability: Standard deviation is easier to interpret than mean absolute deviation. This is because standard deviation is measured in the same units as the data, while mean absolute deviation is measured in the units of the data divided by the number of data points.
# Statistical theory: Standard deviation is used in many statistical theories, such as the central limit theorem and the normal distribution. Mean absolute deviation is not used as frequently in statistical theories.


# However, there are also some cases where mean absolute deviation is a better measure of variability than standard deviation. 
# For example, mean absolute deviation is less sensitive to outliers than standard deviation, so it may be a better choice 
# when there are a few outliers in the data set.


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


filtered_sp500 <- sp500 %>%
  filter(close < 150)

quantiles_close <- quantile(filtered_sp500$close)
quantiles_close

# Create boxplot
boxplot(filtered_sp500$close, main = "Boxplot of Close Values", ylab = "Close", ylim = range(filtered_sp500$close))

# Add labels to whiskers
text(1, quantiles_close[2], paste("Q1:", quantiles_close[2]), pos = 2)
text(1, quantiles_close[3], paste("Q2:", quantiles_close[3]), pos = 2)
text(1, quantiles_close[4], paste("Q3:", quantiles_close[4]), pos = 2)




# Quintile
# Calculate the six quantiles that split up the closing close data into 5 pieces
quintiles_close <- quantile(filtered_sp500$close, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))

# Create boxplot
boxplot(filtered_sp500$close, main = "Boxplot of Close Values", ylab = "Close", ylim = range(filtered_sp500$close), medcol = "red", whisklty = 2)

# Add labels to whiskers
text(1, quintiles_close[2], paste("Q1:", quintiles_close[2]), pos = 2)
text(1, quantiles_close[3], paste("Q2:", quintiles_close[3]), pos = 2)
text(1, quintiles_close[4], paste("Q3:", quintiles_close[4]), pos = 2)
text(1, quintiles_close[5], paste("Q4:", quintiles_close[5]), pos = 2)




# Calculate the deciles of the closing close
quantile(sp500$close, probs = seq(0, 1, 0.1))





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

# 2017 is the year with an average closing price above the upper whisker (outlier)


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

# Dec 2017 is an outlier with an average closing price of 106