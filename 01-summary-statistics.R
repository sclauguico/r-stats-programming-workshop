
# Install packages 
install.packages("dplyr") 
install.packages("tidyverse")
install.packages("ggplot2")

library(tidyverse) # for data tidying
#library(dplyr) # for tabular data manipulation

library(ggplot2) # for visualization

getwd()
setwd("C:/Users/sclau/Documents/stats-with-R")

# Read the pizzas csv file
pizzas <- read.csv("Pizza+Place+Sales/pizza_sales/pizzas.csv")

# Mean and Median

# Filter for small
small_price <- pizzas %>%
  filter(size == 'S')

# Filter for medium
medium_price <- pizzas %>%
  filter(size == 'M')

# Filter for medium
large_price <- pizzas %>%
  filter(size == 'L')

# Calculate mean and median of price for small pizza
mean(small_price$price)
median(small_price$price)

# Calculate mean and median of price for medium pizza
mean(medium_price$price)
median(medium_price$price)

# Calculate mean and  median of price for large pizza
mean(large_price$price)
median(large_price$price)

pizzas %>%
  # Filter for small, medium, large pizzas
  filter(size %in% c("S", "M","L")) %>%
  # Group by size
  group_by(size) %>%
  # Get mean_price and median_price
  summarize(mean_price = mean(price),
            median_price = median(price))

# Mean vs Median

# Read the S&P 500 csv file
sp500 <- read.csv("S&P+500+Stock+Prices+2014-2017.csv/S&P 500 Stock Prices 2014-2017.csv")

sp500 %>%
  # Create histogram of price
  ggplot(aes(close)) +
  geom_histogram()

# Q: How do you thing the mean is compared to the median with the histogram?

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


