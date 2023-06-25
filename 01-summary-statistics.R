
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

# Function to calculate the mode
calculate_mode <- function(x) {
  freq_table <- table(x)
  mode <- as.numeric(names(freq_table)[freq_table == max(freq_table)])
  return(mode)
}

# Calculate the mode of medium_price$price
mode_value <- calculate_mode(medium_price$price)

# Print the mode
print(mode_value)