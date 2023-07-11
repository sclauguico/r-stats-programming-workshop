# Install packages 
install.packages("dplyr")

library(dplyr) # for tabular data manipulation
library(lubridate) # for working with dates


wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/1-r-data-manipulation/2-dplyr-data-joining/01-joining-tables")

# INNER JOIN 
# In many real-world scenarios, data is spread across multiple tables or datasets. 
# Horizontal concatenation of data
# Inner join enables you to merge these tables by matching related records, creating a unified view of the data. 
# This integration is crucial for performing comprehensive analysis and gaining insights from the combined information.
# SYNTAX: tibble1 pipe-operator
#           inner_join(tibble2, by = ('tibble_1_id', 'tibble_2_id'))

# Read the order. order_detail, pizza, and pizza_detail csv files
order <- read.csv("Pizza+Place+Sales/pizza_sales/orders.csv")
order_detail <- read.csv("Pizza+Place+Sales/pizza_sales/order_details.csv")
pizza <- read.csv("Pizza+Place+Sales/pizza_sales/pizzas.csv")
pizza_type <- read.csv("Pizza+Place+Sales/pizza_sales/pizza_types.csv")

head(order)
head(order_detail)
head(pizza)
head(pizza_type)


# Add the correct verb, table, and joining column
order_and_details <- order %>% 
  inner_join(order_detail, by = c('order_id' = 'order_id'))

head(order_and_details)
View(order_and_details)


# If they have the same id names in both tables. you can simply use by = 'id_name'
ordered_pizzas <- order_and_details %>% 
  inner_join(pizza, by = "pizza_id")

View(ordered_pizzas)

# An inner_join works the same way with either table in either position. 
# The table that is specified first is arbitrary, since you will end up with the same information in the resulting table either way.

pizza_orders <- pizza %>% 
  inner_join(order_and_details, by = "pizza_id")

View(pizza_orders)

# JOINING MULTIPLE TABLES IN ONE
# Combining all of them together
ordered_pizza_types <- order %>%
  # Add order details using an inner join 
  inner_join(order_detail, by = 'order_id') %>%
  # Add pizzas using an inner join 
  inner_join(pizza, by = 'pizza_id') %>%
  # Add pizza types using an inner join
  inner_join(pizza_type, by = 'pizza_type_id') 

View(ordered_pizza_types)


# How many orders ordered a specific pizza type?
# Perform a simple aggregation
order %>%
  # Add order details using an inner join 
  inner_join(order_detail, by = 'order_id') %>%
  # Add pizzas using an inner join 
  inner_join(pizza, by = 'pizza_id') %>%
  # Add pizza types using an inner join
  inner_join(pizza_type, by = 'pizza_type_id') %>%
  # Count the number or pizza types and sort
  count(name, sort=TRUE)



# COMBINING WITH THE PREVIOUS LESSONS

glimpse(order_detail)
glimpse(pizza)

  

# When was the top 20 highest pizza sales order?
top_20_sales <- order %>%
  # Add order details using an inner join
  inner_join(order_detail, by = 'order_id') %>%
  # Add pizzas using an inner join
  inner_join(pizza, by = 'pizza_id') %>%
  # Add pizza types using an inner join
  inner_join(pizza_type, by = 'pizza_type_id') %>%
  # Don't include the ingredients
  select(-12) %>%
  # Convert quantity to numeric
  mutate(quantity = as.numeric(quantity),
         # Calculate total sales for each pizza order
         total_pizza_sales = price * quantity) %>%
  # Arrange in descending order of total sales
  arrange(desc(total_pizza_sales)) %>%
  # Select the top 20 rows
  head(20)  

View(top_20_sales)  

  
  
# When was the top 20 highest pizza sales order and what were the commonly bought pizzas?
top_20_sales_commonly_bought <- top_20_sales %>% 
  # Count the type type of pizzas ordered
  count(name) %>%
  # Arrange by count in descending order
  arrange(desc(n)) %>%
  # Merge the top_20_sales with the pizza count
  inner_join(top_20_sales, by = 'name')

View(top_20_sales_commonly_bought)