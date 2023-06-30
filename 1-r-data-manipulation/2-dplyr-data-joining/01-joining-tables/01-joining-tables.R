# Install packages 
install.packages("dplyr")

library(dplyr) # for tabular data manipulation


wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/1-r-data-manipulation/2-dplyr-data-joining/01-joining-tables")

# INNER JOIN 
# In many real-world scenarios, data is spread across multiple tables or datasets. 
# Inner joins enables you to merge these tables by matching related records, creating a unified view of the data. 
# This integration is crucial for performing comprehensive analysis and gaining insights from the combined information.

# Read the order. order_detail, pizza, and pizza_detail csv files
order <- read.csv("Pizza+Place+Sales/pizza_sales/orders.csv")
order_detail <- read.csv("Pizza+Place+Sales/pizza_sales/order_details.csv")
pizza <- read.csv("Pizza+Place+Sales/pizza_sales/pizzas.csv")
pizza_type <- read.csv("Pizza+Place+Sales/pizza_sales/pizza_types.csv")

glimpse(order)
glimpse(order_detail)
glimpse(pizza)
glimpse(pizza_type)


# Add the correct verb, table, and joining column
order_and_details <- order %>% 
  inner_join(order_detail, by = c('order_id' = 'order_id'))

# If they have the same id names in both tables. you can simply use by = 'id_name'

print(order_and_details)



ordered_pizzas <- order_and_details %>% 
  inner_join(pizza, by = "pizza_id")

print(ordered_pizzas)

# An inner_join works the same way with either table in either position. 
# The table that is specified first is arbitrary, since you will end up with the same information in the resulting table either way.

pizza_orders <- pizza %>% 
  inner_join(order_and_details, by = "pizza_id")

print(pizza_orders)

# JOINING MULTIPLE TABLES IN ONE

ordered_pizza_types <- order %>%
  # Add order details using an inner join 
  inner_join(order_detail, by = 'order_id') %>%
  # Add pizzas using an inner join 
  inner_join(pizza, by = 'pizza_id') %>%
  # Add pizza types using an inner join
  inner_join(pizza_type, by = 'pizza_type_id') 

print(ordered_pizza_types)


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

