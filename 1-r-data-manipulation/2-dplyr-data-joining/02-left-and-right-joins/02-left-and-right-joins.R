# Install packages 
install.packages("dplyr")

library(dplyr) # for tabular data manipulation


wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/1-r-data-manipulation/2-dplyr-data-joining/02-left-and-right-joins")

# LEFT JOIN
# A left join returns all records from the left (or first) table and the matching records from the right (or second) table.
# If there are no matches found in the right table, the result will still include all the records from the left table.
# Unmatched records from the right table will have NULL values in the result.


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
order_and_details_l <- order %>% 
  left_join(order_detail, by = c('order_id' = 'order_id'))

# Join versions to sets
order %>%
  left_join(order_detail, by ="order_id", relationship = "many-to-many") %>%
  # Filter for where quanitty is na
  filter(is.na(quantity))



# How many orders made for each type of pizza?
order_and_details_l_total_pizza_type <- order_and_details_l %>%
  group_by(pizza_id) %>%
  summarize(total_quantity = sum(quantity))

print(order_and_details_l_total_pizza_type)


# How much is each type of pizza?
pizza_l_total_price_pizza_type <- pizza %>%
  group_by(pizza_id) %>%
  summarize(unit_price = price)

print(pizza_l_total_price_pizza_type)


# How much is the total sales for each type of pizza?
total_sales <- order_and_details_l_total_pizza_type %>%
  left_join(pizza_l_total_price_pizza_type, by = "pizza_id") %>%
  mutate(total_sales = total_quantity * unit_price)

print(total_sales)

# RIGHT JOIN
# A right join returns all records from the right (or second) table and the matching records from the left (or first) table.
# If there are no matches found in the left table, the result will still include all the records from the right table.
# Unmatched records from the left table will have NULL values in the result.

# Perform a right join
pizza_type_r <- pizza %>%
  # Right join pizza_type
  right_join(pizza_type, by = "pizza_type_id")

print(pizza_type_r)



pizza_type_count_r <- pizza %>%
  # Count the pizza_type_id
  count(pizza_type_id) %>%
  # Right join pizza_type
  right_join(pizza_type, by = "pizza_type_id") %>%
  # Filter for NA
  filter(is.na(n))

print(pizza_type_count_r)


# Create a replace_na function that takes in the data and the value for replacing the na
replace_na <- function(data, replacements) {
  for (column in names(replacements)) {
    data <- data %>% mutate(!!sym(column) := ifelse(is.na(!!sym(column)), replacements[[column]], !!sym(column)))
  }
  return(data)
}



pizza_type_count_r_replaced_na <- pizza %>%
  # Count the pizza_type_id
  count(pizza_type_id) %>%
  # Right join pizza_type
  right_join(pizza_type, by = "pizza_type_id") %>%
  # Use replace_na to replace missing values in the n column
  replace_na(list(n = 0))

print(pizza_type_count_r_replaced_na)


# Left and right joins are useful when you want to include all the records from one table, 
# regardless of whether there is a match in the other table. They allow you to combine data and 
# maintain the integrity of both tables while handling missing or unmatched records.

# It's important to note that the choice between left and right joins depends on the specific scenario 
# and the desired outcome. You would typically select the table that contains the essential records or 
# acts as the primary source of data as the preserved table in the join operation.
