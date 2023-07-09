# Install packages 
install.packages("dplyr")

library(dplyr) # for tabular data manipulation


wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/1-r-data-manipulation/2-dplyr-data-joining/02-left-and-right-joins")

# LEFT JOIN
# A left join returns all records from the left (or first) table and the matching records from the right (or second) table.
# If there are no matches found in the right table, the result will still include all the records from the left table.
# Unmatched records from the right table will have NULL values in the result.
# SYNTAX: tibble1 pipe-operator
#           left_join(tibble2, by = ('tibble_1_id', 'tibble_2_id'))

# Read the order. order_detail, pizza, and pizza_detail csv files
order <- read.csv("Pizza+Place+Sales/pizza_sales/orders.csv")
order_detail <- read.csv("Pizza+Place+Sales/pizza_sales/order_details.csv")
pizza <- read.csv("Pizza+Place+Sales/pizza_sales/pizzas.csv")
pizza_type <- read.csv("Pizza+Place+Sales/pizza_sales/pizza_types.csv")

glimpse(order)
glimpse(order_detail)
glimpse(pizza)
glimpse(pizza_type)

# Compare with the result from order_and_details
order_and_details <- order %>% 
  inner_join(order_detail, by ='order_id')

View(order_and_details)


# Add the verb, table, and left joining the tables
order_and_details_l <- order %>% 
  left_join(order_detail, by = ('order_id'))

View(order_and_details_l)
# Notice how order_and_details_l have order_id, where it's not present on order_and_details
# Notice also how order_id 2 has values from order but columns from order_detail are null
# This is because we cannot match all the data from the right to the left



# Join order_detail to order
order %>%
  left_join(order_detail, by ="order_id", relationship = "many-to-many") %>%
  # Filter for where quantity is na to show the sample that cannot be matched
  filter(is.na(quantity))



# How many orders made for each date?
order_and_details_l_date_count <- order_and_details_l %>%
  group_by(date) %>%
  summarize(count = n())

head(order_and_details_l_date_count)

# Why not use order_and_details (inner join result)?
# The result from order_and_details_l_date_count can show all orders by date (these two columns are present on the left table)
# since it contains all order_id and date info despite the lack of other order details from order_detail 

# COMPARE WITH INNER JOIN
order_and_details_date_count <- order_and_details %>%
  group_by(date) %>%
  summarize(count = n())

head(order_and_details_date_count)

# Notice how January 1 lacks 1 order count compared to order_and_details_l_date_count



# How much is each type of pizza?
pizza_l_total_sales_by_pizza <- order_and_details_l %>%
  left_join(pizza, by = 'pizza_id') %>%
  left_join(pizza_type, by = 'pizza_type_id') %>%
  group_by(name) %>%
  mutate(quantity = as.numeric(quantity),
         total_pizza_sales = price * quantity) %>%
  summarize(total_pizza_sales = sum(total_pizza_sales))

head(pizza_l_total_sales_by_pizza)


# COMPARE WITH INNER JOIN
pizza_total_sales_by_pizza <- order_and_details %>%
  inner_join(pizza, by = 'pizza_id') %>%
  inner_join(pizza_type, by = 'pizza_type_id') %>%
  group_by(name) %>%
  mutate(quantity = as.numeric(quantity),
         total_pizza_sales = price * quantity) %>%
  summarize(total_pizza_sales = sum(total_pizza_sales))

head(pizza_total_sales_by_pizza)

# Why are the results equal? 
# Because the retrieved data like price and quantity are on the right tibble
# This means that the total_pizza_sales from order_id 2 were not computed (even for LEFT JOIN because they are NULL)




# RIGHT JOIN
# A right join returns all records from the right (or second) table and the matching records from the left (or first) table.
# If there are no matches found in the left table, the result will still include all the records from the right table.
# Unmatched records from the left table will have NULL values in the result.
# SYNTAX: tibble1 pipe-operator
#           right_join(tibble2, by = ('tibble_1_id', 'tibble_2_id'))

# Add the verb, table, and right joining the tables
order_and_details_r <- order %>% 
  right_join(order_detail, by = c('order_id' = 'order_id'))

View(order_and_details_r)


# How many orders made for each date?
order_and_details_r_date_count <- order_and_details_r %>%
  group_by(date) %>%
  summarize(count = n())

head(order_and_details_r_date_count)

# Notice how January 1 lacks 1 order count compared to order_and_details_l_date_count, it is same with order_and_details_date_count
# Why is that?
# The right tibble does not have order_id 2 so it only gets order_ids 1, 3, 4, 5, 6, ... just like the result from the inner join

# What is the difference between the results of the right join and inner join?

# How much is each type of pizza?
pizza_r_total_sales_by_pizza <- order_and_details_r %>%
  right_join(pizza, by = 'pizza_id') %>%
  right_join(pizza_type, by = 'pizza_type_id') %>%
  group_by(name) %>%
  mutate(quantity = as.numeric(quantity),
         total_pizza_sales = price * quantity) %>%
  summarize(total_pizza_sales = sum(total_pizza_sales))

head(pizza_r_total_sales_by_pizza)
View(pizza_r_total_sales_by_pizza)

# Why are the The Big Meat Pizza, The Chicken Pesto Pizza, 
# The Five Cheese Pizza, The Four Cheese Pizza, The Thai Chicken Pizza are null?

order_and_details_pizza_r <- order_and_details_r %>%
  right_join(pizza, by = 'pizza_id')
View(order_and_details_pizza_r)

order_and_details_pizza_type_r <- order_and_details_pizza_r %>%
  right_join(pizza_type, by = 'pizza_type_id')

View(order_and_details_pizza_type_r)


# Look at null order_id and order_details_id since this column for joining
filtered_r_na <- order_and_details_pizza_type_r %>%
  filter(is.na(order_id), is.na(order_details_id))

View(filtered_r_na)

# The Big Meat Pizza, The Chicken Pesto Pizza, 
# The Five Cheese Pizza, The Four Cheese Pizza, The Thai Chicken Pizza have both null
# order_id and order_detail_id




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

View(pizza_type_count_r)


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

View(pizza_type_count_r_replaced_na)



# Left and right joins are useful when you want to include all the records from one table, 
# regardless of whether there is a match in the other table. They allow you to combine data and 
# maintain the integrity of both tables while handling missing or unmatched records.

# It's important to note that the choice between left and right joins depends on the specific scenario 
# and the desired outcome. You would typically select the table that contains the essential records or 
# acts as the primary source of data as the preserved table in the join operation.
