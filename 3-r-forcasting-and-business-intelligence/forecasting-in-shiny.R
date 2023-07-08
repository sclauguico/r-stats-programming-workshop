install.packages(c("tidyverse", "dplyr", "magrittr", "ggplot2", "lubrishippedDate", "broom", "tidymodels", "modeltime", "modeltime.ensemble", "modeltime.resample", "timetk"))
library(tidyverse)
library(dplyr)
library(magrittr)
library(ggplot2)
library(lubrishippedDate)
library(broom)
library(tidymodels)
library(timetk)
library(modeltime)
library(modeltime.ensemble)
library(modeltime.resample)


wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/3-r-forcasting-and-business-intelligence/")


northwind_traders_orders <- read.csv("Northwind+Traders/Northwind Traders/orders.csv")
northwind_traders_order_details <- read.csv("Northwind+Traders/Northwind Traders/order_details.csv")

glimpse(northwind_traders_orders)
glimpse(northwind_traders_order_details)

dim(northwind_traders_orders)
dim(northwind_traders_order_details)

northwind_traders <- northwind_traders_orders %>% 
  left_join(northwind_traders_order_details, by = c('orderID'))

glimpse(northwind_traders)

head(northwind_traders)
dim(northwind_traders)

northwind_traders <- northwind_traders %>%
  mutate(total_order_sales = unitPrice * quantity)

# northwind_traders <- mutate(northwind_traders, shippedDate = as.shippedDate("ordershippedDate", format = "%d/%m/%Y"))
# northwind_traders <- mutate(northwind_traders, shippedDate = as.shippedDate("requiredshippedDate", format = "%d/%m/%Y"))
# northwind_traders <- mutate(northwind_traders, shippedDate = as.shippedDate("shippedDate", format = "%d/%m/%Y"))

northwind_traders <- northwind_traders %>%
  mutate(
    shippedDate = ymd(shippedDate)
  )

# northwind_traders_my <- northwind_traders %>%
#   mutate(
#     month_year = shippedDate %>%
#       strftime("%b-%Y")
#   ) %>%
#   group_by(month_year)


head(northwind_traders)

northwind_traders %>%
  group_by(shipperID) %>%
  plot_time_series(shippedDate, total_order_sales,
                   .smooth = FALSE,
                   .facet_ncol = 3,
                   .interactive = FALSE
  )


FORECAST_HORIZON <- 30

northwind_traders <- northwind_traders %>%
  mutate(
    shipperID = as.character(shipperID)
  )

full_data_tbl <- northwind_traders %>%
  select(shipperID, shippedDate, total_order_sales) %>%
  group_by(shipperID) %>%
  future_frame (
    .shippedDate_var = shippedDate,
    .length_out = FORECAST_HORIZON,
    .bind_data = TRUE
  ) %>%
  ungroup() %>%
  mutate(shipperID = fct_drop(shipperID))

# Training Data
data_prepared_tbl <- full_data_tbl %>%
  filter(!is.na(total_order_sales))


data_prepared_tbl %>%
  group_by(shipperID) %>%
  summarize(
    mean = mean(total_order_sales),
    sd = sd(total_order_sales),
    min = min(total_order_sales),
    max = max(total_order_sales),
    n = n(),
    missing = sum(is.na(total_order_sales))
  )


future_tbl <- full_data_tbl %>%
  filter(is.na(total_order_sales))

# Panel Data Splitting
splits <- data_prepared_tbl %>%
  time_series_split(
    shippedDate_var = shippedDate,
    assess = FORECAST_HORIZON,
    cumulative = TRUE
  )


315 / 30

1840




# 5.0 PREPROCESSOR

library(tidymodels)

# Create recipe specification
recipe_spec_1 <- recipe(total_order_sales ~ ., data = training(splits)) %>%
  step_timeseries_signature(shippedDate) %>%
  step_rm(matches("(.iso$)|(.xts$)|(day)|(hour)|(minute)|(second)|(am.pm)")) %>%
  step_normalize(shippedDate_index.num, shippedDate_year) %>%
  step_mutate(shippedDate_week = factor(shippedDate_week, ordered = TRUE)) %>%
  step_dummy(all_nominal(), one_hot = TRUE)

# Prepare and summarize the recipe
recipe_spec_1 %>%
  prep() %>%
  juice() %>%
  glimpse()

# UpshippedDate the role of the "shippedDate" variable
recipe_spec_2 <- recipe_spec_1 %>%
  update_role("shippedDate", new_role = "ID")

recipe_spec_1 %>%
  prep() %>%
  summary()

# Prepare and summarize the upshippedDated recipe
recipe_spec_2 %>%
  prep() %>%
  summary()


#6.0 MODELS---
#Prophet w/ Regressors 

# Fit Prophet model
wflw_fit_prophet <- workflow() %>%
  add_model(prophet_reg() %>% set_engine("prophet")) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))

# Fit XGBoost model
wflw_fit_xgboost <- workflow() %>%
  add_model(boost_tree(mode = "regression") %>% set_engine("xgboost")) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(splits))

library(tidymodels)

# Install 'ranger' package
install.packages("ranger")


# Fit Random Forest model
wflw_fit_rf <- workflow() %>%
  add_model(rand_forest(mode = "regression") %>% set_engine("ranger")) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(splits))

library(tidymodels)
library(kernlab)

# Fit SVM model with RBF kernel
wflw_fit_svm <- workflow() %>%
  add_model(svm_rbf(mode = "regression") %>% set_engine("kernlab")) %>%
  add_recipe(recipe_spec_2) %>%
  fit(training(splits))


# Fit Prophet Boost model
wflw_fit_prophet_boost <- workflow() %>%
  add_model(prophet_boost(
    seasonality_daily = FALSE,
    seasonality_weekly = FALSE,
    seasonality_yearly = FALSE
  ) %>% set_engine("prophet_xgboost")) %>%
  add_recipe(recipe_spec_1) %>%
  fit(training(splits))


# 7.0 MODELTIME Table 

submodels_tbl <- modeltime_table (
  wflw_fit_prophet,
  wflw_fit_xgboost,
  wflw_fit_rf,
  wflw_fit_svm,
  wflw_fit_prophet_boost
)

#
