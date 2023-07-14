# Install packages 
install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("magrittr")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("broom")

library(tidyverse) # for data tidying
# library(dplyr) # for tabular data manipulation
# library(magrittr)
# library(ggplot2) # for visualization
# library(lubridate) # for handling date fields
library(broom) # for showing results into dataframes

wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/2-r-statistics-fundamentals/2-r-intro-to-regression/02-predictions-and-model-objects")


boston_real_estate <- read.csv("archive/data.csv")

View(boston_real_estate_clean)

# Remove rows with missing values
boston_real_estate_clean <- boston_real_estate[complete.cases(boston_real_estate), ]

# Add a linear trend line without a confidence ribbon
ggplot(boston_real_estate_clean, aes(DIS, NOX)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)


# MAKING PREDICTIONS

# Create a tibble with index of accessibility to DIS column from 2 to -2
# This is also known as the test data 
explanatory_data <- tibble(
  DIS = seq(0, 2.5, by = 0.1)
)


# Create the linear model
mdl_NOX_vs_DIS <- lm(NOX ~ DIS, data = boston_real_estate_clean)

# Make a prediction of NOX using the model, mdl_NOX_vs_DIS with the new data from explanatory_data

prediction_data <- explanatory_data %>% 
  mutate(
    NOX = predict(mdl_NOX_vs_DIS, explanatory_data)
  )

# See the result
prediction_data



# Plot the predictIon
ggplot(boston_real_estate_clean, aes(DIS, NOX)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # Add a point layer of prediction data, colored yellow
  geom_point(data = prediction_data, color = "yellow")





# MODEL OBJECTS

# Obtain the vector of coefficients and intercept
coefficients(mdl_NOX_vs_DIS)

# Obtain the predictions on the original data set
fitted(mdl_NOX_vs_DIS)

# Obtain the actual - predicted, ⬇️ residuals, ⬆️ model
residuals(mdl_NOX_vs_DIS)

# Obtain the info summary of the model object
summary(mdl_NOX_vs_DIS)

# A good fit means it follows a normal distribution
# Conditions for good fit:
  # Median is close to 0
  # Q1 and Q3 are almost equal in terms of their absolute value, IQR ~ 0


# broom
# Get the coefficient-level elements of the model
# Info for understanding the significance and strength of the relationships between the predictors and the response variable.
tidy(mdl_NOX_vs_DIS)

# Get the observation-level elements of the model
# info for analyzing individual observations, detecting outliers, and evaluating model fit
augment(mdl_NOX_vs_DIS)

# Get the model-level elements of the model
# Info for assessing the overall performance and fit of the model
glance(mdl_NOX_vs_DIS)




#MANUAL PREDICTION

# Get the coefficients of mdl_NOX_vs_DIS
coeffs <- coefficients(mdl_NOX_vs_DIS)
coeffs

# Get the intercept
intercept <- coeffs[1]
intercept

# Get the slope
slope <- coeffs[2]
slope


manual_pred <- explanatory_data %>% 
  mutate(
    # Manually calculate the predictions
    NOX = intercept + slope * DIS
  )

View(manual_pred)

# Compare to the results from the not manual predict()
auto_pred <- predict(mdl_NOX_vs_DIS, explanatory_data)

View(auto_pred)







# TRASNFORMING VARIABLES IF THE RELATIONSHIP IS NOT A STRAIHT LINE
# Plot the original data for comparison
ggplot(boston_real_estate_clean, aes(DIS, NOX)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# Transform DIS by getting the square root to make the plot more linear
# SQRT transformation is good for right skewed data, to make the data more distributed
ggplot(boston_real_estate_clean, aes(sqrt(DIS), NOX)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# Make a linear regression model of NOX vs. square root of DIS using boston_real_estate_clean
mdl_NOX_vs_DIS_sqrt <- lm(
  NOX ~ sqrt(DIS), 
  data = boston_real_estate_clean
)

# Get the model summary
summary(mdl_NOX_vs_DIS_sqrt)


explanatory_data <- tibble(
  DIS = seq(0, 2.5, by = 0.1)
)

# Use mdl_NOX_vs_DIS_sqrt to predict explanatory_data_sq
prediction_data <- explanatory_data %>% 
  mutate(
    NOX = predict(mdl_NOX_vs_DIS_sqrt, explanatory_data)
)

# See the result
prediction_data



# Plot the prediction with the transformed independent variable
ggplot(boston_real_estate_clean, aes(sqrt(DIS), NOX)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # Add points from prediction_data, colored green, size 3
  geom_point(data = prediction_data, color = "green", size = 3)



# Plot the predition with the original non-transformed independent variable
ggplot(boston_real_estate_clean, aes((DIS), NOX)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # Add points from prediction_data, colored vioelt, size 3
  geom_point(data = prediction_data, color = "violet", size = 3)
