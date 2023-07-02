# Install packages 
install.packages("tidyverse")
install.packages("dplyr")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("broom")


library(tidyverse) # for data tidying
library(dplyr) # for tabular data manipulation
library(magrittr)
library(ggplot2) # for visualization
library(lubridate) # for handling date fields
library(broom)

wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/2-r-statistics-fundamentals/2-r-intro-to-regression/02-predictions-and-model-objects")


boston_real_estate <- read.csv("archive/data.csv")

glimpse(boston_real_estate)

# Create a tibble with index of accessibility to AGEial highways column from zero to 12
explanatory_data <- tibble(
  AGE = 0:100
)


mdl_TAX_vs_AGE <- lm(TAX ~ AGE + 0, data = boston_real_estate)

# Edit this, so predictions are stored in prediction_data
prediction_data <- explanatory_data %>% 
  mutate(
    TAX = predict(mdl_TAX_vs_AGE, explanatory_data)
  )

# See the result
prediction_data



# Add to the plot
ggplot(boston_real_estate, aes(AGE, TAX)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # Add a point layer of prediction data, colored yellow
  geom_point(data = prediction_data, color = "yellow")


coefficients(mdl_TAX_vs_AGE)

fitted(mdl_TAX_vs_AGE)

residuals(mdl_TAX_vs_AGE)

summary(mdl_TAX_vs_AGE)


#MANUAL PREDICTION

# Get the coefficients of mdl_TAX_vs_AGE
coeffs <- coefficients(mdl_TAX_vs_AGE)

# Get the intercept
intercept <- coeffs[1]

# Get the slope
slope <- coeffs[2]

explanatory_data %>% 
  mutate(
    # Manually calculate the predictions
    TAX = intercept + slope * AGE
  )

# Compare to the results from predict()
predict(mdl_TAX_vs_AGE, explanatory_data)


# Get the coefficient-level elements of the model
tidy(mdl_TAX_vs_AGE)

# Get the observation-level elements of the model
augment(mdl_TAX_vs_AGE)

# Get the model-level elements of the model
glance(mdl_TAX_vs_AGE)




ggplot(boston_real_estate, aes(RAD, TAX)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Run the code to see the plot
# Edit so x-axis is square root of RAD
ggplot(boston_real_estate, aes(sqrt(RAD), TAX)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# Run a linear regression of TAX vs. square root of RAD using boston_real_estate
mdl_TAX_vs_RAD <- lm(
  TAX ~ sqrt(RAD), 
  data = boston_real_estate
)

# See the result
mdl_TAX_vs_RAD



# Run a linear regression of TAX vs. square root of RAD using boston_real_estate
mdl_TAX_vs_RAD <- lm(
  TAX ~ sqrt(RAD), 
  data = boston_real_estate
)

explanatory_data <- tibble(
  RAD = seq(0, 80, 10) ^ 2
)

# Use mdl_TAX_vs_RAD to predict explanatory_data
prediction_data <- explanatory_data %>% 
  mutate(
    TAX = predict(mdl_TAX_vs_RAD, explanatory_data)
  )

# See the result
prediction_data





mdl_TAX_vs_RAD <- lm(
  TAX ~ sqrt(RAD), 
  data = boston_real_estate
)

explanatory_data <- tibble(
  RAD = seq(0, 20, 10) ^ 2
)

# Use mdl_TAX_vs_RAD to predict explanatory_data
prediction_data <- explanatory_data %>% 
  mutate(
    TAX = predict(mdl_TAX_vs_RAD, explanatory_data)
  )



ggplot(boston_real_estate, aes(sqrt(RAD), TAX)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # Add points from prediction_data, colored green, size 5
  geom_point(data = prediction_data, color = "green", size = 5)
