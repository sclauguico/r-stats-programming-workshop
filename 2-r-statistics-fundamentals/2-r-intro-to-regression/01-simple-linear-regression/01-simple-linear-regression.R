# Install packages 
install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("magrittr")
# install.packages("ggplot2")
# install.packages("lubridate")

library(tidyverse) # for data tidying
# library(dplyr) # for tabular data manipulation
# library(magrittr)
# library(ggplot2) # for visualization
# library(lubridate) # for handling date fields

wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/2-r-statistics-fundamentals/2-r-intro-to-regression/01-simple-linear-regression")


# Linear regression is a statistical modeling technique used to understand the relationship between a dependent variable and one or more independent variables.
# It assumes a linear relationship between the dependent variable and the independent variables, meaning that the relationship can be represented by a straight line.
# The goal of linear regression is to find the best-fitting line that minimizes the difference between the predicted values and the actual values of the dependent variable.
# The line is defined by two parameters: the slope, which represents the change in the dependent variable for each unit change in the independent variable, and the intercept, which is the value of the dependent variable when the independent variable is zero.
# The fit of the linear regression model is typically evaluated using the coefficient of determination (R-squared), which measures the proportion of variance in the dependent variable that is explained by the independent variable(s).
# Linear regression can be used for prediction by plugging in values of the independent variables into the model to obtain predicted values of the dependent variable.
# It can also be used for inference, allowing us to make statistical inferences about the relationship between the independent and dependent variables and test hypotheses.
# Linear regression assumptions include linearity, independence of errors, constant variance of errors (homoscedasticity), and normally distributed errors.
# If the assumptions are violated, alternative regression techniques or model adjustments may be necessary to obtain accurate and reliable results.

boston_real_estate <- read.csv("archive/data.csv")

# Add a linear trend line without a confidence ribbon
ggplot(boston_real_estate, aes(AGE, TAX)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)

# Run a linear regression of TAX vs. AGE
lm(TAX ~ AGE, data = boston_real_estate)



# CATEGORICAL INDEPENDENT VARIABLES
boston_real_estate <- boston_real_estate %>%
  mutate(age_group = cut(AGE, breaks = seq(0, 100, 10), include.lowest = TRUE, labels = FALSE)) %>%
  mutate(age_group = factor(age_group, labels = c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100")))


# Using boston_real_estate, plot TAX
ggplot(boston_real_estate, aes(TAX)) +
  # Make it a histogram with 10 bins
  geom_histogram(bins = 10) +
  # Facet the plot so each house age group gets its own panel
  facet_wrap(vars(age_group))



summary_stats <- boston_real_estate %>% 
  # Group by age
  group_by(age_group) %>% 
  # Summarize to calculate the mean tax
  summarize(mean_by_group = mean(TAX))

# See the result
summary_stats



# Run a linear regression of tax vs. age_group
mdl_tax_vs_age <- lm(TAX ~ age_group, data = boston_real_estate)

# See the result
mdl_tax_vs_age
