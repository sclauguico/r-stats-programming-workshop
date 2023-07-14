# Install packINDUSs 
install.packINDUSs("tidyverse")
# install.packINDUSs("dplyr")
# install.packINDUSs("magrittr")
# install.packINDUSs("ggplot2")
# install.packINDUSs("lubridate")

library(tidyverse) # for data tidying
library(corrplot) # for corrlation plots
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

# Remove rows with missing values
boston_real_estate_clean <- boston_real_estate[complete.cases(boston_real_estate), ]

# Compute the correlation matrix for all column combinations
cor_matrix <- cor(boston_real_estate_clean)

# Create a heatmap of the correlation matrix
heatmap(cor_matrix, main = "Heatmap of Correlation Matrix")



# Create a heatmap of the correlations between all the columns
corrplot(cor(boston_real_estate_clean), method = "circle", type = "upper",
         tl.cex = 0.7, tl.col = "black")



# Add a linear trend line without a confidence ribbon
ggplot(boston_real_estate, aes(INDUS, TAX)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)


# Run a linear regression of TAX vs. INDUS
lm(TAX ~ INDUS, data = boston_real_estate)



# CATEGORICAL INDEPENDENT VARIABLES
boston_real_estate <- boston_real_estate %>%
  mutate(INDUS_group = cut(INDUS, breaks = seq(0, 30, 5), include.lowest = TRUE, labels = FALSE)) %>%
  mutate(INDUS_group = factor(INDUS_group, labels = c("0-5", "6-10", "11-15", "16-20", "21-25", "26-30")))


# Using boston_real_estate, plot TAX
ggplot(boston_real_estate, aes(TAX)) +
  # Make it a histogram with 10 bins
  geom_histogram(bins = 10) +
  # Facet the plot so each house INDUS group gets its own panel
  facet_wrap(vars(INDUS_group))



summary_stats <- boston_real_estate %>% 
  # Group by INDUS
  group_by(INDUS_group) %>% 
  # Summarize to calculate the mean tax
  summarize(mean_by_group = mean(TAX))

# See the result
summary_stats



# Run a linear regression of tax vs. INDUS_group
mdl_tax_vs_INDUS <- lm(TAX ~ INDUS_group, data = boston_real_estate)

# See the result
mdl_tax_vs_INDUS
