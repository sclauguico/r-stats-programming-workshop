# Install packages 
install.packages("tidyverse")
install.packages("dplyr")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("lubridate")


library(tidyverse) # for data tidying
library(dplyr) # for tabular data manipulation
library(magrittr)
library(ggplot2) # for visualization
library(lubridate) # for handling date fields


wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/2-r-statistics-fundamentals/1-r-intro-to-statistics/02-correlation-and-experimental-design")

# Correlation is a statistical measure that describes the relationship between two variables.
# The correlation coefficient (r) ranges between -1 and 1.
# A correlation coefficient of 1 indicates a strong positive correlation, meaning that as one variable increases, the other variable tends to increase as well.
# A correlation coefficient of -1 indicates a strong negative correlation, meaning that as one variable increases, the other variable tends to decrease.
# A correlation coefficient of 0 indicates no linear relationship between the variables.
# Correlation measures the strength and direction of the linear relationship between variables, but it does not imply causation.
# Correlation identifies the degree to which changes in one variable are associated with changes in another variable.