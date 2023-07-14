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
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/2-r-statistics-fundamentals/1-r-intro-to-statistics/02-correlation")

# CORRELATION
# Correlation is a statistical measure that describes the relationship between two variables.
# The correlation coefficient (r) ranges between -1 and 1.
# A correlation coefficient of 1 indicates a strong positive correlation, meaning that as one variable increases, the other variable tends to increase as well.
# A correlation coefficient of -1 indicates a strong negative correlation, meaning that as one variable increases, the other variable tends to decrease.
# A correlation coefficient of 0 indicates no linear relationship between the variables.
# Correlation measures the strength and direction of the linear relationship between variables, but it does not imply causation.
# Correlation identifies the degree to which changes in one variable are associated with changes in another variable.


madrid_weather <- read.csv("Madrid+Daily+Weather+1997-2015/Madrid Daily Weather 1997-2015.csv")

View(madrid_weather)

# Check for missing values
missing_rows <- !complete.cases(madrid_weather$Mean.TemperatureC, madrid_weather$Mean.Humidity)

# Remove missing rows
clean_weather <- madrid_weather[complete.cases(madrid_weather), ]


# Create a scatterplot of mean temperature vs. mean humidity
ggplot(clean_weather, aes(Mean.TemperatureC, Mean.Humidity)) +
  # Plot the points
  geom_point()



# Create a scatterplot of mean temperature vs. mean humidity
ggplot(clean_weather, aes(Mean.TemperatureC, Mean.Humidity)) +
  geom_point() +
  # Plot a trendline
  geom_smooth(method = "lm", se = FALSE)


# Q: Based on the linear trend, what would most likely be the correlation?

# A. -0.7
# B. -0.3
# C. 0.3
# D. 0.7



# Add a linear trendline to scatterplot
ggplot(clean_weather, aes(Mean.TemperatureC, Mean.Humidity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Correlation between mean temperature vs. mean humidity
correlation <- cor(clean_weather$Mean.TemperatureC, clean_weather$Mean.Humidity)
correlation

# A: 


# CORRELATION CAVEATS

# Correlation does not imply causation: A high correlation between two variables does not necessarily imply that one variable is causing changes in the other. It only indicates a statistical association between the two variables.
# Outliers can influence correlations: Unusual or extreme observations (outliers) can have a significant impact on correlation values. It's important to investigate and potentially address outliers before drawing conclusions from correlation analyses.
# Non-linear relationships may exist: Correlation measures the strength of a linear relationship between variables. If the relationship is non-linear, the correlation coefficient may not accurately represent the association. Visualizing the data and considering other analysis techniques may be necessary.
# Correlation is sensitive to sample size: Correlation coefficients can be influenced by the size of the sample. Smaller samples may result in less reliable estimates of the true population correlation. Larger sample sizes generally provide more accurate estimates.
# Correlation does not capture all relationships: Correlation measures the strength and direction of linear relationships. It may not capture more complex relationships, such as interactions or non-linear associations. Exploring additional analysis techniques can help uncover such relationships.
# Correlation is a valuable tool for understanding associations between variables, but it should be interpreted with caution, taking into account the specific context and potential limitations of the data and analysis methods.
