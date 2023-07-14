library(dplyr) #for data manipulation
library(astsa) # for forecasting
library(fpp2) # for forecasting viz

# The astsa library is an R package that provides functions for the analysis of time series data. It includes functions for:

# Describing time series data
# Fitting ARIMA models
# Forecasting time series data
# Testing for stationarity
# Analyzing seasonal data
# Comparing different time series models


# The fpp2 library is built on top of the astsa library and provides a more user-friendly 
# interface for many of the functions in astsa. It also includes a number of additional features, such as:

# Automatic model selection
# Forecast evaluation
# Visualization tools

wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/3-r-forecasting-and-business-intelligence/2-forecasting-in-r-shiny/1-stock-price-forecasting-r-shiny")


# plot the trend of HSBC stock
# 1. read the HSBC.csv file into a data frame called `stock`
stock <- read.csv("HSBC.csv")

# 2. print the first few rows of the data frame
# head(stock)

# 3. select the "Date" and "Close" columns from the `stock` data frame
stock <- stock[c("Date", "Close")]

# 4. convert the "Date" column to a Date object
stock$Date <- as.Date(stock$Date)

# 5. evaluate the `plot()` function within the context of the `stock` data frame
with(data = stock, 
     plot(Date, Close, type = "l", 
          main = "HSBC Stock",
          xlab = "12 Months",
          ylab = "$ Close"))


# Forecast the next 10 days closing prices with ARIMA

# ARIMA stands for Autoregressive Integrated Moving Average. 
# It is a statistical model that is used to forecast time series data. 
# ARIMA models are often used to forecast financial data, such as stock prices.

# Here are some of the advantages of using ARIMA models:
#   
# They can be used to forecast time series data with a high degree of accuracy.
# They are relatively easy to understand and interpret.
# They can be used to forecast a variety of time series data, including financial data, economic data, and weather data.
# Here are some of the disadvantages of using ARIMA models:

# They can be sensitive to the choice of parameters.
# They can be computationally expensive to fit.
# They can be difficult to interpret if the time series data is not stationary.

# 6. fit an ARIMA model to the `Close` column of the `stock` data frame
arima.hsbc <- auto.arima(stock["Close"])

# 7. forecast the next 10 days of prices based on the ARIMA model
forecast(arima.hsbc, h = 10)

# 8. plot the time series of the `Close` column of the `stock` data frame
autoplot(ts(stock["Close"]))

# Forecast with SARIMA

# SARIMA is an extension of ARIMA that includes seasonal components.

# SARIMA stands for Seasonal Autoregressive Integrated Moving Average. 
# It is a statistical model that is used to forecast time series data that has seasonal patterns. 
# SARIMA models are often used to forecast financial data, such as stock prices, and weather data.

# Here are some of the advantages of using SARIMA models:
   
# They can be used to forecast time series data with a high degree of accuracy.
# They can be used to forecast time series data that has seasonal patterns.
# They are relatively easy to understand and interpret.
# Here are some of the disadvantages of using SARIMA models:

# They can be sensitive to the choice of parameters.
# They can be computationally expensive to fit.
# They can be difficult to interpret if the time series data is not stationary.

# 9. fit a SARIMA model to the `Close` column of the `stock` data frame
sarima(ts(stock["Close"]), 0, 1, 0)

# 0 autoregressive terms. This means that the model does not use past values of the time series to predict future values.
# 1 difference. This means that the time series is differenced once to remove the trend.
# 0 moving average terms. This means that the model does not use past errors to predict future values.

# 10. forecast the next 10 days of prices based on the SARIMA model
sarima.for(ts(stock["Close"]), 10, 0, 1, 0)

# 11. print the length of the `Close` column of the `stock` data frame
length(stock["Close"])

# 12. print the dimensions of the `stock` data frame
dim(stock)[1]
