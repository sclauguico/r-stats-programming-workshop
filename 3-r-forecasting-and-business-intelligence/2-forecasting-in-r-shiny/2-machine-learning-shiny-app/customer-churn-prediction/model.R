# Importing libraries
library(RCurl) # for downloading the iris CSV file
library(randomForest)
library(caret)
library(tidyverse)


wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/3-r-forecasting-and-business-intelligence/2-forecasting-in-r-shiny/2-machine-learning-shiny-app/customer-churn-prediction")

# Load the customer churn csv file
customer_churn <- read.csv("telecom_customer_churn.csv")



ucc <- customer_churn %>%
  filter(Customer.Status %in% c("Stayed", "Churned")) %>%
  distinct(Customer.ID, .keep_all = TRUE) %>%
  select(Total.Refunds, Total.Extra.Data.Charges, Total.Long.Distance.Charges, Customer.Status) %>%
  mutate(Customer.Status = ifelse(Customer.Status == "Churned", 1, 0))

glimpse(ucc)

# Performs stratified random split of the data set
set.seed(123)  # Setting a seed for reproducibility
TrainingIndex <- createDataPartition(ucc$Customer.Status, p = 0.7, list = FALSE)
TrainingSet <- ucc[TrainingIndex,] # Training Set
TestingSet <- ucc[-TrainingIndex,] # Test Set

write.csv(TrainingSet, "training.csv", row.names = FALSE)
write.csv(TestingSet, "testing.csv", row.names = FALSE)

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

# Building Random Forest model
model <- randomForest(Customer.Status ~ ., data = TrainSet, ntree = 500, mtry = 3, importance = TRUE)

# Save model to RDS file
saveRDS(model, "model.rds")

