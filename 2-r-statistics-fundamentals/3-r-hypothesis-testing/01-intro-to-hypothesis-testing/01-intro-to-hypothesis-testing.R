install.packages("tidyverse")


library(tidyverse)

wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/2-r-statistics-fundamentals/3-r-hypothesis-testing/01-intro-to-hypothesis-testing")

# Load the customer churn csv file
customer_churn <- read.csv("telecom_customer_churn.csv")

# View the customer_churn dataset
view(customer_churn)
glimpse(customer_churn)

# Problem: The CEO of a telecommunication company had an initial guess that the churn rate among 
# their customers was around 20% based on a sample data. However, being uncertain about the accuracy of this assumption, 
# the CEO decided to seek the help of a skilled Data Scientist.

# Calculate a point estimate (sample statistic). 
# For this analysis, select the proportion of customer churn.
# Percentage of those who churned of the total sample
churned_prop_samp <- customer_churn %>%
  summarize(prop_churned_customers = mean(Customer.Status == "Churned")) %>%
  # Extract the value from the dataframe
  pull(prop_churned_customers)


# Print the results, the sample statistic churned_prop_samp.
print(churned_prop_samp)


# Calculating a z-score

# Why Z-Score?
# It would be wrong if a hypothesis test gave a different answer 
# if your variables were in Euros instead of US dollars. 
# Standardization avoids that.

# One standardized value of interest in a hypothesis test is called a z-score. 
# To calculate it, you need three numbers: 
  # 1. The sample statistic (point estimate). In this study, that is the churned_prop_samp
  # 2. A hypothesized statistic. In this study, you will hypothesize a certain value called churned_prop_hyp
  # 3. The standard error of the statistic (which you will estimate from the bootstrap distribution).
 
# Z-scores can be used to normalize data, which means to convert it to a standard scale. 
# This is useful for things like machine learning, where the data needs to be on a similar
# scale for the algorithms to work properly.

# Let's make a hypothesis
# Hypothesize that the proportion is 20%
churned_prop_hyp <- 0.20


# To calculate the Z-Score, bootstrap resampling is an important step. 
# You can obtain the 3rd number by calculating for the standard deviation of the bootrstrap sample


# Bootstrap sampling is a resampling technique that involves drawing samples from a given dataset 
# with replacement to estimate the sampling distribution and make inferences about the population.
# By simulating multiple resamples from the original data, it allows for more robust inference 
# and confidence interval estimation

# Bootstrap sampling helps address the idea of the dataset being a random sample from a larger population 
# by simulating multiple resamples from the observed data, allowing for more accurate estimation of sampling 
# variability and capturing the inherent randomness in the dataset.

# Generate 1 bootstrap resample
customer_churn_resample_1 <- customer_churn %>%
  slice_sample(prop = churned_prop_samp, replace = TRUE)

# Print the result
View(customer_churn_resample_1)


# Calculate mean churn prop of resample
mean_churned_1 <- customer_churn_resample_1 %>% 
  summarize(mean_danceability = mean(Customer.Status == "Churned")) %>% 
  pull(mean_danceability)

# Print the result
print(mean_churned_1)


# 1 resample is not enough. Let's have 1000 resamples and 
# compute the Churned proportion for every sample (mean of Churned Customers)


# Replicate this 1000 times
mean_churned_1000 <- replicate(
  n = 1000,
  expr = {
    # Resample the data
    customer_churn_resample <- customer_churn %>% 
      slice_sample(prop = 1, replace = TRUE) %>% 
      # Calculate the summary stat
      summarize(mean_churned = mean(Customer.Status == "Churned")) %>% 
      pull(mean_churned)
  }
)

# Print the result
print(mean_churned_1000)


# Store the resamples in a tibble
customer_churn_boot_distn <- tibble(
  resample_mean = mean_churned_1000
)

print(customer_churn_boot_distn)

# Plot a histogram  of the resample means with binwidth 0.01
customer_churn_boot_distn %>% 
  ggplot(aes(x = resample_mean)) +
  geom_histogram(binwidth = 0.002, fill = "royalblue4") +
  labs(x = "Mean Churned", y = "Frequency", title = "Distribution of Resampled Mean Churned")


View(customer_churn_boot_distn)


# Now you can obtain no. 3 requirement for the z_score
# Calculate the standard error, it is the standard deviation of the bootstrap distribution
std_error <- customer_churn_boot_distn %>% 
  summarize(sd_churned_prop = sd(resample_mean)) %>% 
  pull(sd_churned_prop)

std_error

# Find z-score of churned_prop_samp
z_score <- (churned_prop_samp - churned_prop_hyp) / std_error

# Print the results
z_score # Is this high or low?
# The z-score is a standardized measure of the difference between the sample statistic and the hypothesized statistic.

# In order to determine whether to choose the null hypothesis or the alternative hypothesis, 
# you need to calculate a p-value from the z-score.

# Let's return to the customer churn dataset and the proportion of customer churn.

# H0: The proportion of customer churn is 20 percent.
# Ha: The proportion of customer churn is greater than 20 percent.

# The observed sample statistic, churned_prop_samp, 
# The null hypothesis statistic, churned_prop_hyp (20%), 
# and the bootstrap standard error, std_error are available.

# Calculate the p-value
# Ha The proportion of Churned is greater than 20% 
# When Ha is greater, lower.tail = FALSE
p_value <- pnorm(z_score, lower.tail = FALSE)


# Print the result
print(p_value)   

# Since the p_value is less than < 0.05, we can reject the H0 and accept Ha
# The mean of 

mean(customer_churn_boot_distn$resample_mean)

# is significantly greater than the null hypothesis


# When we estimate something based on a sample, there is always some uncertainty. 
# For example, if we estimate that 20% of customers will leave our telecom company, 
# there is a chance that the actual number is slightly different. 
# This is because our estimate is based on a limited sample of customers.

# A confidence interval is a range of values that is likely to contain the true value of the population parameter.
# For example, we might say that we are 95% confident that the proportion of customers leaving our company falls 
# between 15% and 25%. This means that if we were to take many samples of customers, 
# 95% of the time the true proportion would fall within the range of 15% to 25%.

# Calculate the confidence interval. Use the quantiles of the bootstrap distribution.


# Calculate 95% confidence interval using quantile method
conf_int_quantile <- customer_churn_boot_distn %>%
  summarize(lower = quantile(resample_mean, 0.025),
            upper = quantile(resample_mean, 0.975)
  )

# Print the result
conf_int_quantile

# Here, we are 95% sure that the churn rate is from 25.6 to 27.5%
# As a Data Scientist, you can tell tell the CEO's initial guess was wrong, and you're 95% certain that the customer
# churn rate is 25.6% to 27.5%