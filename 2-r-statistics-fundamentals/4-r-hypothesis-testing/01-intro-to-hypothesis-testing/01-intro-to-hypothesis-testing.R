install.packages("dplyr")
install.packages("tidyverse")


library(dplyr) # for functions
library(tidyverse)

wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/2-r-statistics-fundamentals/4-r-hypothesis-testing/01-intro-to-hypothesis-testing")

# Load the customer churn csv file
customer_churn <- read.csv("telecom_customer_churn.csv")

# View the customer_churn dataset
view(customer_churn)
glimpse(customer_churn)

# Calculate a point estimate (sample statistic), namely the proportion of customer churn.
# Percentage of those who churned of the total sample
churned_prop_samp <- customer_churn %>%
  summarize(prop_churned_customers = mean(Customer.Status == "Churned")) %>%
  pull(prop_churned_customers)


# See the results, the sample statistic churned_prop_samp.
print(churned_prop_samp)

# Calculating a z-score
# Since variables have arbitrary ranges and units, we need to standardize them. 
# For example, it would be silly if a hypothesis test gave a different answer if your variables were in Euros instead of US dollars. 
# Standardization avoids that.

# One standardized value of interest in a hypothesis test is called a z-score. 
# To calculate it, we need three numbers: the sample statistic (point estimate), 
# the hypothesized statistic, and the standard error of the statistic (which we estimate from the bootstrap distribution).
 

# churned_prop_boot_distn is a bootstrap distribution of the proportion of customer churn. 

# Let's make a hypothesis
# Hypothesize that the proportion is 20%
churned_prop_hyp <- 0.2

# Generate 1 bootstrap resample
customer_churn_resample <- customer_churn %>%
  slice_sample(prop = churned_prop_samp, replace = TRUE)

# See the result
customer_churn_resample


# Calculate mean churn prop of resample
mean_churned_1 <- customer_churn_resample %>% 
  summarize(mean_danceability = mean(Customer.Status == "Churned")) %>% 
  pull(mean_danceability)

# See the result
print(mean_churned_1)


# Replicate this 1000 times
mean_churned_1000 <- replicate(
  n = 1000,
  expr = {
    customer_churn_resample <- customer_churn %>% 
      slice_sample(prop = 1, replace = TRUE)
    customer_churn_resample %>% 
      summarize(mean_churned = mean(Customer.Status == "Churned")) %>% 
      pull(mean_churned)
  }
)

# See the result
print(mean_churned_1000)

# From previous steps
# mean_churned_1000 <- load_step_4()

# Store the resamples in a tibble
customer_churn_boot_distn <- tibble(
  resample_mean = mean_churned_1000
)

print(customer_churn_boot_distn)

# Draw a histogram of the resample means with binwidth 0.002
ggplot(customer_churn_boot_distn, aes(resample_mean)) +
  geom_histogram(binwidth = 0.002, fill = "royalblue4")

View(customer_churn_boot_distn)

# Calculate the standard error
std_error <- customer_churn_boot_distn %>% 
  summarize(sd_churned_prop = sd(resample_mean)) %>% 
  pull(sd_churned_prop)

std_error

# Find z-score of churned_prop_samp
z_score <- (churned_prop_samp - churned_prop_hyp) / std_error

# See the results
z_score # Is this high or low?
# The z-score is a standardized measure of the difference between the sample statistic and the hypothesized statistic.

# In order to determine whether to choose the null hypothesis or the alternative hypothesis, you need to calculate a p-value from the z-score.

# Let's return to the customer churn dataset and the proportion of customer churn.

# The null hypothesis is that the proportion of customer churn is 20 percent.
# The alternative hypothesisis that the proportion of customer churn is greater than 20 percent.

# The observed sample statistic, churned_prop_samp, the null hypothesis statistic, churned_prop_hyp (20%), 
# and the bootstrap standard error, std_error are available.

# Calculate the p-value
# Ha The proportion of Churned is greater than 20% 
p_value <- pnorm(z_score, lower.tail = FALSE)


# See the result
print(p_value)   


# If you give a single estimate of a sample statistic, you are bound to be wrong by some amount. 
# For example, the hypothesized proportion of late shipments was 6%. Even if evidence suggests the null hypothesis that 
# the proportion of late shipments is equal to this, for any new sample of shipments, 
# the proportion is likely to be a little different. Consequently, it's a good idea to state a confidence interval. 
# That is, you say "we are 95% 'confident' the proportion of late shipments is between A and B" (for some value of A and B).

# Sampling in R demonstrated two methods for calculating confidence intervals. 
# Here, you'll use quantiles of the bootstrap distribution to calculate the confidence interval.


# Calculate 95% confidence interval using quantile method
conf_int_quantile <- customer_churn_boot_distn %>%
  summarize(lower = quantile(resample_mean, 0.025),
            upper = quantile(resample_mean, 0.975)
  )

# See the result
conf_int_quantile
