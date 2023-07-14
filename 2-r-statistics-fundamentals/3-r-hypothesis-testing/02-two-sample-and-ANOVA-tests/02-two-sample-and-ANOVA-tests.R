install.packages("tidyverse")


library(tidyverse)

wd <- getwd()
setwd("C:/Users/sclau/Documents/r-stats-programming-workshop/2-r-statistics-fundamentals/4-r-hypothesis-testing/02-two-sample-and-ANOVA-tests")

# Read the customer churn csv file
customer_churn <- read.csv("telecom_customer_churn.csv")

# View the unique_customer_churn dataset
View(customer_churn)



# Problem: The CEO of a telecommunication company called a stakeholders meeting. He had this intuition that 
# he just needs to focus on those who stay and retain them because the revenue from the those who stayed is obviously
# be more than those who churned. 73% stayed is way higher than 27 churned, right?

# However, you interrupted (just kidding) the CEO saying that we should better be sure than sorry. 😅
# The CEO is assuming that the generated revenue from each of the two groups are proportionally distributed.

# What if the company actually generates more money from those 26% of customers who left? 
# And they left because they subscribed to something unreasonably expensive?
  # Other underlying questions for future analyses based on this initiative are:
    # Should we win them back? 
    # Are there similar behaviors among the churned customers, similar reasons for churning, 
    # that we should look out for those who stayed, so we can retain them?


# T-TEST

# To answer the question, you should to an independent two-sample t-test: 
# This test is used to compare the means of a variable for two groups that are independent of each other.

# In a t-test, you should obtain the t-statistic (also known as the t-value). It is a measure that quantifies 
# the difference between the means of two groups and indicates the likelihood of observing such a difference by chance alone.

# Data Scientist sets up the hypothesis:
# H0: MuRevenueChurned <= MuRevenueStayed
# Ha: MuRevenueChurned > MuRevenueStayed

# Get the unique Customer.Id values
customer_churn %>% 
  distinct(Customer.ID)


unique_customer_churn <- customer_churn %>%
  filter(Customer.Status %in% c("Stayed", "Churned")) %>%
  distinct(Customer.ID, .keep_all = TRUE)

View(unique_customer_churn)

# To obtain the t-statistic, we need 6 numbers:
#   mean1 is the mean of the first group
#   mean2 is the mean of the second group
#   s1 is the standard deviation of the first group
#   s2 is the standard deviation of the second group
#   n1 is the degrees of freedom of observations in the first group
#   n2 is the degrees of freedom of observations in the second group

# The degrees of freedom for a t-test is equal to the number of observations in the data set minus 2.

# Calculate the separate means of the  two customer status groups
mean_values <- unique_customer_churn %>%
  group_by(Customer.Status) %>%
  summarise(mean_Total_Revenue = mean(Total.Revenue))

# View the result
print(mean_values)

# Get the mean value for "Stayed"
xbar_stayed <- mean_values$mean_Total_Revenue[mean_values$Customer.Status == "Stayed"]

# Get the mean value for "Churned"
xbar_churned <- mean_values$mean_Total_Revenue[mean_values$Customer.Status == "Churned"]


# Calculate the standard deviation using dplyr
std_values <- unique_customer_churn %>%
  group_by(Customer.Status) %>%
  summarise(sd_Total_Revenue = sd(Total.Revenue))

# View the result
std_values

# Get the standard deviation value for "Started"
s_stayed <- std_values$sd_Total_Revenue[std_values$Customer.Status == "Stayed"]

# Get the standard deviation value for "Churned"
s_churned <- std_values$sd_Total_Revenue[std_values$Customer.Status == "Churned"]

# Calculate the sample size using dplyr
sample_size <- unique_customer_churn %>%
  group_by(Customer.Status) %>%
  summarise(n = n())

# View the result
sample_size

# Remove "L" suffix from sample size values
sample_size$n <- as.numeric(sample_size$n)

# View the updated sample size
sample_size

# Access sample size value for "Stayed"
n_stayed <- sample_size$n[sample_size$Customer.Status == "Stayed"]

# Access sample size value for "Churned"
n_churned <- sample_size$n[sample_size$Customer.Status == "Churned"]


# The hypothesis test for determining if there is a difference between the means of two populations uses 
# a different type of test statistic to the z-scores. It's called "t", 
# and can be calculated from three values from each sample using this equation.

# The sample means for the two groups are available as xbar_stayed - xbar_churned. 
# The sample standard deviations are s_stayed and s_churned. The sample sizes are n_stayed and n_churned.


# Calculate the numerator of the test statistic
numerator <- xbar_stayed - xbar_churned

# Calculate the denominator of the test statistic
denominator <- sqrt(s_stayed ^ 2 / n_stayed + s_churned ^ 2 / n_churned)

# Calculate the test statistic
t_stat <- numerator / denominator

# Print the result
t_stat #When testing for differences between means, the test statistic is called 't' rather than 'z', 
# and can be calculated using six numbers from the samples.


# stayed = unique_customer_churn$Total.Revenue[unique_customer_churn$Total.Revenue== 'Stayed']
# churned = unique_customer_churn$Total.Revenue[unique_customer_churn$Total.Revenue== 'Churned']
# 
# t.test(stayed , churned,
#        paired = FALSE, var.equal = FALSE, conf.level = 0.95) 


# Calculate the degrees of freedom
degrees_of_freedom <- n_stayed + n_churned - 2

# H0: MuRevenueChurned <= MuRevenueStayed
# Ha: MuRevenueChurned > MuRevenueStayed

# Calculate the p-value from the test stat
p_value <- pt(t_stat, df = degrees_of_freedom, lower.tail = FALSE)

# Print the result
print(p_value)

# There is strong evidence to support the alternative hypothesis that the mean revenue for the "Churned" 
# group is indeed greater than the mean revenue for the "Stayed" group.

# Since the p-value is very low, we can reject the null hypothesis and conclude that there is a statistically 
# significant difference between the two means. 

# The observed difference between the means is likely due to chance (being xbar_churned < xbar_stayed).





# REVERSING THE HYPOTHESIS FORMULATION


# H0: MuRevenueChurned >= MuRevenueStayed
# Ha: MuRevenueChurned < MuRevenueStayed


# Calculate the p-value from the test stat
p_value <- pt(t_stat, df = degrees_of_freedom, lower.tail = TRUE)

# Print the result
print(p_value)

# There is not enough evidence to support the alternative hypothesis that the mean revenue for the "Churned" 
# group is indeed less than the mean revenue for the "Stayed" group.

# Since the p-value is high, we cannot reject the null hypothesis and we can
# conclude that there is no statistically significant difference between the two means.

# The observed difference between the means is in the opposite direction of what we expected (being xbar_churned < xbar_stayed), 
# but this is probably just by chance and not statistically significant.







# ANOVA

# Check for missing values
missing_rows <- !complete.cases(unique_customer_churn$Internet.Type)

# Remove missing rows
unique_customer_churn_clean <- unique_customer_churn[complete.cases(unique_customer_churn), ]

# Problem: The CEO wishes to determine if there are certain internet types that generate more revenue, so he consulted
# you as the company Data Scientist on how to determine if there are certain types of internet that the company should
# focus on for the marketing team's efforts.

# Q:
# Is the average total revenue different for different internet types?

# Take a peak of the data about the total revenue for each of the different internet types
ggplot(unique_customer_churn_clean, aes(x = Internet.Type, y = Total.Revenue)) +
  geom_boxplot() +
  coord_flip()

# Fiber Optic looks like it generates more revenue than the others. 
# Q: Should we immediately tell the marketing team
# to focus their marketing effort on this internet type to generate more sales and revenue?

# A: Not yet. As a Data Scientist, you have to verify if it is really significantly different.
# You need to do a hypothesis test

# To determine the significance of the average total revenue per internet type having 3 available categories, you
# will need to use ANOVA. 

# Analysis of variance (ANOVA) is a statistical test that is used to compare the means of two or more groups.

# ANOVA is a powerful tool that can be used to test a variety of hypotheses about the differences between groups. 
# It is often used in the social sciences, medical sciences, and business to compare the effects of different treatments 
# or interventions.


# For further information about the data, calculate the mean total revenue per internet type
mean_std_values <- unique_customer_churn_clean %>%
  group_by(Internet.Type) %>%
  summarise(xbar_Total_Revenue = mean(Total.Revenue),
            s_Total_Revenue = sd(Total.Revenue))

# View the result
mean_std_values


# Run a linear regression of Total.Revenue vs. Internet.Type
mdl_total_revenue_vs_internet_type <- lm(Total.Revenue ~ Internet.Type, data = unique_customer_churn_clean)

# Print the results
summary(mdl_total_revenue_vs_internet_type)
  
  
# Perform ANOVA on the regression model
anova(mdl_total_revenue_vs_internet_type)
  

# The anova function shows that at least two of the internet types have significant differences at the 2.2e-16 level
# in the revenue generated but does not tell which internet types

# Perform Pairwise T-Test to further support the ANOVA methodology on determining which among the internet types
# have significant differences when it comes to the total revenue generated

# A Pairwise T-Test is a statistical test that is used to compare the means of two groups that are paired together.
# To compare all three, the Pairwise T-Test will conduct 3 tests to hypothesis test all possible pair combinations
# of internet type.
# Generally, the number of tests a Pairwise T-Test is C(n, 2) = n*(n-1)/2 or  num_categories C 2

test_results <- pairwise.t.test(
  unique_customer_churn_clean$Total.Revenue,
  unique_customer_churn_clean$Internet.Type,
  p.adjust.method = "none"
)

# Print the results
test_results
# The result shows a matrix of 3 p-values
# Two of them is less than the alpha of 0.05
# Only the Cable-DSL pair have no significant differences in the revenue generated

# As the number of categories or groups increases, the no. of pairs and tests increase quadratically.
# The higher the number of tests, the higher the chance of having at least one False Positive result.

# True Positive (TP): Correctly classified infected individuals
# False Positive (FP): Incorrectly classified non-infected individuals as infected
# True Negative (TN): Correctly classified non-infected individuals
# False Negative (FN): Misclassified infected individuals


# For example, if there are 3 pairwise comparisons and the significance level is 0.05, then the false positive rate would be:
# FP = (1 - 1/3) * 0.05 = 0.1667



# To address this, modify the pairwise t-tests to use Bonferroni p-value adjustment to reduce the reduce the likelihood
# of getting a False Positive.

# The Bonferroni correction is calculated by dividing the significance level by the number 
# of comparisons being made. For example, if the significance level is 0.05 and there are 10 comparisons being made, 
# then the Bonferroni-corrected significance level would be 0.005.

# This means that each comparison would need to be significant at the 0.005 level in order to be 
# considered statistically significant. The Bonferroni correction can be used with any type of 
# statistical test, but it is most commonly used with pairwise t-tests.

test_results <- pairwise.t.test(
    unique_customer_churn$Total.Revenue,
    unique_customer_churn$Internet.Type,
    p.adjust.method = "bonferroni"
  )
  
test_results 

# Still, the DSL-Cable pair has no significant differences in the generated average revenue

# Other p-adjustment methods
p.adjust.methods
# Default: Holm  

# A: Fiber Optic generated more revenue on the average compared with either DSL or Cable.
# You, as a Data Scientist, can recommend to the marketing team to focus their efforts marketing this internet type
# for more chances of conversions.
  