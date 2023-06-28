#Ho: The mean Total Revenue is the same for those that Churned and those that Stayed
#Ha: The mean Total Revenue is greater for those that Stayed and those that Churned
#HO:  MuStayed = MuChurned
#Ha:  MuStayed > MuChurned

install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")


library(dplyr) # for functions
library(tidyverse)

# Read the customer churn csv file
customer_churn <- read.csv("telecom_customer_churn.csv")

# View the customer_churn dataset
View(customer_churn)


# Calculate the separate means using dplyr
mean_values <- customer_churn %>%
  group_by(Customer.Status) %>%
  summarise(mean_Total_Revenue = mean(Total.Revenue))

# View the result
mean_values

# Access mean value for "Stayed"
xbar_stayed <- mean_values$mean_Total_Revenue[mean_values$Customer.Status == "Stayed"]

# Access mean value for "Churned"
xbar_churned <- mean_values$mean_Total_Revenue[mean_values$Customer.Status == "Churned"]


# Calculate the standard deviation using dplyr
std_values <- customer_churn %>%
  group_by(Customer.Status) %>%
  summarise(sd_Total_Revenue = sd(Total.Revenue))

# View the result
std_values

# Access standard deviation value for "Started"
s_stayed <- std_values$sd_Total_Revenue[std_values$Customer.Status == "Stayed"]

# Access standard deviation value for "Churned"
s_churned <- std_values$sd_Total_Revenue[std_values$Customer.Status == "Churned"]

# Calculate the sample size using dplyr
sample_size <- customer_churn %>%
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


# The hypothesis test for determining if there is a difference between the means of two populations uses a different type of test statistic to the z-scores you saw in Chapter one. It's called "t", and can be calculated from three values from each sample using this equation.
# 
#  
#  
#  
# 
# While trying to determine why some shipments are late, you may wonder if the weight of the shipments that were late is different from the weight of the shipments that were on time. The late_shipments dataset has been split into a "yes" group, where late == "Yes" and a "no" group where late == "No". The weight of the shipment is given in the weight_kilograms variable.
# 
# For convenience, the sample means for the two groups are available as xbar_no and xbar_yes. The sample standard deviations are s_no and s_yes. The sample sizes are n_no and n_yes.

# Calculate the numerator of the test statistic
numerator <- xbar_stayed - xbar_churned

# Calculate the denominator of the test statistic
denominator <- sqrt(s_stayed ^ 2 / n_stayed + s_churned ^ 2 / n_churned)

# Calculate the test statistic
t_stat <- numerator / denominator

# See the result
t_stat #When testing for differences between means, the test statistic is called 't' rather than 'z', and can be calculated using six numbers from the samples.


# stayed = customer_churn$Total.Revenue[customer_churn$Total.Revenue== 'Stayed']
# churned = customer_churn$Total.Revenue[customer_churn$Total.Revenue== 'Churned']
# 
# t.test(stayed , churned,
#        paired = FALSE, var.equal = FALSE, conf.level = 0.95) 


# Calculate the degrees of freedom
degrees_of_freedom <- n_stayed + n_churned - 2

# Calculate the p-value from the test stat
p_value <- pt(t_stat, df = degrees_of_freedom)

# See the result
p_value

#################################################
############## t-tests: examine if the difference in means is significant or not

install.packages("ggplot2", dependencies=TRUE)

data("ToothGrowth")
head(ToothGrowth)
str(ToothGrowth)

hist(ToothGrowth$len)
# Shapiro-Wilk normality test 
#H0: data are normally distributed
shapiro.test(ToothGrowth$len) #data are normally distributed

library(ggplot2)
qplot(supp,len,data=ToothGrowth, 
      main="Tooth growth of guinea pigs",xlab="Supplement type", ylab="Tooth length") + geom_boxplot(aes(fill = supp))

mean(ToothGrowth$len)
### one sided t-test
### test if the mean value is equal to a certain number
### H0: true value of mean=18
t.test(ToothGrowth$len,mu=18)
#or greater than or less than a given value

t.test(ToothGrowth$len, alternative = "greater", mu = 3)
#H0: true value of mean is 3


# independent 2-group t-test
# test the difference in mean
#H0: These is no difference in the population means of the 2 groups
# split data set
OJ = ToothGrowth$len[ToothGrowth$supp == 'OJ']
VC = ToothGrowth$len[ToothGrowth$supp == 'VC']

t.test(OJ, VC,
       paired = FALSE, var.equal = FALSE, conf.level = 0.95) 
#Variances of tooth growth are different when using different supplement and dosage.
#test for mean being significantly different

t.test(OJ, VC,alternative = "greater",paired = TRUE) 

#test for mean being significantly greater
#paired=FALSE : measurements collected separately
#If you randomly sample each set of items separately, under different conditions, the samples are independent


## paired observations: If you collect two measurements on each item
t.test(OJ, VC,
       paired = TRUE, var.equal = FALSE, conf.level = 0.95) 

#e.g before or after treatments
# OJ and VC were applied on test subjects in 2 treatment groups


# Calculate the separate means using dplyr
mean_std_values <- customer_churn %>%
  group_by(Internet.Type) %>%
  summarise(xbar_Total_Revenue = mean(Total.Revenue),
            s_Total_Revenue = sd(Total.Revenue))

# View the result
mean_std_values

# Using late_shipments, plot pack_price vs. shipment_mode
# as a box plot with flipped x and y coordinates
ggplot(customer_churn, aes(x = Internet.Type, y = Total.Revenue)) +
  geom_boxplot() +
  coord_flip()

# Run a linear regression of pack price vs. shipment mode 
mdl_total_revenue_vs_internet_type <- lm(Total.Revenue ~ Internet.Type, data = customer_churn)

# See the results
summary(mdl_total_revenue_vs_internet_type)
  
  
  # Perform ANOVA on the regression model
  anova(mdl_total_revenue_vs_internet_type)
  
  # Pairwise t-tests
  # The ANOVA test didn't tell us which categories of shipment mode had significant differences in pack prices. To pinpoint which categories had differences, we could instead use pairwise t-tests.
  # Perform pairwise t-tests on pack price, grouped by shipment mode, no p-value adjustment
  test_results <- pairwise.t.test(
    customer_churn$Total.Revenue,
    customer_churn$Internet.Type,
    p.adjust.method = "none"
  )
  
  # See the results
  test_results
  
  
  #Modify the pairwise t-tests to use Bonferroni p-value adjustment.
  
test_results <- pairwise.t.test(
    customer_churn$Total.Revenue,
    customer_churn$Internet.Type,
    p.adjust.method = "bonferroni"
  )
  
test_results 
  
  