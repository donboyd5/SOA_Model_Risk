library(dplyr)
library(ggplot2)

# Build a simple Asset Liability Model that simulates the evoluation of assets and liabilities over 30 years
# Inputs are investment returns generated from another ESG model for 30 years
# Assumptions include discount rate, salary growth rate, inflation rate, initial benefit payments, assets, and liabilities
# Funding policies include the following: 20 year open level dollar amortization

# Set assumptions
salary_growth <- 0.04
benefit_factor <- 0.02
discount_rate <- 0.07
COLA <- 0.02
inflation_rate <- 0.02
amort_period <- 30

# set the number of simulation years
n <- 30

# Calculate annuity factor for amortization over 15 years
growth_annuity <- (1 - ((1 + salary_growth) / (1 + discount_rate))^amort_period) / (discount_rate - salary_growth)
print(growth_annuity)

# Create the initial empty dataframe
alm <- data.frame(
  year = 1:n,
  asset = rep(0, n),
  liability = rep(0, n),
  normal_cost = rep(0, n),
  payroll = rep(0, n),
  benefit_payment = rep(0, n),
  contribution = rep(0, n),
  investment_returns = rep(0, n),
  investment_return_rate = rep(0.07, n),
  discount_rate = rep(discount_rate, n)
  )

# Input the investment return rates
alm$investment_return_rates <-

# Set the initial scenario (year 0) for assets, liabilities, and benefit payments
alm$liability[1] <- 100
alm$asset[1] <- 78
alm$payroll[1] <- 19            #19% of actuarial liabilities
alm$benefit_payment[1] <- 5.13  #27% of payroll
alm$normal_cost[1] <- 2.28       #12% of payroll)
alm$contribution[1] <- alm$normal_cost[1] + (alm$liability[1]-alm$asset[1])/growth_annuity
alm$investment_returns[1] <- alm$asset[1]*0.07

# set return scenario assumptions met
alm$investment_return <- 0.07


# Calculate assets, liabilities, contributions in 30 years
for (i in 2:nrow(alm)) {
  alm$payroll[i] <-  alm$payroll[i-1] * (1+salary_growth)
  alm$benefit_payment[i] <-alm$benefit_payment[i-1] * (1+salary_growth)
  alm$normal_cost[i] <-  alm$normal_cost[i-1] * (1+salary_growth)
  alm$liability[i] <-  alm$liability[i-1] * (1 + discount_rate) + alm$normal_cost[i] - alm$benefit_payment[i]
  alm$investment_returns[i] <- alm$asset[i-1] * alm$investment_return_rate[i]
  alm$contribution[i] <- alm$normal_cost[i] + (alm$liability[i-1]  - alm$asset[i-1])/growth_annuity
  alm$asset[i] <- alm$asset[i-1] + alm$contribution[i] + alm$investment_returns[i] - alm$benefit_payment[i]
  }

alm <- alm %>%
  mutate(
  normal_cost_rate = normal_cost/payroll,
  contribution_rate = contribution/payroll,
  funded_ratio = asset/liability)

ggplot(alm, aes(x=year, y=funded_ratio)) + 
  geom_line() +
  ylim(0, 1)

