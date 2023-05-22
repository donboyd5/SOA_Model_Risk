
# Load the boot package
install.packages("boot")
library(boot)


rts <- ts(returns |> select(-year, -cpiu))

rts <- ts(returns |> select(sp500))
mean(rts)

f <- function(data) {
  return(mean(data))
}

f <- function(data) {
  return(data)
}

tail(rts)

library(meboot)
ensemble <- meboot(rts, reps = 5)
str(ensemble)
head(ensemble$x); head(rts)
ensemble$ensemble[1:10, 1:5]

x <- meboot(rts, reps = 1000)
dim(x$ensemble)



library(tseries)
x <- tsbootstrap(rts, nb=100, statistic=mean)
str(x)
x$statistic

set.seed(123)
bstrap <- tsboot(rts, statistic=f, R=50, l=10, n.sim=1000, sim="geom") # n.sim=10, 
bstrap$t[1:10, 1:5]

str(bstrap)
# bstrap$statistic
# bstrap$seed
bstrap$t0 # stat function applied to input data
mean(bstrap$data); mean(rts) # the input data
mean(bstrap$t) # result

dim(bstrap$t) # R rows, n.sim columns
bstrap$t # vector length R

# 50,] -0.03064452  0.302348431  0.074937280  0.09967051  0.01325921  0.06509284  0.185194902  0.317352455 -0.047023902  0.20419055

surrogate(rts, ns=5, fft=TRUE, amplitude = TRUE)



# old ---------------------------------------------------------------------
# Assuming 'returns' is your data frame
returns_ts <- ts(returns |> select(-year, -cpiu))



# Define a function to calculate the statistic of interest (e.g. mean)
stat_func <- function(data, indices) {
  return(mean(data[indices]))
}

stat_func <- function(data, indices) {
  return(colMeans(data[indices, ]))
}

# Perform block bootstrap
set.seed(123)  # for reproducibility
boot_results <- tsboot(returns_ts, statistic=stat_func, R=1000, l=5, sim="fixed")


install.packages("tseries")
library(tseries)
set.seed(123) # for reproducibility

# Define a block bootstrap function
block_bootstrap <- function(data, block_size, num_replications) {
  num_blocks <- nrow(data) / block_size
  bootstrap_samples <- vector("list", num_replications)
  
  for (i in 1:num_replications) {
    bootstrap_indices <- sample(1:num_blocks, num_blocks, replace = TRUE)
    bootstrap_indices <- unlist(lapply(bootstrap_indices, function(x) ((x - 1) * block_size + 1):(x * block_size)))
    bootstrap_samples[[i]] <- data[bootstrap_indices, ]
  }
  
  return(bootstrap_samples)
}

# Apply the function to your data
bootstrap_samples <- block_bootstrap(returns, block_size = 5, num_replications = 1000)

# Now bootstrap_samples is a list of 1000 bootstrap samples




# bootstrap ---------------------------------------------------------------

# Install and load the tseries package
# install.packages("tseries")
library(tseries)

# Assume these are your 50 years of annual returns
mret <- rnorm(50, mean = 0.07, sd = 0.15)

# Define the block size
block.size <- 5

# Number of bootstrap replicates
R <- 1000

# Perform block bootstrap using tsbootstrap function
# bootstrap_samples <- tsbootstrap(mret, nb = R, m = block.size, type = "stationary")
bs <- tsbootstrap(mret, nb = R, b = block.size, type = "stationary")
str(bs)


# bootstrap_samples now contains your 1000 scenarios of 50 years each


# bootstrap with real data ---------------------------------------------------------------
returns <- readRDS(here::here("data", "damodaran_data.rds"))
efflist <- readRDS(here::here("data", "eff_history.rds")) # efficient frontier, 1950-2022
analysis_years <- 1950:2022
(return_5050 <- returns |> 
    filter(year %in% analysis_years) |> 
    mutate(port5050=.5 * sp500 + .5 * baacorp) |> 
    pull(port5050) |> 
    mean())
i5050 <- iclosest(return_5050, efflist$effdf$mean)
(w5050 <- efflist$effdf |> 
    filter(obs == i5050) |> 
    rename_with(.fn= ~str_remove(.x, "w."), .cols=starts_with("w.")) |> 
    mutate(across(c(sp500, tbill3, ustbond, baacorp, realestate, gold),
                  ~ .x * (abs(.x) > 1e-12)))) # force weights to be at least 1e-12, else zero
tmp <- efflist$effdf

effport <- returns |> 
  mutate(porteff5050=w5050$sp500 * sp500 +
           w5050$tbill3 * tbill3 +
           w5050$ustbond * ustbond +
           w5050$baacorp * baacorp +
           w5050$realestate * realestate +
           w5050$gold * gold)

# Assume these are your 50 years of annual returns
# mret <- rnorm(50, mean = 0.07, sd = 0.15)
mret <- effport |> 
  filter(year %in% 1950:2022) |> 
  pull(porteff5050)

# Define the block size
block.size <- 5

# Number of bootstrap replicates
R <- 1000

# Perform block bootstrap using tsbootstrap function
# bootstrap_samples <- tsbootstrap(mret, nb = R, m = block.size, type = "stationary")
bs <- tsbootstrap(mret, nb = R, b = block.size, type = "stationary")


# we want 50 x 1000
ranstarts <- function(m1, nkeep){
  (maxstart <- nrow(m1) - nkeep + 1) # the latest year we can start in
  starts <- sample(1:maxstart, ncol(m1), replace=TRUE)
  
  # create a new submatrix with nyears rows for each column, starting in the random year
  f <- function(i, m){
    m[starts[i]:(starts[i] + nkeep - 1), i]
  }
  
  (ncols <- ncol(m1))
  ml <- purrr::map(1:ncols, f, m1) # create a list
  m2 <- matrix(unlist(ml), ncol=ncols, byrow=FALSE)  # convert the list to a matrix
  t(m2)
}

# dim(mebs$ensemble)

# maximum entropy resampling ----
set.seed(123)
mebs <- meboot(rts, reps = 1000)
m <- ranstarts(mebs$ensemble, 50)
e1071::kurtosis(m, type=1)
title2 <- "Maximum entropy bootstrapping, excess kurtosis=0.4"

# block resampling ----
statfn <- function(data, indices) {
  return(data[indices])
}
set.seed(123)

bsr <- tsboot(c(rts), statistic = statfn, R = 1000, l = 5, sim = "geom")
bsr <- tsboot(c(rts), statistic = statfn, R = 1000, l = 5, sim = "fixed", endcorr = FALSE)
dim(bsr$t)
m <- ranstarts(t(bsr$t), 50)
e1071::kurtosis(m, type=1)
title2 <- "Block resampling bootstrapping (blocksize=5), excess kurtosis=-0.0"

p2 <- tibble(sample=c(m)) |> 
  ggplot(aes(sample=sample)) +
  stat_qq_line() + 
  stat_qq(colour="blue", size=0.75) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = mean(m), linetype="dashed") +
  geom_vline(xintercept = 0) +
  scale_x_continuous(name="theoretical quantiles for standard normal distribution",
                     breaks=seq(-4, 4, 0.5),
                     labels=label_number(accuracy=0.1)) +
  scale_y_continuous(breaks=c(seq(-2, 2, .05), mean(ir)),
                     labels=label_percent(accuracy=0.1)) +
  ggtitle(label = title1, subtitle = title2) +
  theme_bw(base_size = 11)

p2



feasts::ACF(.data=as_tsibble(rts), value) |> autoplot()
stats::acf(rts)

str(bs)
dim(bs)
mean(bs); sd(bs)


m <- ranstarts(mebs$ensemble, 50)
e1071::kurtosis(m, type=1)
m <- ranstarts(bs, 50)
e1071::kurtosis(m, type=1)
dim(m)
mean(m); sd(m)
e1071::kurtosis(m, type=1)
e1071::kurtosis(rts, type=1)


tail(rts, 73)
e1071::kurtosis(tail(rts, 73), type=1)
c(rts)
p <- tibble(sample=c(tail(rts, 73))) |> 
  ggplot(aes(sample=sample)) +
  stat_qq_line() + 
  stat_qq(colour="blue", size=0.75) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = mean(ir), linetype="dashed") +
  geom_vline(xintercept = 0) +
  scale_x_continuous(name="theoretical quantiles for standard normal distribution",
                     breaks=seq(-4, 4, 0.5),
                     labels=label_number(accuracy=0.1)) +
  scale_y_continuous(breaks=c(seq(-2, 2, .05), mean(ir)),
                     labels=label_percent(accuracy=0.1)) +
  ggtitle(label = "Efficient long-only portfolio with same mean as 50-50 stocks-bonds, 1950-22", 
          subtitle = "Excess kurtosis = -0.30") +
  theme_bw(base_size = 11)

p





# Perform block bootstrap using tsboot function
statfn <- function(data, indices) {
  return(data[indices])
}
bsr <- tsboot(c(rts), statistic = statfn, R = 1000, l = 5, sim = "fixed")
bsr <- tsboot(c(rts), statistic = statfn, R = 1000, l = 5, sim = "geom")
str(bsr)
dim(bsr$t)

m <- ranstarts(t(bsr$t), 50)
e1071::kurtosis(m, type=1)

p2 <- tibble(sample=c(m)) |> 
  ggplot(aes(sample=sample)) +
  stat_qq_line() + 
  stat_qq(colour="blue", size=0.75) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = mean(ir), linetype="dashed") +
  geom_vline(xintercept = 0) +
  scale_x_continuous(name="theoretical quantiles for standard normal distribution",
                     breaks=seq(-4, 4, 0.5),
                     labels=label_number(accuracy=0.1)) +
  scale_y_continuous(breaks=c(seq(-2, 2, .05), mean(ir)),
                     labels=label_percent(accuracy=0.1)) +
  ggtitle(label = title1, subtitle = title2) +
  theme_bw(base_size = 11)

p2

set.seed(1234)
y0 <- arima.sim(n = 50, list(ar = -0.20), innov = rnorm(50, .07, .10))
mean(y0); sd(y0)
y1 <- scale_dist(y0, .07, .1)

ar(y0, aic=FALSE, order.max = 1)$ar
ar(y1, aic=FALSE, order.max = 1)$ar

e1071::kurtosis(y0, type=1)
e1071::kurtosis(y1, type=1)




library(mvtnorm)

# Set the mean, standard deviation, and correlation
mean1 <- 0.08  # Mean of asset 1 returns
std1 <- 0.15  # Standard deviation of asset 1 returns
mean2 <- 0.10  # Mean of asset 2 returns
std2 <- 0.12  # Standard deviation of asset 2 returns
correlation <- 0.5  # Correlation between asset 1 and asset 2 returns

# Set the number of samples (years)
num_samples <- 10000

# Set the seed for reproducibility
set.seed(0)

# Generate random numbers from the normal distributions
# returns1 <- rnorm(num_samples, mean = mean1, sd = std1)
# returns2 <- rnorm(num_samples, mean = mean2, sd = std2)

# Generate correlated returns
cor_matrix <- matrix(c(std1^2, correlation * std1 * std2,
                       correlation * std1 * std2, std2^2), ncol = 2)
correlated_returns <- rmvnorm(num_samples, mean = c(mean1, mean2), sigma = cor_matrix)

# Print the generated returns
cat("Asset 1 Returns:", returns1, "\n")
cat("Asset 2 Returns:", returns2, "\n")
cat("Correlated Returns:\n", correlated_returns, "\n")
cor(correlated_returns)


library(mvtnorm)

# Set the means, standard deviations, and correlation matrix
means <- c(0.08, 0.10, 0.12)  # Means of asset returns
stds <- c(0.15, 0.12, 0.18)  # Standard deviations of asset returns
correlation_matrix <- matrix(c(1.0, 0.5, 0.3,
                               0.5, 1.0, 0.4,
                               0.3, 0.4, 1.0), nrow = 3)  # Correlation matrix

# Set the degrees of freedom for the t-distribution
df <- 5  # Degrees of freedom

# Set the number of samples (years)
num_samples <- 1000

# Set the seed for reproducibility
set.seed(0)

# Generate correlated returns
cov_matrix <- diag(stds) %*% correlation_matrix %*% diag(stds)
correlated_returns <- matrix(0, nrow = num_samples, ncol = 3)

# Generate normal returns for assets 1 and 2
# correlated_returns[, 1:2] <- MASS::mvrnorm(n = num_samples, mu = means[1:2], Sigma = cov_matrix[1:2, 1:2])
# 
# # Generate t-distributed returns for asset 3
# correlated_returns[, 3] <- rt(num_samples, df = df) * stds[3] + means[3]


# Generate normal returns for assets 1 and 2
correlated_returns[, 1:2] <- MASS::mvrnorm(n = num_samples, mu = means[1:2], Sigma = correlation_matrix[1:2, 1:2])

# Generate t-distributed returns for asset 3, correlated with assets 1 and 2
corr_coeff_3 <- correlation_matrix[3, 1:2]
correlated_returns[, 3] <- rmvt(num_samples, sigma = diag(c(stds[3], stds[3])) %*% matrix(c(1, corr_coeff_3, corr_coeff_3, 1), nrow = 2), df = df) + means[3]

correlated_returns <- matrix(0, nrow = num_samples, ncol = 3)

# Generate normal returns for assets 1 and 2
correlated_returns[, 1:2] <- MASS::mvrnorm(n = num_samples, mu = means[1:2], Sigma = correlation_matrix[1:2, 1:2])

# Generate t-distributed returns for asset 3, correlated with assets 1 and 2
corr_coeff_3 <- correlation_matrix[3, 1:2]
corr_matrix_3 <- matrix(c(1, corr_coeff_3, corr_coeff_3, 1), nrow = 2)
cov_matrix_3 <- diag(c(stds[3], stds[3])) %*% sqrt(corr_matrix_3) %*% sqrt(corr_matrix_3)
correlated_returns[, 3] <- rmvt(n = num_samples, df = df, sigma = cov_matrix_3) + means[3]




# Print the generated correlated returns
cor(correlated_returns)




library(mvtnorm)

# Set the means, standard deviations, and correlation matrix
means <- c(0.08, 0.10, 0.12)  # Means of asset returns
stds <- c(0.15, 0.12, 0.18)  # Standard deviations of asset returns
correlation_matrix <- matrix(c(1.0, 0.5, 0.3,
                               0.5, 1.0, 0.4,
                               0.3, 0.4, 1.0), nrow = 3)  # Correlation matrix

# Set the degrees of freedom for the t-distribution
df <- 5  # Degrees of freedom

# Set the number of samples (years)
num_samples <- 1000

# Set the seed for reproducibility
set.seed(0)

# Generate correlated returns
correlated_returns <- rmvt(n = num_samples, sigma = diag(stds) %*% correlation_matrix %*% diag(stds), df = df, delta = means)

# Print the generated correlated returns
print("Correlated Returns:")
print(correlated_returns)


# abc ----
library(mvtnorm)

# Set the means, standard deviations, and correlation matrix
means <- c(0.08, 0.10, 0.12)  # Means of asset returns
stds <- c(0.15, 0.12, 0.18)  # Standard deviations of asset returns
correlation_matrix <- matrix(c(1.0, 0.5, 0.3,
                               0.5, 1.0, 0.4,
                               0.3, 0.4, 1.0), nrow = 3)  # Correlation matrix

# Set the degrees of freedom for the t-distribution for asset 3
df_asset3 <- 5  # Degrees of freedom for asset 3

# Set the number of samples (years)
num_samples <- 10

# Set the seed for reproducibility
set.seed(0)

# Generate correlated returns
correlated_returns <- matrix(0, nrow = num_samples, ncol = 3)

# Generate normal returns for assets 1 and 2
correlated_returns[, 1:2] <- MASS::mvrnorm(n = num_samples, mu = means[1:2], Sigma = correlation_matrix[1:2, 1:2])

# Generate t-distributed returns for asset 3 correlated with assets 1 and 2
corr_coeff_3 <- correlation_matrix[3, 1:2]
corr_matrix_3 <- matrix(c(1, corr_coeff_3, corr_coeff_3, 1), nrow = 2)
cov_matrix_3 <- diag(c(stds[3], stds[3])) %*% sqrt(corr_matrix_3) %*% sqrt(corr_matrix_3)
correlated_returns[, 3] <- rmvt(n = num_samples, df = df_asset3, sigma = cov_matrix_3) + means[3]

# Print the generated correlated returns
print("Correlated Returns:")
print(correlated_returns)


cor(correlated_returns)



cm

means <- c(0.08, 0.10, 0.12)  # Means of asset returns
stds <- c(0.15, 0.12, 0.18)  # Standard deviations of asset returns

# cholesky ----
nobs <- 1000
ncols <- 3
set.seed(123)
m1 <- rnorm(nobs, .05, .12)
m2 <- rnorm(nobs, .03, .04)
m3 <- rnorm(nobs, .09, .16)
m <- cbind(m1, m2, m3)
dim(m)
cor(m)
cm <- matrix(c(1, .2, .3,
               .2, 1, .9,
               .3, .9, 1),
             nrow=3)

cm <- matrix(c(1.0, 0.6, 0.9,
               0.6, 1.0, 0.5,
               0.9, 0.5, 1.0),
             nrow=3)
chol(cm)

chol1 <- chol(cm)
tchol1 <- t(chol1)
tchol1 %*% chol1

# mcor <- m %*% chol1
mcor <- m %*% tchol1
cor(mcor)


# https://stats.stackexchange.com/questions/160054/how-to-use-the-cholesky-decomposition-or-an-alternative-for-correlated-data-si
# https://stats.stackexchange.com/questions/38856/how-to-generate-correlated-random-numbers-given-means-variances-and-degree-of/38867#38867

n <- 10000
corM <- rbind(c(1.0, 0.6, 0.9), c(0.6, 1.0, 0.5), c(0.9, 0.5, 1.0))
set.seed(123)
SigmaEV <- eigen(corM)
eps <- rnorm(n * ncol(SigmaEV$vectors))
Meps <- matrix(eps, ncol = n, byrow = TRUE)    
Meps <- SigmaEV$vectors %*% diag(sqrt(SigmaEV$values)) %*% Meps
Meps <- t(Meps)
# target correlation matrix
corM
#      [,1] [,2] [,3]
# [1,]  1.0  0.6  0.9
# [2,]  0.6  1.0  0.5
# [3,]  0.9  0.5  1.0
# correlation matrix for simulated data
cor(Meps)
#           [,1]      [,2]      [,3]
# [1,] 1.0000000 0.6002078 0.8994329
# [2,] 0.6002078 1.0000000 0.5006346
# [3,] 0.8994329 0.5006346 1.0000000



# Load required libraries
library(mvtnorm)

# Set the means, standard deviations, and correlation matrix for the variables
mean1 <- 10  # Mean of variable 1
std1 <- 2  # Standard deviation of variable 1
mean2 <- 5  # Mean of variable 2
std2 <- 1  # Standard deviation of variable 2
correlation <- 0.5  # Correlation between variable 1 and variable 2

# Set the number of samples
num_samples <- 10000

# Generate correlated normal random numbers
set.seed(0)
correlated_norm <- rmvnorm(n = num_samples, mean = c(0, 0), sigma = matrix(c(1, correlation, correlation, 1), nrow = 2))

# Generate uncorrelated random numbers from different distributions
uncorrelated_var1 <- rnorm(num_samples, mean = 0, sd = std1)
uncorrelated_var2 <- rnorm(num_samples, mean = 0, sd = std2)

# Adjust the scaling of the correlated normal random numbers to achieve the desired correlation
correlated_norm[, 2] <- correlated_norm[, 2] * (std2 * sqrt(1 - correlation^2)) / sd(correlated_norm[, 2])

# Combine the correlated and uncorrelated random numbers to get the correlated random numbers from different distributions
correlated_var1 <- mean1 + std1 * correlated_norm[, 1] + uncorrelated_var1
correlated_var2 <- mean2 + std2 * correlated_norm[, 2] + uncorrelated_var2

# Print the generated correlated random numbers
print("Correlated Variable 1:")
print(correlated_var1)
print("Correlated Variable 2:")
print(correlated_var2)

cor(correlated_var1, correlated_var2)


# this is it ----
# but this is really (?) only good for normal distribution
# also see https://www.youtube.com/watch?v=WFEzkoK7tsE for copulas



library(mvtnorm)

# Set the means, standard deviations, and correlation matrix for the variables
mean1 <- 10  # Mean of variable 1
std1 <- 2  # Standard deviation of variable 1
mean2 <- 5  # Mean of variable 2
std2 <- 1  # Standard deviation of variable 2
correlation <- 0.5  # Correlation between variable 1 and variable 2

# Set the number of samples
num_samples <- 1000

# Generate standard normal random numbers
set.seed(0)
standard_norm <- matrix(rnorm(2 * num_samples), ncol = 2)

# Generate correlated normal random numbers using the Nataf transformation
correlated_norm <- rmvnorm(num_samples, mean = c(0, 0), sigma = matrix(c(1, correlation, correlation, 1), nrow = 2))
correlated_norm <- pnorm(correlated_norm)

# Apply inverse CDF (quantile) transformations to the desired distributions
correlated_var1 <- qnorm(correlated_norm[, 1], mean = mean1, sd = std1)
correlated_var2 <- qnorm(correlated_norm[, 2], mean = mean2, sd = std2)

# Print the generated correlated random numbers
print("Correlated Variable 1:")
print(correlated_var1)
print("Correlated Variable 2:")
print(correlated_var2)
cor(correlated_var1, correlated_var2)

