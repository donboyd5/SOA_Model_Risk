# Create a vector of 50 years
years <- 1:50

# Create a matrix to store the asset returns
returns <- matrix(NA, nrow = 50, ncol = 3)

# Set the mean and standard deviation of the normal distribution
mu_normal <- 0.05
sigma_normal <- 0.10

# Set the degrees of freedom of the t distribution
df_t <- 5

# Set the AR(1) coefficient
phi <- -0.20

# Generate the asset returns
for (i in 1:50) {
  # Generate a normal return
  returns[i, 1] <- rnorm(1, mean = mu_normal, sd = sigma_normal)
  
  # Generate a t return
  returns[i, 2] <- rt(1, df = df_t)
  
  # Generate a mean-reverting return
  if(i==1) returns[i, 3] <- rnorm(1, mean = 0, sd = sigma_normal) else
    returns[i, 3] <- phi * returns[i - 1, 3] + rnorm(1, mean = 0, sd = sigma_normal)
}

# Plot the asset returns
plot(returns)

returns
cor(returns[, 1], returns[, 2])


# Load the required libraries
library(MASS)
library(mvtnorm)

# Set the seed for reproducibility
set.seed(123)

# Set the number of years
n_years <- 50

# Set correlation matrix
rho <- matrix(c(1.0, 0.5, 0.3,
                0.5, 1.0, 0.6,
                0.3, 0.6, 1.0), nrow=3, ncol=3)

# Set mean returns and standard deviation for the assets
mean_returns <- c(0.07, 0.06, 0.05)
sd_returns <- c(0.15, 0.20, 0.25)

# Generate normally distributed returns for asset 1
asset1_returns <- rmvnorm(n=n_years, mean=mean_returns[1], sigma=rho[1,1]*sd_returns[1]^2)

# Generate t-distributed returns for asset 2 (df=3 for fatter tails)
asset2_returns <- rmvt(n=n_years, sigma=rho[2,2]*sd_returns[2]^2, df=3)
asset2_returns <- asset2_returns * sd_returns[2] / sd(asset2_returns) + mean_returns[2] - mean(asset2_returns)

# Generate mean-reverting returns for asset 3
asset3_returns <- arima.sim(n=n_years, list(order=c(1,0,0), ar=-0.2, sd=sqrt(rho[3,3])*sd_returns[3]))
asset3_returns <- asset3_returns * sd_returns[3] / sd(asset3_returns) + mean_returns[3] - mean(asset3_returns)

# Combine into a data frame
returns <- data.frame(year=1:n_years, asset1=asset1_returns, asset2=asset2_returns, asset3=asset3_returns)

# Print the returns
print(returns)

cor(returns[, 1], returns[, 2])




