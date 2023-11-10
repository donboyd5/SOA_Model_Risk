
# geometric random walk 

# Set parameters
initial_value <- 100   # Initial value of the index
num_steps <- 100       # Number of steps in the random walk
std_dev <- 0.02        # Standard deviation of the normal residuals

# Initialize vector to store the log of index values
log_values <- numeric(num_steps)
log_values[1] <- log(initial_value)

# Generate the geometric random walk
for(i in 2:num_steps) {
  # Add a normally distributed random value to the log of the previous value
  log_values[i] <- log_values[i-1] + rnorm(1, mean = 0, sd = std_dev)
}

# Convert log values back to the index values
index_values <- exp(log_values)

# Plot the geometric random walk
plot(index_values, type = "l", col = "blue", xlab = "Time", ylab = "Index Value", main = "Geometric Random Walk")

# Assuming each step in the model represents one day
# and there are 252 trading days in a year

# Calculate log returns for each step
log_returns <- diff(log(index_values))

# Convert log returns to simple returns
simple_returns <- exp(log_returns) - 1

# Annualize the returns
# Note: This assumes the returns are compounded daily
# annual_returns <- (1 + simple_returns)^(252) - 1

annual_returns <- simple_returns

# Plot annual returns
plot(annual_returns, type = "l", col = "red", xlab = "Time", ylab = "Annual Return", main = "Annual Returns of the Asset")

# vectorized ----
# Set parameters
initial_value <- 100   # Initial value of the index
num_steps <- 100       # Number of steps in the random walk
num_series <- 1000     # Number of series to generate
std_dev <- 0.02        # Standard deviation of the normal residuals

# Generate a matrix of random residuals
residuals_matrix <- matrix(rnorm(num_series * num_steps, mean = 0, sd = std_dev), nrow = num_series, ncol = num_steps)

# Calculate cumulative sum of residuals and add initial log value
log_values_matrix <- t(apply(residuals_matrix, 1, cumsum)) + log(initial_value)

# Convert log values back to the index values
index_values_matrix <- exp(log_values_matrix)

# index_values_matrix now contains 1,000 series, each with 100 steps

# annual returns with drift ----
# Set parameters
initial_value <- 100   # Initial value of the index
num_steps <- 100       # Number of steps in the random walk, each representing one year
annual_drift <- 0.06   # Annual drift (6% annual return)
std_dev <- 0.12        # Standard deviation of the normal residuals

# Initialize vector to store the log of index values
log_values <- numeric(num_steps)
log_values[1] <- log(initial_value)

# Generate the geometric random walk with annual steps
for(i in 2:num_steps) {
  log_values[i] <- log_values[i-1] + rnorm(1, mean = annual_drift, sd = std_dev)
}

# Convert log values back to the index values
index_values <- exp(log_values)

# Plot the geometric random walk
plot(index_values, type = "l", col = "blue", xlab = "Year", ylab = "Index Value", main = "Geometric Random Walk with Annual Steps")

# Calculate annual returns
# The return for a year is the change from the previous year
annual_returns <- (index_values[-1] / index_values[-length(index_values)]) - 1

# Plot the annual returns
plot(annual_returns, type = "l", col = "red", xlab = "Year", ylab = "Annual Return", main = "Annual Returns")

# vectorized ----
# Set parameters
initial_value <- 100   # Initial value of the index
num_years <- 100       # Number of years in each series
num_series <- 1000     # Number of series
annual_drift <- 0.06   # Annual drift (6% annual return)
std_dev <- 0.12        # Standard deviation of the normal residuals

# Generate a matrix of random residuals
residuals_matrix <- matrix(rnorm(num_series * num_years, mean = annual_drift, sd = std_dev), nrow = num_series, ncol = num_years)

# Calculate cumulative sum of adjusted residuals and add initial log value
log_values_matrix <- apply(residuals_matrix, 1, cumsum) + log(initial_value)

# Convert log values back to index values
index_values_matrix <- exp(log_values_matrix)

# Calculate annual returns for each series
annual_returns_matrix <- (index_values_matrix[, -1] / index_values_matrix[, -ncol(index_values_matrix)]) - 1

# Example: Plot annual returns for the first series
plot(annual_returns_matrix[1, ], type = "l", col = "red", xlab = "Year", ylab = "Annual Return", main = "Annual Returns for Series 1")

     

