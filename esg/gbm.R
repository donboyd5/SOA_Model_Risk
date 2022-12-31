
# Geometric Brownian Motion ----

## a single path ----
# First, we need to specify the parameters of the GBM model
mu <- 0.1  # drift term
sigma <- 0.2  # volatility term
S0 <- 100  # initial price of the asset
T <- 1  # time horizon (in years)
dt <- 1/252  # time step (in days)
n <- T/dt  # number of time steps

# Next, we can generate the random shocks using the normal distribution
Z <- rnorm(n)  # draw n random numbers from a normal distribution with mean 0 and standard deviation 1

# Now we can use the GBM equation to generate the discrete time path for the asset's price
S <- rep(0, n+1)  # create an empty vector to store the asset's price at each time step
S[1] <- S0  # set the initial price of the asset
for (t in 2:(n+1)) {
  S[t] <- S[t-1] * exp((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z[t-1])
}

# Finally, we can plot the asset's price over time
plot(S, type = "l", xlab = "Time", ylab = "Price")

## many paths, looping ----
# First, we need to specify the parameters of the GBM model
mu <- 0.1  # drift term
sigma <- 0.2  # volatility term
S0 <- 100  # initial price of the asset
T <- 1  # time horizon (in years)
dt <- 1/252  # time step (in days)
n <- T/dt  # number of time steps

# Next, we can generate the random shocks using the normal distribution
Z <- rnorm(n)  # draw n random numbers from a normal distribution with mean 0 and standard deviation 1

# Now we can use the GBM equation to generate the discrete time path for the asset's price
S <- rep(0, n+1)  # create an empty vector to store the asset's price at each time step
S[1] <- S0  # set the initial price of the asset

# Initialize a matrix to store the asset's price at each time step for each path
paths <- matrix(0, nrow = n+1, ncol = 10000)

# Generate the paths
for (i in 1:10000) {
  # Generate a new set of random shocks for each path
  Z <- rnorm(n)
  
  # Use the GBM equation to generate the discrete time path for the asset's price
  S[1] <- S0  # set the initial price of the asset
  for (t in 2:(n+1)) {
    S[t] <- S[t-1] * exp((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z[t-1])
  }
  
  # Store the asset's price at each time step for this path
  paths[, i] <- S
}

# Finally, we can plot the asset's price over time for one of the paths
plot(paths[, 1], type = "l", xlab = "Time", ylab = "Price")


## many paths, vectorized ----
# First, we need to specify the parameters of the GBM model
mu <- 0.1  # drift term
sigma <- 0.2  # volatility term
S0 <- 100  # initial price of the asset
T <- 1  # time horizon (in years)
dt <- 1/252  # time step (in days)
n <- T/dt  # number of time steps

# Next, we can generate the random shocks using the normal distribution
Z <- matrix(rnorm(n*10000), nrow = n)  # draw 10000 sets of n random numbers from a normal distribution with mean 0 and standard deviation 1

# Use the GBM equation to generate the discrete time path for the asset's price
S <- matrix(0, nrow = n+1, ncol = 10000)  # create a matrix to store the asset's price at each time step for each path
S[1, ] <- S0  # set the initial price of the asset
for (t in 2:(n+1)) {
  S[t, ] <- S[t-1, ] * exp((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z[t-1, ])
}

# Finally, we can plot the asset's price over time for one of the paths
path <- 1
plot(S[, path], type = "l", xlab = "Time", ylab = "Price")

dim(S)
S[1:5, 1:10]

# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))

# use S from above
mtt <- function(m){
  # matrix to tibble
  # takes a matrix of returns, m, where rows are days, columns are sims
  # and cells are asset values
  # returns a tibble with a column called sim, a column called day and
  # a column called asset
  # this is much faster than converting a matrix to tibble and pivoting
  # dim(m) # each col is a sim, each row is a day
  df <- tibble(sim=rep(1:ncol(m), each=nrow(m)),
               day=rep(0:(nrow(m) - 1), ncol(m)),
               asset=as.vector(m))
  df
}

# fast way to convert matrix of returns to a long data frame
# much faster than pivoting
dim(S) # each col is a sim, each row is a day
df <- mtt(S)

# df <- tibble(sim=rep(1:ncol(S), each=nrow(S)),
#              day=rep(0:(nrow(S) - 1), ncol(S)),
#              asset=as.vector(S))

# verify that days and sims are correct
old <- options(pillar.sigfig = 7)
simnum <- 103
df |> filter(sim==simnum, day<10)
S[1:10, simnum]
options(old)

df |> 
  filter(sim %in% c(101, 32, 53, 80)) |> 
  ggplot(aes(day, asset, colour=as.factor(sim))) +
  geom_line() +
  geom_hline(yintercept = 100)




