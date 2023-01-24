
# http://www.turingfinance.com/random-walks-down-wall-street-stochastic-processes-in-python/

# correlated gbm ----
# https://quant.stackexchange.com/questions/24472/two-correlated-brownian-motions

#n is number of samples 
#r is correlation
#t is tick step

# 1) Generate n standard normal variate for x (the first asset)
# 2) Since, ei∼N(0,1−ρ2). So generates n normal variate as ei from normal distribution with mean 0 and variance 1−ρ2.
# 3) Get y=ρx+ei (standard normal for y, the 2nd asset)
# 4) Convert your standard normal numbers back to Normal (remember correlation is independent of change of origin and scale)

f <- function(n=30, r=.5, t=1/252){
  asset1_sn <- rnorm(n, mean=0, sd= 1) # std normal version of first asset
  se <- sqrt(1 - r^2) # standard deviation of error
  e <- rnorm(n, mean=0, sd=se)
  asset2_sn <- r*asset1_sn + e # std normal for 2nd asset
  change1 <- c(0, asset1_sn* sqrt(t))
  change2 <- c(0, asset2_sn* sqrt(t))
  asset1 <- cumsum(change1)
  asset2 <- cumsum(change2)
  tibble(time=0:n, asset1=asset1, asset2=asset2) |> 
    pivot_longer(-time)
}

f() |> 
  ggplot(aes(time, value, colour=name)) +
  geom_line() +
  geom_hline(yintercept = 0)


n <- 30
r <- .5
t <- 1/252
x <- rnorm(n, mean=0, sd= 1)
se <- sqrt(1 - r^2) # standard deviation of error
e <- rnorm(n, mean=0, sd=se)
y <- r*x + e

change1 <- c(0, x* sqrt(t))
change2 <- c(0, y* sqrt(t))

stocks <- cumsum(x* sqrt(t))
bonds <- cumsum(y* sqrt(t))

tibble(stocks=stocks, bonds=bonds, time=1:length(stocks)) |> 
  pivot_longer(-time) |> 
  ggplot(aes(time, value, colour=name)) +
  geom_line()
  



corGBM <- function(n, r, t=1/365, plot=TRUE) {
  #n is number of samples 
  #r is correlation
  #t is tick step
  x <- rnorm(n, mean=0, sd= 1)
  se <- sqrt(1 - r^2) #standard deviation of error
  e <- rnorm(n, mean=0, sd=se)
  y <- r*x + e
  
  X <- cumsum(x* sqrt(t))
  Y <- cumsum(y* sqrt(t))
  Max <- max(c(X,Y))
  Min <- min(c(X,Y))
  
  if(plot) {
    plot(X, type="l", ylim=c(Min, Max))
    lines(Y, col="blue")
  }
  return(cor(x,y))
}

#sample result
corGBM(10000,.95)




# GBM with package ----
library(LSMRealOptions)
# https://search.r-project.org/CRAN/refmans/LSMRealOptions/html/GBM_simulate.html


# n	The total number of price paths to simulate
# t	The forecasting period, in years
# mu The drift term of the GBM process
# sigma	The volatility term of the GBM process
# S0	The initial value of the underlying asset
# dt	The discrete time step of observations, in years




## 100 simulations of 1 year of monthly price paths:
m <- GBM_simulate(n = 100,
                  t = 1,
                  mu = 0.05,
                  sigma = 0.2,
                  S0 = 100,
                  dt = 1/12)

m <- GBM_simulate(n = 10000,
                  t = 30,
                  mu = 0.07,
                  sigma = 0.12,
                  S0 = 100,
                  dt = 1)

colnames(m) <- 0:(ncol(m) - 1)

df <- as_tibble(m) |> 
  mutate(time=row_number() - 1) |> 
  pivot_longer(-time, names_to = "sim")

df |> 
  #filter(sim %in% 1:4) |> 
  filter(sim %in% sample(1:nrow(m), 5)) |> 
  ggplot(aes(time, value, colour=sim)) +
  geom_line() +
  geom_hline(yintercept = 100) +
  scale_x_continuous(breaks=0:100) +
  scale_y_continuous(limits=c(0, NA))

f <- function(x) x^(1/30) - 1
cagr <- m[31, ] |> f()
quantile(cagr)


# Geometric Brownian Motion from scratch ----

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




