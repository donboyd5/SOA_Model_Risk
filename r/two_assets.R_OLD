# https://quant.stackexchange.com/questions/1260/r-code-for-ornstein-uhlenbeck-process
# Take a look at the sde package; specifically the dcOU and dsOU functions. You may also find some examples 
# on the R-SIG-Finance mailing list, which would be in the results of a search on www.rseek.org

# https://analyticsrusers.blog/2019/05/01/understanding-correlations-and-copulas-in-finance-an-application-in-risk-portfolio-aggregation-using-r/

# https://rviews.rstudio.com/2017/10/11/from-asset-to-portfolio-returns/
# http://rstudio-pubs-static.s3.amazonaws.com/289642_078ad0f5df5f4bd294d8c077d9e16b3d.html
# https://bookdown.org/compfinezbook/introcompfinr/Portfolios-of-Two-Risky-Assets.html

# https://www.r-bloggers.com/2014/04/fit-an-ornstein-uhlenbeck-process-with-discrete-time-series-data/

# http://copula.r-forge.r-project.org/book/02_copulas.html

library(quantmod)
k <- 10 # how many years back?
end<- format(Sys.Date(),"%Y-%m-%d")
start<-format(Sys.Date() - (k*365), "%Y-%m-%d")
symetf = c('SPY','TLT', 'IEF')
l <- length(symetf)
w0 <- NULL
for (i in 1:l){
  dat0 = getSymbols(symetf[i], src="yahoo", from=start, to=end, auto.assign = F,
                    warnings = FALSE, symbol.lookup = F)
  w1 <- weeklyReturn(dat0)
  w0 <- cbind(w0,w1)
}
time <- as.Date(substr(index(w0),1,10))
w0 <- as.matrix(w0)
colnames(w0) <- symetf
# head(w0)

# Let's split the sample in two parts
# Create a portfolio based on first part and 
# See how it is doing in the second part
t_insample <- NROW(w0)/2
tmpind <- 1:t_insample
w0_insample <- w0[tmpind, ]
w0_outsample <- w0[-tmpind, ]



library(FRAPO)   # install if not yet, using install.packages
citation("FRAPO")
ERC <- PERC(Sigma= cov(w0_insample) )@weights # Equal Risk Contribution
GB <- PGMV(Returns= w0_insample)@weights # Global Minimum Variance 
MTD <- PMTD(Returns= w0_insample)@weights # Minimum Tail-Dependence 
MD <- PMD(Returns= w0_insample)@weights # Most Diversified


tmp_tab <- cbind(ERC, GB, MTD, MD)
colnames(tmp_tab) <- c("Equal Risk Contribution", "Global Minimum Variance", "Minimum Tail-Dependence", "Most Diversified")
ports <-  w0_outsample %*% cbind(tmp_tab/100, 1/l)
cum_ret <- matrix(nrow= NROW(ports), ncol= NCOL(ports) )
for(j in 1:NCOL(ports)) {
  for( i in 1: NROW(ports) ) {
    cum_ret[i, j] <- prod( (1+ports[1:i,j]) ) - 1
  } }
52*(ports %>% apply(2, mean))


PERC(Sigma= cov(w0_insample) )@weights
port <- w0_outsample * ERC / 100

cumprod(c(1, 1 + rowSums(port)))

f1 <- function(port, w0){
  base::unname(base::rowSums(w0 * port / 100))
}
f1(ERC, w0)
port <- ERC

df <- as_tibble(w0, rownames="date") |> 
  mutate(erc=f1(ERC, w0), gb=f1(GB, w0),
         mtd=f1(MTD, w0), md=f1(MD, w0),
         samp=ifelse(row_number() <= t_insample,
                     "insamp", "outsamp"),
         date=as.Date(date)) |> 
  pivot_longer(-c(date, samp)) |> 
  arrange(name, date) |> 
  mutate(cumval=cumprod(1 + value), .by=c(samp, name))
count(df, name)


df |> 
  dplyr::filter(name %in% c("erc", "gb", "mtd", "md")) |> 
  dplyr::filter(samp=="insamp") |> 
  ggplot(aes(date, cumval, colour=name)) +
  geom_line(size=1)

df |> 
  dplyr::filter(name %in% c("erc", "gb", "mtd", "md")) |> 
  dplyr::filter(samp=="outsamp") |> 
  ggplot(aes(date, cumval, colour=name)) +
  geom_line(size=1)








# Set the number of investments and the number of returns to generate
n_investments <- 5
n_returns <- 1000

# Set the correlation matrix for the returns
correlation_matrix <- matrix(c(1, 0.5, 0.2, 0.3, 0.4,
                               0.5, 1, 0.3, 0.4, 0.5,
                               0.2, 0.3, 1, 0.5, 0.6,
                               0.3, 0.4, 0.5, 1, 0.7,
                               0.4, 0.5, 0.6, 0.7, 1), nrow = n_investments)

normalCopula()

# Use the Gaussian copula to generate correlated returns
returns <- rCopula(n_returns, copula=normalCopula(correlation_matrix))



# install required packages
install.packages("copula")
install.packages("simulate, predict")

# load packages
library(copula)
library(simulate, predict)

# set seed for reproducibility
set.seed(123)

# specify number of scenarios and time horizon
n_scenarios <- 1000
n_years <- 10

# specify bond portfolio parameters
bond_mean <- 0.05
bond_vol <- 0.03
bond_reversion <- 0.5

# specify stock portfolio parameters
stock_mean <- 0.1
stock_vol <- 0.15

# specify stock-bond correlation
cor





# load package
library(copula)


# function to generate returns using an Ornstein-Uhlenbeck (OU) process
ou_process <- function(mean, vol, rate, n_scenarios, n_years) {
  # specify time step
  delta_t <- 1/252
  
  # initialize matrix to store returns
  returns <- matrix(nrow = n_scenarios, ncol = (n_years*252))
  
  # loop over scenarios
  for (i in 1:n_scenarios) {
    # initialize process at mean
    x <- mean
    # loop over time steps
    for (t in 1:(n_years*252)) {
      # generate return using Euler-Maruyama approximation
      returns[i, t] <- x + (mean - x) * rate * delta_t + vol * sqrt(delta_t) * rnorm(1)
      # update process value
      x <- returns[i, t]
    }
  }
  
  # return returns
  return(returns)
}

# function to generate returns using a geometric Brownian motion (GBM) process
gbm_process <- function(mean, vol, n_scenarios, n_years) {
  # specify time step
  delta_t <- 1/252
  
  # initialize matrix to store returns
  returns <- matrix(nrow = n_scenarios, ncol = (n_years*252))
  
  # loop over scenarios
  for (i in 1:n_scenarios) {
    # initialize process at 1
    x <- 1
    # loop over time steps
    for (t in 1:(n_years*252)) {
      # generate return using Euler-Maruyama approximation
      returns[i, t] <- x * exp((mean - vol^2/2) * delta_t + vol * sqrt(delta_t) * rnorm(1))
      # update process value
      x <- returns[i, t]
    }
  }
  
  # return returns
  return(returns)
}

# set seed for reproducibility
set.seed(123)

# specify number of scenarios and time horizon
n_scenarios <- 10
n_years <- 2

# specify bond portfolio parameters
bond_mean <- 0.05
bond_vol <- 0.03
bond_reversion <- 0.5

# generate bond returns using OU process
bond_returns <- ou_process(mean = bond_mean, vol = bond_vol, rate = bond_reversion, 
                           n_scenarios = n_scenarios, n_years = n_years)

# specify stock portfolio parameters
stock_mean <- 0.1
stock_vol <- 0.15

# generate stock returns using GBM process
stock_returns <- gbm_process(mean = stock_mean, vol = stock_vol, 
                             n_scenarios = n_scenarios, n_years = n_years)

# specify stock-bond correlation
correlation <- 0.5

# generate correlated returns using copula function

# specify copula distribution
copula <- normalCopula(param = correlation, dim = 2)

# generate correlated returns
returns <- rCopula(n = n_scenarios*n_years*252, copula)

# split returns into bond and stock returns
bond_returns <- returns[, 1]
stock_returns <- returns[, 2]

# br2 <- returns[, 1]
# sr2 <- returns[, 2]
# 
# sr2[1:10, 1:5]


# reshape returns into matrix with one scenario per row
bond_returns <- matrix(bond_returns, nrow = n_scenarios, ncol = n_years*252)
stock_returns <- matrix(stock_returns, nrow = n_scenarios, ncol = n_years*252)

# compute bond and stock portfolios
bond_portfolio <- cumprod(1 + bond_returns)
stock_portfolio <- cumprod(1 + stock_returns)

# plot bond and stock portfolios
plot(bond_portfolio, type = "l")
plot(stock_portfolio, type = "l", col = "red")

































# install required packages
install.packages("tidyverse")
install.packages("simulate, predict")

# load packages
library(tidyverse)
# library(simulate)
# library(predict)

# set seed for reproducibility
set.seed(123)

# specify number of scenarios and time horizon
n_scenarios <- 3
n_years <- 2

# specify bond portfolio parameters
bond_mean <- 0.05
bond_vol <- 0.03
bond_reversion <- 0.5

# generate bond returns using OU process
bond_returns <- ou_process(mean = bond_mean, vol = bond_vol, rate = bond_reversion, 
                           n_scenarios = n_scenarios, n_years = n_years)
dim(bond_returns)

# specify stock portfolio parameters
stock_mean <- 0.1
stock_vol <- 0.15

# generate stock returns using GBM process
stock_returns <- gbm_process(mean = stock_mean, vol = stock_vol, 
                             n_scenarios = n_scenarios, n_years = n_years)
dim(stock_returns)

# compute bond and stock portfolios
bond_portfolio <- cumprod(1 + bond_returns)
# stock_portfolio <- cumprod(1 + stock_returns) # error
stock_portfolio <- stock_returns

# plot bond and stock portfolios
plot(bond_portfolio, type = "l")
plot(stock_portfolio, type = "l", col = "red")

# function to generate returns using an Ornstein-Uhlenbeck (OU) process
ou_process <- function(mean, vol, rate, n_scenarios, n_years) {
  # specify time step
  delta_t <- 1/252
  
  # initialize matrix to store returns
  returns <- matrix(nrow = n_scenarios, ncol = n_years * 252)
  
  # loop over scenarios
  for (i in 1:n_scenarios) {
    # initialize process at mean
    x <- mean
    # loop over time steps
    for (t in 1:(n_years*252)) {
      # generate return using Euler-Maruyama approximation
      returns[i, t] <- x + (mean - x) * rate * delta_t + vol * sqrt(delta_t) * rnorm(1)
      # update process value
      x <- returns[i, t]
    }
  }
  
  # return returns
  return(returns)
}

# function to generate returns using a geometric Brownian motion (GBM) process
gbm_process <- function(mean, vol, n_scenarios, n_years) {
  # specify time step
  delta_t <- 1/252
  
  # initialize matrix to store returns
  returns <- matrix(nrow = n_scenarios, ncol = n_years * 252)
  
  # loop over scenarios
  for (i in 1:n_scenarios) {
    # initialize process at 1
    x <- 1
    # loop over time steps
    for (t in 1:(n_years*252)) {
      # generate return using Euler-Maruyama approximation
      returns[i, t] <- x * exp((mean - vol^2/2) * delta_t + vol * sqrt(delta_t) * rnorm(1))
      # update process value
      x <- returns[i, t]
    }
  }
  
  # return returns
  return(returns)
}



library(sde)
set.seed (123)
d <- expression (-5 * x)
s <- expression (3.5)
sde.sim(X0 =10 , drift =d, sigma =s) -> X
plot (X, main = "Ornstein - Uhlenbeck")
