

# https://github.com/m-clark/models-by-example/blob/main/bayesian-stochastic-volatility.Rmd
# https://discourse.mc-stan.org/t/adapt-delta/7947/2
# https://groups.google.com/g/stan-users/c/zNttu0wgavg?pli=1
# https://mc-stan.org/rstanarm/reference/adapt_delta.html#references
# https://mc-stan.org/docs/stan-users-guide/time-series.html#stochastic-volatility-models


source(here::here("report", "_common.R"))
source(here::here("report", "libraries_ts.R"))

library(quantmod)
library(stats4)
library(stochvolTMB)
library(stochvol)
library(rstan)
library(rstantools)

# options(pillar.sigfig = 3) # default
options(pillar.print_max = 20)
tprint <- 25  # default tibble print
options(tibble.print_max = tprint, tibble.print_min = tprint) # show up to tprint rows


# get sp500 ---------------------------------------------------------------

spdaily <- readRDS(here::here("data", "sp500_index_rawdaily.rds"))

sp500m <- spdaily |> 
  to.monthly(OHLC=FALSE) |> 
  fortify.zoo() |> # to dataframe with Index from rowname
  tibble() |> 
  select(date=Index, price=GSPC.Adjusted) |> 
  # we can safely assume data are sorted by date
  mutate(date=ldom(as.Date(date)),
         vname="us", 
         src="sp500",
         lr=log(price / lag(price))) # note that we lose lr for the first observation

summary(sp500m)



# stan --------------------------------------------------------------------

period <- c("1955-12-31", "2003-12-31")

stock_index <- sp500m |> 
  filter(date >= period[1],
         date <= period[2]) |> 
  pull(price)

returns <- sp500m |> 
  filter(date >= period[1],
         date <= period[2]) |> 
  pull(lr)

# setup stan chatgpt  --------------------------------------------------------------


# Assuming you have your data ready in 'stock_index' variable


# E:/R_projects/projects/SOA_Model_Risk/report/airg.stan
fpath_chat <- fs::path(rptdir, "airg_chatg.stan")

# Fit the model
# data_list <- list(T = length(stock_index), stock_index = stock_index)
data_chat <- list(T = length(returns), log_returns = returns)

fit1 <- stan(file = fpath_chat, data = data_chat, iter = 2000, chains = 4)

# Check results
print(fit1)



# direct estimation, perplexity -------------------------------------------

library(rstan)

# Fit the model to the data
data_perplex <- list(T = length(returns), r = returns)

# Compile the model
fpath_perplex <- fs::path(rptdir, "airg_perplex.stan")


# model <- stan_model(model_code = model_code)
model <- stan_model(fpath_perplex)
fit2 <- sampling(model, data = data_perplex)

print(fit2) # Extract parameter estimates

# this is about the same
fit3 <- stan(file = fpath_perplex, data = data_perplex, iter = 2000, chains = 4)
print(fit3)

# old stuff ---------------------------------------------------------------



# get centered log returns
period <- c("1955-12-31", "2003-12-31")
# period <- c("1955-12-31", "2023-12-31")
# period <- c("1950-02-28", "2024-03-31")

log_returns <- spmonth |> 
  filter(date >= period[1],
         date <= period[2]) |> 
  mutate(mclr=scale(lr, scale = FALSE) |> c()) |> # mean-centered log return
  pull(mclr)

stan_data <- list(
  N_t = length(log_returns),
  y = log_returns
)

sfile <- here::here("report", "slv_best.stan") # best so far
sfile <- here::here("report", "slv1.stan") # slow
sfile <- here::here("report", "slv2.stan") # good 67 secs

# control <- list(adapt_delta=0.99, stepsize = 0.01, max_treedepth = 20)
# control <- list(adapt_delta= 0.99, stepsize = 0.005, max_treedepth = 20)
# control <- list(adapt_delta= 0.995, stepsize = 0.005, max_treedepth = 25)
# control <- list(adapt_delta= 0.995, max_treedepth = 25)
control <- list(adapt_delta= 0.995)

fit <- stan(
  file = sfile,
  data = stan_data,
  control=control,
  iter = 2000,
  chains = 4
)

print(
  fit,
  digits = 3,
  par    = c('mu', 'phi', 'sigma'),
  probs  = c(.025, .5, .975)
)


```