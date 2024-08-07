

# setup -------------------------------------------------------------------

source(here::here("report", "_common.R"))
source(here::here("report", "libraries_ts.R"))

library(quantmod)
library(stats4)
library(stochvolTMB)
library(stochvol)
library(rstan)
library(V8)


# stuff -------------------------------------------------------------------

data(spy)
summary(spy)
period <- c("2005-01-04", "2018-12-31")

plot(spy$date, spy$log_return, type = "l", xlab = "", ylab = "", main = "Log-returns of S&P500")

spy |> 
  mutate(lr=log(price / lag(price))) |> 
  head()

plot(spy$date, spy$price, type = "l", xlab = "", ylab = "", main = "Price of S&P500")

gaussian = estimate_parameters(spy$log_return, model = "gaussian", silent = TRUE)
t_dist = estimate_parameters(spy$log_return, model = "t", silent = TRUE)
skew_gaussian = estimate_parameters(spy$log_return, model = "skew_gaussian", silent = TRUE)
leverage = estimate_parameters(spy$log_return, model = "leverage", silent = TRUE)

summary(gaussian, report = "fixed")
summary(gaussian, report = "transformed")

AIC(gaussian, 
    t_dist, 
    skew_gaussian, 
    leverage)

plot(leverage, include_ci = TRUE, plot_log = TRUE, dates = spy$date)
plot(leverage, include_ci = TRUE, plot_log = FALSE, dates = spy$date)

# try my data -------------------------------------------------------------

spdaily <- readRDS(here::here("data", "sp500_index_rawdaily.rds"))

spy2 <- spdaily |> 
  fortify.zoo() |> 
  select(date=Index, sp500=GSPC.Adjusted) |> 
  # we can safely assume data are sorted by date
  mutate(log_return=log(sp500 / lag(sp500))) |> # note that we lose lr for the first observation
  filter(date >= period[1],
         date <= period[2])

summary(spy)
summary(spy2)
nrow(spy); nrow(spy2)


gaussian2 = estimate_parameters(spy2$log_return, model = "gaussian", silent = TRUE)
t_dist2 = estimate_parameters(spy2$log_return, model = "t", silent = TRUE)
skew_gaussian2 = estimate_parameters(spy2$log_return, model = "skew_gaussian", silent = TRUE)
leverage2 = estimate_parameters(spy2$log_return, model = "leverage", silent = TRUE)

summary(gaussian, report = "fixed")
summary(gaussian2, report = "fixed")
summary(gaussian, report = "transformed")

AIC(gaussian2, 
    t_dist2, 
    skew_gaussian2, 
    leverage2)


# try airg time period, daily and monthly data --------------------------------------------------------
period <- c("1955-12-01", "2003-12-31")

spy2 <- spdaily |> 
  fortify.zoo() |> 
  select(date=Index, sp500=GSPC.Adjusted) |> 
  # we can safely assume data are sorted by date
  mutate(log_return=log(sp500 / lag(sp500))) |> # note that we lose lr for the first observation
  filter(date >= period[1],
         date <= period[2])

summary(spy2)
nrow(spy2)


gaussian3 = estimate_parameters(spy2$log_return, model = "gaussian", silent = TRUE)
t_dist3 = estimate_parameters(spy2$log_return, model = "t", silent = TRUE)
skew_gaussian3 = estimate_parameters(spy2$log_return, model = "skew_gaussian", silent = TRUE)
leverage3 = estimate_parameters(spy2$log_return, model = "leverage", silent = TRUE)

summary(gaussian3, report = "fixed")
summary(gaussian3, report = "transformed")
summary(leverage3, report = "fixed")
summary(leverage3, report = "transformed")


summary(gaussian, report = "fixed")
summary(gaussian2, report = "fixed")
summary(gaussian, report = "transformed")

AIC(gaussian3, 
    t_dist3, 
    skew_gaussian3, 
    leverage3)

stochvolTMB::predict()

tmp <- predict(leverage3, steps = 10L, nsim = 10000, include_parameters = TRUE)
summary(tmp)

# monthly -----------------------------------------------------------------



spmonthly <- spdaily |> 
  to.monthly(OHLC=FALSE) |> 
  fortify.zoo() |> # to dataframe with Index from rowname
  select(date=Index, sp500=GSPC.Adjusted) |> 
  # we can safely assume data are sorted by date
  mutate(lr=log(sp500 / lag(sp500))) # note that we lose lr for the first observation
