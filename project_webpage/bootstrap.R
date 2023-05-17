
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
acf(rts)

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


