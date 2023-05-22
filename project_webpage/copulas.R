# https://www.r-bloggers.com/2015/10/modelling-dependence-with-copulas-in-r/
# https://gist.github.com/mick001/620195a84212c6afea1c
# https://cran.r-project.org/web/packages/copula/copula.pdf

# http://firsttimeprogrammer.blogspot.com/2016/03/how-to-fit-copula-model-in-r-heavily.html


library(MASS)
set.seed(100)

m <- 3
n <- 2000
sigma <- matrix(c(1, 0.4, 0.2,
                  0.4, 1, -0.8,
                  0.2, -0.8, 1), 
                nrow=3)
z <- mvrnorm(n,mu=rep(0, m),Sigma=sigma,empirical=T)


library(psych)
cor(z,method='spearman')
pairs.panels(z)

# [,1]       [,2]       [,3]
# [1,] 1.0000000  0.3812244  0.1937548
# [2,] 0.3812244  1.0000000 -0.7890814
# [3,] 0.1937548 -0.7890814  1.0000000

u <- pnorm(z)
pairs.panels(u)

library(rgl)
plot3d(u[,1],u[,2],u[,3],pch=20,col='navyblue')


plot3d(z[,1],z[,2],z[,3],pch=20,col='green')

x1 <- qgamma(u[,1],shape=2,scale=1)
x2 <- qbeta(u[,2],2,2)
x3 <- qt(u[,3],df=5)
plot3d(x1,x2,x3,pch=20,col='blue')

df <- cbind(x1,x2,x3)
pairs.panels(df)
cor(df,meth='spearman')

# x1         x2         x3
# x1 1.0000000  0.3812244  0.1937548
# x2 0.3812244  1.0000000 -0.7890814
# x3 0.1937548 -0.7890814  1.0000000

library(copula)
set.seed(100)
myCop <- normalCopula(param=c(0.4,0.2,-0.8), dim = 3, dispstr = "un")
str(myCop)
myCop@dispstr
myCop@param.names

myMvd <- mvdc(copula=myCop, margins=c("gamma", "beta", "t"),
              paramMargins=list(list(shape=2, scale=1),
                                list(shape1=2, shape2=2), 
                                list(df=5)) )
# Z2 <- rmvdc(myMvd, 2000)
Z2 <- rMvdc(2000, myMvd)
Z2 <- rMvdc(10000, myMvd)
colnames(Z2) <- c("x1", "x2", "x3")
pairs.panels(Z2)

# cree <- read.csv('cree_r.csv',header=F)$V2
# yahoo <- read.csv('yahoo_r.csv',header=F)$V2

# plot(cree,yahoo,pch='.')
# abline(lm(yahoo~cree),col='red',lwd=1)
# cor(cree,yahoo,method='spearman')
# 
# [1] 0.4023584


# clayton copula ----
# load the package
library(copula)

# specify the copula (in this case, a Clayton copula)

# The Clayton copula is a specific type of copula that is particularly good at
# modeling situations where the variables have a high degree of dependency in
# their lower tail. In other words, it can effectively capture the situation
# where low values of one variable are associated with low values of another
# variable.

# A common application of the Clayton copula in finance is in modeling asset
# returns. In this context, a Clayton copula can capture the observation that
# assets often decrease together in value during periods of market stress (i.e.,
# they have "lower tail dependence").

clayton_cop <- claytonCopula(dim = 2, param = 2)

# generate uniform marginals using the copula
uniform_data <- rCopula(1000, clayton_cop)
cor(uniform_data)
plot(uniform_data)
hist(uniform_data[, 1])
hist(uniform_data[, 2])


# transform the uniform marginals to Gaussian marginals
gaussian_data <- qnorm(uniform_data)
cor(gaussian_data)
hist(gaussian_data[, 1])
hist(gaussian_data[, 2])

# plot the data
plot(gaussian_data)

# now make one variable follow  a t distribution
# transform the first variable to Gaussian and the second to a t-distribution with 3 degrees of freedom (heavier tails)
gaussian_t_data <- cbind(qnorm(uniform_data[, 1]), qt(uniform_data[, 2], df = 3))
gaussian_t_data <- cbind(qnorm(uniform_data[, 1]), qt(uniform_data[, 2], df = 14))
cor(gaussian_t_data)
hist(gaussian_t_data[, 1])
hist(gaussian_t_data[, 2])
plot(gaussian_t_data)



# ggplot ------------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))
as_tibble(gaussian_t_data, .name_repair="unique") |> 
  select(nvar=1, tvar=2) |> 
  mutate(obs=row_number()) |> 
  pivot_longer(-obs) |> 
  filter(obs %in% 1:100) |> 
  ggplot(aes(obs, value, colour=name)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()




# rugarch -----------------------------------------------------------------

# install the package if it's not already installed
# load the package
library(rugarch)

# assume 'returns' is a matrix of returns for different assets
retdf <- readRDS(here::here("data", "damodaran_data.rds"))

returns <- retdf |> 
  select(-c(cpiu, year)) |> 
  as.matrix()

# specify the model
spec <- ugarchspec(mean.model = list(armaOrder = c(0, 0)), 
                   variance.model = list(model = "sGARCH"), 
                   distribution.model = "norm")

# fit the model
fit <- ugarchfit(spec, data = returns)
fit # doesn't look like serial correlation

# get the estimated conditional correlations
str(fit)
corr <- fitted(fit, type = "correlation")

# look at the correlations
head(corr)



# dcc rmgarch -------------------------------------------------------------
# install the package if it's not already installed
# if (!require(rmgarch)) {
#   install.packages("rmgarch")
# }

# load the necessary packages
library(rmgarch)

# create a date sequence from 2000 to 2023
dates <- seq(as.Date("2000/1/1"), as.Date("2023/12/31"), by = "day")

# generate random returns for two assets
set.seed(123)
asset1 <- rnorm(length(dates), mean = 0.0005, sd = 0.02)
asset2 <- rnorm(length(dates), mean = 0.0005, sd = 0.02)

# create the data frame
df <- data.frame(date = dates, asset1 = asset1, asset2 = asset2)

# add a new column for the year
df$year <- year(df$date)

skim(df)

# calculate annual returns
annual_returns <- df %>%
  group_by(year) %>%
  summarise(asset1 = sum(asset1), asset2 = sum(asset2))

# convert to a time series object
library(timeSeries)
returns_ts <- as.timeSeries(annual_returns[, c("asset1", "asset2")])

# specify the univariate GARCH models
uspec <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(0, 0)), 
                                           variance.model = list(model = "sGARCH"), 
                                           distribution.model = "norm")))

# specify the DCC model
dccspec <- dccspec(uspec = uspec, dccOrder = c(1, 1))

# fit the DCC model to the data
dccfit <- dccfit(dccspec, data = returns_ts)

# simulate new data from the fitted model
simulated_data <- ugarchsim(dccfit, n.sim = 50)

# plot the simulated data
plot(simulated_data)


monthly_returns <- df %>%
  mutate(month=month(date)) |> 
  group_by(year, month) %>%
  summarise(asset1 = sum(asset1), asset2 = sum(asset2))

# convert to a time series object
returns_ts <- as.timeSeries(monthly_returns[, c("asset1", "asset2")])

# specify the univariate GARCH models
uspec <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(0, 0)), 
                                           variance.model = list(model = "sGARCH"), 
                                           distribution.model = "norm")))

# specify the DCC model
dccspec <- dccspec(uspec = uspec, dccOrder = c(1, 1))

# fit the DCC model to the data
dccfit <- dccfit(dccspec, data = returns_ts)

# simulate new data from the fitted model
simulated_data <- ugarchsim(dccfit, n.sim = 50)

# plot the simulated data
plot(simulated_data)

# specify the univariate GARCH models
uspec <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(0, 0)), 
                                           variance.model = list(model = "sGARCH"), 
                                           distribution.model = "norm")))

# specify the DCC model
dccspec <- dccspec(uspec = uspec, dccOrder = c(1, 1))

# simulate from the DCC model
simulated_data <- rmgarchsim(spec = dccspec, n.sim = 50)

# plot the simulated data
plot(simulated_data)


# Specify the univariate GARCH models
uspec <- multispec(replicate(2, ugarchspec(mean.model = list(armaOrder = c(0, 0)), 
                                           variance.model = list(model = "sGARCH"), 
                                           distribution.model = "norm")))

# Specify the DCC model
dccspec <- dccspec(uspec = uspec, dccOrder = c(1, 1))

# Simulate from the DCC model
simulated_data <- rmgarchpath(spec = dccspec, n.sim = 50)

# Plot the simulated data
plot(simulated_data)



# two assets --------------------------------------------------------------

# load necessary libraries
library(copula)
library(sde)

# parameters for the Ornstein-Uhlenbeck process
theta <- 0.15  # mean reversion level
mu <- 0.05  # mean reversion speed
sigma <- 0.02  # volatility

# simulation parameters
n <- 1000  # number of observations
dt <- 1/252  # time step (assuming 252 trading days in a year)

# simulate Ornstein-Uhlenbeck processes for two assets
OU1 <- sde.sim(X0=theta, r=mu, sigma=sigma, T=1, N=n, model="OU")
OU2 <- sde.sim(X0=theta, r=mu, sigma=sigma, T=1, N=n, model="OU")

# get the returns of the two assets
returns1 <- diff(OU1[,2])
returns2 <- diff(OU2[,2])

# standardize the returns to have zero mean and unit variance
returns1 <- scale(returns1)
returns2 <- scale(returns2)

# define a Clayton copula with lower tail dependence
clayton <- claytonCopula(param=2, dim=2)

# simulate from the copula
u <- rCopula(n, clayton)

# apply the inverse cumulative distribution function of the returns
# to the uniform marginals from the copula
sim_returns1 <- qnorm(u[,1], mean=mean(returns1), sd=sd(returns1))
sim_returns2 <- qnorm(u[,2], mean=mean(returns2), sd=sd(returns2))

# plot the simulated returns
plot(sim_returns1, type='l')
plot(sim_returns2, type='l')


