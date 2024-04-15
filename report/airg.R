
library(MASS)
library(tidyverse)
library(vroom)
library(readxl)
library(openxlsx) # for writing xlsx files
library(lubridate)
library(RcppRoll)
library(mvtnorm)




# set up matrices

mu <- c(0, 0)
sigma <- matrix(c(1, params$rho, params$rho, 1), nrow = 2, ncol = 2)
sigma
x <- rmvnorm(n = 100, mean = mu, sigma = sigma)
x

library(mvnfast)
mu <- c(0, 0)
sigma <- matrix(c(1, params$rho, params$rho, 1), nrow = 2, ncol = 2)
sigma
x <- rmvn(n = 100, mu = mu, sigma = sigma)



# get equity fund parameters ----------------------------------------------------------

fname <- here::here("data", "airg_by_hand.xlsx")
params1 <- read_excel(fname,
                      sheet="params",
                      range="A3:H14")

params2 <- params1 |>
  select(symname, us, int, small, aggr) |> 
  pivot_longer(-symname) |> 
  pivot_wider(names_from = symname) |> 
  mutate(target_logvol=log(tau))


# initialize matrices
params <- params2 |> 
  filter(name=="us") 

get_period <- function(matrices, period, params){
  
  # only call for periods > 1 !!
  # matrix rows are sims, columns are periods
  
  # get this period's correlated random normal shocks as a matrix with nsims rows and 2 columns
  mu <- c(0, 0)
  sigma <- matrix(c(1, params$rho, params$rho, 1), nrow = 2, ncol = 2)
  shocks <- rmvn(n = nrow(volatility), mu = mu, sigma = sigma)
  colnames(shocks) <- c("vol_shock", "return_shock")
  
  # update volatility
  currLogVol <- log(volatility[, period - 1])
  currLogVol <- (1 - params$phi) * currLogVol + (params$phi * params$target_logvol)
  currentVol <- exp(currLogVol)
  currentVol <- pmin(params$sigma_max_before, currentVol)
 
  # update log volatility
  currLogVol <- log(currentVol)
  currLogVol <- currLogVol + shocks[, "vol_shock"] * params$sigma_v

  # final updating of volatility
  currentVol <- exp(currLogVol)
  currentVol <- pmin(params$sigma_max_after, currentVol)
  currentVol <- pmax(params$sigma_min, currentVol)

  nextMeanReturn <- params$A + params$B * currentVol + params$C * currentVol^2
  # convert to monthly
  nextReturn <- (nextMeanReturn / 12L) + shocks[, "return_shock"] * (currentVol / (12L ^ 0.5))
  nextLogReturn <- exp(nextReturn) - 1L
  
  matrices$volatility[, period] <- currentVol
  matrices$logreturns[, period] <- nextLogReturn
  
  return(matrices)
  }

# volatility <- get_period(volatility, returns, period=3, params)

nperiods <- 12*30
nsims <- 5000

volatility <- matrix(0, nrow = nsims, ncol=nperiods)
logreturns <- matrix(0, nrow = nsims, ncol=nperiods)

# period 1 values
volatility[, 1] <- params$sigma_0
logreturns[, 1] <- params$A
volatility
logreturns

matrices <- mget(c("volatility", "logreturns"))
matrices
# get_period(matrices, period=2, params)



a <- proc.time()
for(period in 2:nperiods){
  matrices <- get_period(matrices, period=period, params)
}
b <- proc.time()
b - a

matrices$volatility[1:10, 1:5]
matrices$logreturns[1:10, 1:5]



nsims <- 10

# targetVol	0.12515	tau	long run target
# meanRevStrength	0.35229	phi	
# volStdDev	0.32645	sigma(v)	monthly sd of log volatility
# ??	-0.2488	rho	used for rng
# a	0.055	A	stock ret at zero volatiility (annual?)
# b	0.56	B	coeff, quadratic
# c	-0.9	C	coeff, quadratic
# currentVol (initial)	0.1476	sigma(0)	
# minVol	0.0305	sigma-	
# maxVolBefore	0.3	sigma+	
# maxVolAfter	0.7988	sigma*	

tau	<- 0.12515 # long run target
phi	<- 0.35229 # mean reversion strength
sigma_v <- 0.32645 # monthly sd of log volatility
rho	<- -0.2488 # for rng
A1	<- 0.0550 
B1	<- 0.56
C1 <- -0.9
sigma_0 <- 0.1476
sigma_min	<- 0.0305
sigma_max_before <- 0.3
sigma_max_after <- 0.7988



# initialize
returnShock <- 0
volShock <- 0


params2 |> 
  filter(name=="us") |> 
  as.list()

params <- params1 |>
  select(symname, us) |> 
  pivot_wider(names_from = symname,
              values_from = us) |> 
  mutate(target_logvol=log(tau))


get_next_return <- function(returnShock, 
                            volShock,
                            priorVol,
                            params){
  
  currLogVol <- log(priorVol)
  targetLogVol <- log(params$tau)
  
  # Update the volatility
  currLogVol <- (1 - params$phi) * currLogVol + (params$phi * targetLogVol)
  currentVol <- exp(currLogVol)
  currentVol <- pmin(params$sigma_max_before, currentVol)
  
  currLogVol <- log(currentVol)
  currLogVol <- currLogVol + volShock * params$sigma_v
  
  currentVol <- exp(currLogVol)
  currentVol <- pmin(params$sigma_max_after, currentVol)
  currentVol <- pmax(params$sigma_min, currentVol)
  
  nextMeanReturn <- params$A + params$B * currentVol + params$C * currentVol^2
  # convert to monthly
  nextReturn <- (nextMeanReturn / 12L) + returnShock * (currentVol / (12L ^ 0.5))
  nextLogReturn <- exp(nextReturn) - 1L
  
  return(tibble(nextLogReturn=nextLogReturn, currentVol=currentVol))
}

params <- params2 |> 
  filter(name=="us") 




  
  
  
nsims <- 10
nperiods <- 5

df <- crossing(sim=1:nsims, period=1:nperiods) |> 
  mutate(period=paste0("p", str_pad(period, width=5, side="left", pad="0")),
         returnShock=rnorm(nsims * nperiods),
         volShock=rnorm(nsims*nperiods)) |> 
  pivot_longer()
  pivot_wider(names_from = period)
df

df <- tibble(returnShock=c(0, .02, .03),
             volShock=c(0, -0.01, 0.02),
             priorVol=c(0.1476, 0.1476, 0.1476))

df2 <- df |> 
  mutate(get_next_return(returnShock, volShock, priorVol, params))

df2

df2 |> 
  unnest_wider(col = nextr)

get_next_return(returnShock=0.01, 
                volShock=0.01,
                priorVol=0.1476,
                params)


f <- function(x, y){
  xpy <- x + y
  xtimesy <- x * y
  return(tibble(xpy, xtimesy))
}

df <- tibble(x=1:4, y=5:8) |> 
  mutate(f(x, y))

df |> 
  unnest(z)

# pmax(1:5, 3)

# Public targetVol As Double
# Public currentVol As Double
# Public minVol As Double
# Public maxVolBefore As Double
# Public maxVolAfter As Double
# Public meanRevStrength As Double
# Public volStdDev As Double
# Public a As Double
# Public b As Double
# Public C As Double
# 
# Public SETmedianReturn As Double
# Public SETvolatility As Double
# 
# Public Function getNextReturn(returnShock As Double, volShock As Double) As Double
# Dim currLogVol As Double
# Dim targetLogVol As Double
# Dim nextReturn As Double
# Dim nextMeanReturn As Double
# 
# currLogVol = Log(currentVol)
# targetLogVol = Log(targetVol)
# 
# 'First update the volatility
#   currLogVol = (1 - meanRevStrength) * currLogVol + (meanRevStrength * targetLogVol)
#   currentVol = Exp(currLogVol)
#   currentVol = Application.WorksheetFunction.min(maxVolBefore, currentVol)
#   currLogVol = Log(currentVol)
#   
#   currLogVol = currLogVol + volShock * volStdDev
#   currentVol = Exp(currLogVol)
#   currentVol = Application.WorksheetFunction.min(maxVolAfter, currentVol)
#   currentVol = Application.WorksheetFunction.max(minVol, currentVol)
#   
#   'Next compute the log return based on the volatility and shock
# nextMeanReturn = a + b * currentVol + C * currentVol * currentVol
# nextReturn = (nextMeanReturn / 12#) + returnShock * (currentVol / (12# ^ 0.5))
#               
#               getNextReturn = Exp(nextReturn) - 1#
#               
#               End Function