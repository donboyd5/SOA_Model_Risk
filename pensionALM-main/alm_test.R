
library(tidyverse)

# Set assumptions
salary_growth <- 0.04
benefit_factor <- 0.02
discount_rate <- 0.07
COLA <- 0.02
inflation_rate <- 0.02
amort_period <- 30

# Calculate annuity factor for amortization over 15 years
salary_growth <- 0.04; discount_rate <- 0.07
# salary_growth <- 0.06; discount_rate <- 0.05
growth_annuity <- 
  (1 - 
     ((1 + salary_growth) / (1 + discount_rate))^amort_period) /
  (discount_rate - salary_growth)

assets0 <- 78
liability0 <- 100
payroll0 <- 19
ncrate <- 0.12
benpayrate <- .27
ufl0 <- liability0 - assets0

nyears <- 30
nsims <- 5

mat <- matrix(nrow=nsims, ncol=nyears)

# objects that vary only by year (and thus are vectors)
payroll <-  payroll0 * (1 + salary_growth)^(0:(nyears - 1))
nc <- payroll * ncrate
benefits <- payroll * benpayrate

# objects that vary by year and sim (and thus are matrices)
assets <- liability <- ufl <- amort <- irate <- ii <- contrib <- mat

# initialize
irate[,] <- .07
assets[, 1] <- assets0
liability[, 1] <- liability0
ufl[, 1] <- liability0 - assets0
amort[, 1] <- ufl0 / growth_annuity
contrib[, 1] <- nc[1] + amort[, 1]
ii[, 1] <- assets[, 1] * irate[, 1]

# y <- 3

for(y in 2:nyears){
  # for each year, calc values for all sims at once
  liability[, y] <- liability[, y-1] * (1 + discount_rate) +
    nc[y] - benefits[y]
  
  # note that amortization can be positive or negative
  amort[, y] <- (liability[, y-1] - assets[, y-1]) / growth_annuity
  contrib[, y] <- nc[y] + amort[, y]
  ii[, y] <- assets[, y-1] * irate[, y]
  
  assets[, y] <- assets[, y - 1] + 
    contrib[, y] + 
    ii[, y] -
    benefits[y]
}

assets

funded_ratio <- assets / liability

flatmat <- function(matrix){
  # flatten a matrix with all elements in one row followed by all in next
  c(t(matrix))
}

# create a tibble
df <- expand_grid(sim=1:nsims, year=1:nyears) |> 
  mutate(assets=flatmat(assets),
         liability=flatmat(liability),
         benefits=rep(benefits, nsims),
         payroll=rep(payroll, nsims),
         nc=rep(nc, nsims),
         contrib=flatmat(contrib),
         ii=flatmat(ii),
         irate=flatmat(irate),
         fr=assets / liability)

df
df |> filter(sim==1)

tmp <- df |> filter(sim==1)

tmp$assets - alm$asset
tmp$fr - alm$funded_ratio





# dataframe approach ------------------------------------------------------




alm2 <- tibble(year=1:n,
               asset=0,
               liability=0,
               ufl=0,
               payroll=0,
               benefit_payment=0,
               normal_cost=0,
               amort = 0,
               contribution=0,
               investment_returns=0,
               investment_return_rate=0.07,
               discount_rate=discount_rate) |> 
  # set values known at start
  mutate(asset = replace(asset, 1, asset0),
         liability = replace(liability, 1, liability0),
         ufl = replace(ufl, 1, ufl0),
         payroll = payroll0 * (1 + salary_growth)^(0:(n - 1)),
         normal_cost = payroll * ncrate,
         amort = replace(amort, 1, ufl0 / growth_annuity),
         contribution = normal_cost + amort)
alm2

alm |> 
  mutate(ncr=normal_cost / payroll) |> 
  select(year, ncr)

alm |> 
  mutate(benpct=benefit_payment / payroll) |> 
  select(year, benpct)


