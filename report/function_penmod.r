
# function to compute pension model for a set of sims

default_params <- within(list(),{
  salary_growth = 0.04
  benefit_factor = 0.02
  discount_rate = 0.07
  COLA = 0.02
  inflation_rate = 0.02
  amort_period = 30
  
  # Calculate annuity factor for amortization over 15 years
  salary_growth = 0.04
  discount_rate = 0.07
  growth_annuity = 
    (1 - 
       ((1 + salary_growth) / (1 + discount_rate))^amort_period) /
    (discount_rate - salary_growth)
  
  assets0 = 78
  liability0 = 100
  payroll0 = 19
  ncrate = 0.12
  benpayrate = .27
  ufl0 = liability0 - assets0
})


penmod <- function(
    irates=.07,
    nsims=5,
    nyears=10,
    params=default_params){
  
  # define objects that vary by year and sim (and thus are matrices)
  mat <- matrix(nrow=nsims, ncol=nyears)
  assets <- liability <- ufl <- amort <- ir <- ii <- contrib <- mat
  
  # fill the ir matrix based on values of irates
  if(length(irates) == 1 & is.vector(irates)) { # scalar
    ir[,] <- irates
  } else if(is.vector(irates) & length(dim(irates)) == 1) { # vector
    # assume it has one rate per year, not varying by sim
    # TODO: VERIFY that it has the right number of years
    ir <-  t(replicate(nsims, irates))
  } else if(is.matrix(irates)) {
    # TODO: verify size of irates
    ir <- irates[1:nsims, 1:nyears]
  } else {
    stop("irates is not correct")
  }
  
  # objects that vary only by year (and thus are vectors)
  payroll <-  params$payroll0 * (1 + params$salary_growth)^(0:(nyears - 1))
  nc <- payroll * params$ncrate
  benefits <- payroll * params$benpayrate
  
  # initialize
  assets[, 1] <- params$assets0
  liability[, 1] <- params$liability0
  ufl[, 1] <- params$liability0 - params$assets0
  amort[, 1] <- params$ufl0 / params$growth_annuity
  contrib[, 1] <- nc[1] + amort[, 1]
  ii[, 1] <- assets[, 1] * ir[, 1]
  
  for(y in 2:nyears){
    # for each year, calc values for all sims at once
    liability[, y] <- liability[, y-1] * (1 + params$discount_rate) +
      nc[y] - benefits[y]
    
    # note that amortization can be positive or negative
    amort[, y] <- (liability[, y-1] - assets[, y-1]) / params$growth_annuity
    contrib[, y] <- nc[y] + amort[, y]
    ii[, y] <- assets[, y-1] * ir[, y]
    
    assets[, y] <- assets[, y - 1] + 
      contrib[, y] + 
      ii[, y] -
      benefits[y]
  }
  
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
           ir=flatmat(ir),
           fr=assets / liability)
  df
}


# res <- penmod(nsims=10000, nyears = 50)
# res <- penmod(nyears=30)
# 
# res |> filter(sim==1)
 