
fmod3 <- function(anorm, correl13, const2, const3){
  # anorm is a 3-dimension array:
  # array dimensions are [ncurves=nyears x 12 curves, 3 rnorms, nsims]
  # return a vector of revised values for the third random number for a yield curve
  
  # here is the AIRG code
  # randNums(i, 3) = randNums(i, 1) * correl13 + 
  #   randNums(i, 2) * const2 + 
  #   randNums(i, 3) * const3
  anorm[, 1, ] * correl13 +
    anorm[, 2, ] * const2 + 
    anorm[, 3, ] * const3
}

fmod2 <- function(anorm, correl12, const1){
  # return a vector of revised values for the second random number for a yield curve
  # array dimensions are [ncurves=nyears x 12 curves, 3 rnorms, nsims]
  
  # here is the AIRG code
  # randNums(i, 2) = randNums(i, 1) * correl12 +
  #   randNums(i, 2) * const1
  anorm[, 1, ] * correl12 +
    anorm[, 2, ] * const1
}


intgen_rancorr <- function(nyears, nsims, params, seed=1234)
{
  # generate 3 correlated random numbers per yield curve per scenario
  # follow the code in the AIRG spreadsheet model, except that I create
  # all random variables for all sims in one fell swoop, for efficieny reasons
  # (as long as we have enough memory)
  
  # we will have 12 yield curves per year, 1 per month
  
  # note that we will be creating
  # nyears x 12 x 3 x nsims random variables
  # for a full run this could be
  # 100 years x 12 months x 3 sets of rnorms x 10000 scenarios
  # = 36 million elements, x 8 bytes * 1000 / 1024= ~ 281 megabytes
  # not huge, but worth being careful about
  # for now I do NOT reuse arnorm but if memory becomes an issue, we might do so
  
  ncurves <- nyears * 12
  
  # base random number generator
  # set.seed(seed)
  # arnorm <- rnorm(ncurves * 3 * nsims) # vector of random normal variables
  
  # fast random number generation
  # https://cran.r-project.org/web/packages/dqrng/vignettes/dqrng.html
  # dqset.seed(seed)
  # arnorm <- dqrng::dqrnorm(ncurves * 3 * nsims) # vector of random normal variables
  
  # https://cran.r-project.org/web/packages/RcppZiggurat/RcppZiggurat.pdf
  zsetseed(seed) 
  arnorm <- RcppZiggurat::zrnorm(ncurves * 3 * nsims) # vector of random normal variables
  dim(arnorm) <- c(ncurves, 3, nsims) # reshape to the form we need
  
  # now correlate the random numbers
  # "unpack" desired correlation values and constants
  correl12 <- params$ir_generator$correl12
  correl13 <- params$ir_generator$correl13
  const1 <- params$ir_generator$const1
  const2 <- params$ir_generator$const2
  const3 <- params$ir_generator$const3
  # here are the default correlations and their meanings
  # Correl12	-0.19197 Correlation of shocks to long rate and slope (long - short)
  # Correl13	0		Correlation of shocks to long rate and volatility
  # Correl23	0		Correlation of shocks to slope and volatility
  
  # the constants are calculated in get_parameters and are based on
  #  the correlations and other interest rate parameters
  
  # make a copy of the original array - if mem an issue, instead could modify in place
  arnorm_cor <- arnorm 
  # first calculate the 3rd rnorm values, therefore always using the original
  #  values for the 1st and 2nd rnorm values
  #  then the 2nd rnorm values, using the new 3rd rnorm values (if they have been revised)
  #  although fmod2 does not currently use the 3rd rnorms as inputs
  arnorm_cor[, 3, ] <- fmod3(arnorm_cor, correl13, const2, const3)
  arnorm_cor[, 2, ] <- fmod2(arnorm_cor, correl12, const1)
  
  # return the correlated array
  arnorm_cor
}
