
# TODO: ----
#   [DONE] pluck initial yield curve from the data esp short and long values
#   put maturities into params
#   put initial yield curve, as vector, into param


# ONETIME: routines that should have been run before this --------------

airg_fn <- "2022-academy-interest-rate-generator.xls" # used in the ONETIME file
# run airg_read_default_parameters_and_data.r to save default data and parameters
# see CAUTION in that file, re mu calculation for equities


# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))


# locations ------------------------------------------------------------------
ddata <- here::here("airg", "data")


# constants ---------------------------------------------------------------



# functions ---------------------------------------------------------------

source(here::here("r", "airg_utilities.r"))
source(here::here("r", "airg_get_parameters.r"))
source(here::here("r", "airg_correlated_random.r"))

# get data ----------------------------------------------------------------
params <- readRDS(path(ddata, "params_airg.rds"))
data <- readRDS(path(ddata, "data_airg.rds"))
names(params)
names(data)


# model overview ----------------------------------------------------------
# my R status is in [] brackets

# examine starting date -- month <= 12, year >= 1954
# make sure output file size is not too big!
# If (StartingCurveOK) Then Call ProcessAllScenarios

# Public Sub ProcessAllScenarios() in newgenerator
#  get output file information, including projection frequency (monthly, qtr, semi, or annual)

#  Update the mean reversion point based on the scenario start date
#    Call MRP_Update [done]
#    Retrieve the NAIC Mean Reversion Point from the MRP worksheet (already calculated)
#    put into parameters worksheet

#  Retrieve the parameters of the generator
#    Call GetParameters [done]

#  Loop through scenarios -- this is the heart of the model
#    Generate in the IntScenarioClass generates a single interest rate scenario
#      will gen an interest rate curve for each month of each year
#      generate 3 sets of uncorrelated random numbers for each curve [done]
#      then construct correlated versions [done]
#      then yield curves I think
#    fundScenario.Generate looks like it generates a single equity scenario

#  Write results and clean up


# implementation ----------------------------------------------------------


runparams <- get_parameters(data=data, 
                            params=params, 
                            start_date="2021-12-01")
runparams
names(runparams)

runparams$nyears <- 30
runparams$nsims <- 10

# runparams$nyears <- 100
# runparams$nsims <- 10000



# play --------------------------------------------------------------------

tmp <- runparams$init_curve




# interest rates ----------------------------------------------------------

## get correlated random numbers ----
a <- proc.time()
arnorm_cor <- intgen_rancorr(nyears=runparams$nyears,
                             nsims=runparams$nsims, runparams, seed=1234)
b <- proc.time()
b - a
cor(c(arnorm_cor[, 1, ]), c(arnorm_cor[, 2, ]))
runparams$ir_generator$correl12


## construct yield curves ----
### begin by constructing the initial yield curve ----
# get 2 points on the yield curve -- 1 and 20 years
# using Nielsen-Siegel method, generate a yield curve from that
# compare to actual initial yield curve
# perturb moves an NS estimate toward actual by a portion of the difference
#   all the way, in the case of the initial curve
#   AND it sets a floor for all rates in the curve of 0.0001

# NOTE that this uses no randomness and is the same for 10,000 scenarios
# but I think airg does it in each scenario

ncurves <- runparams$nyears * 12 # number of curves PER SCENARIO
runparams$init_curve
rate_short <- runparams$init_curve$UST_1 # 1-year rate on start_date
rate_long <- runparams$init_curve$UST_20 # 20-year rate on start_date
logvol <- log(runparams$ir_generator$initialvol)
genrate1 <- max(rate_short, 0.0001)
genrate2 <- max(rate_long, 0.0001)

icurve <- runparams$init_curve |> dplyr::select(-c(date, curvenum)) |> as.numeric()
runparams$init_curve_NS <- interpolateNS(r1=genrate1, r20=genrate2)0
runparams$init_curve_NS_perturbed <- perturb(icurve, runparams$init_curve_NS)

### construct future yield curve ----
# get previously generated random numbers - already done
# initialize prior-month rates
oldShortRate <- rate_short
oldLogLongRate <- log(rate_long) # notice the log
oldDiff <- rate_long - rate_short
oldLogVol <- logvol
# With curves(0)
# oldShortRate = .rateAtIndex(3)
# oldLogLongRate = Log(.rateAtIndex(9))
# oldDiff = .rateAtIndex(9) - .rateAtIndex(3)
# oldLogVol = .logVolatility
# End With


# 'Initialize the soft cap and floor on the log long rate
# note that airg var names use r1 for long and r2 for short, oddly
minLogLongRate <- log(runparams$ir_generator$minr1) # log soft floor on long rate before shock
maxLogLongRate <- log(runparams$ir_generator$maxr1) # # log soft cap on long rate before shock
minShortRate <- runparams$ir_generator$minr2


# Generate future yield curves *****************************
  
# First initialize the prior month rates


# 'Initialize the soft cap and floor on the log long rate
#   minLogLongRate = Log(Range("Minr1").Value)
#   maxLogLongRate = Log(Range("Maxr1").Value)
#   minShortRate = Range("Minr2").Value
  


perturb <- function(init_curve_hist, init_curve_NS, portion=1){
  # AIRG calls hist minus NS "initialCurveFit" but I give a different name
  # Note that when portion=1, as it does for the startup, then
  # the revised curve is the same as the historical curve 
  diff <- init_curve_hist - init_curve_NS
  revised <- init_curve_NS + portion * diff
  revised
}

# interpolatedRates(i) = interpolatedRates(i) + portion * adj(i)
# 'Now enforce no negative interest rates
#     If (interpolatedRates(i) < 0.0001) Then interpolatedRates(i) = 0.0001
# 
# perturb(initialCurveFit, 1#)

tibble(maturity=maturities, 
       init=icurve, 
       ns=runparams$init_curve_NS, 
       perturb=runparams$init_curve_NS_perturbed) |> 
  pivot_longer(-maturity) |> 
  ggplot(aes(maturity, value, colour=name)) +
  geom_line(aes(linetype=name), linewidth=1)


interpolateNS <- function(r1, r20){
  # Nelson-Siegel two point interpolation 
  
  # According to ChatGPT (not sure I understand this)
  # Nelson-Siegel two-point interpolation fits a curve to a set of points to
  # estimate the yield curve. It uses three parameters (lambda, beta1, and
  # beta2) to fit a curve to the points on the yield curve. The curve is
  # constructed by adding together two exponential functions, each of which is
  # controlled by one of the beta parameters. The lambda parameter controls the
  # overall shape of the curve, while the beta1 and beta2 parameters control the
  # slope of the curve at different points in time.
  
  maturities <- c(0.25, 0.5, 1, 2, 3, 5, 7, 10, 20, 30)
  
  k <- 0.4
  const1 <- (1 - exp(-k * 1)) / (k * 1)
  const20 <- (1 - exp(-k * 20)) / (k * 20)
  
  b1 <- (r1 - r20) / (const1 - const20)
  b0 <- r1 - (b1 * const1)
  
  t <- ifelse(maturities==0, 0.25, maturities)
  interp_NS <- b0 + b1 * (1 - exp(-k * t)) / (k * t)
  interp_NS
}


# initialize yield curve before looping through scenarios
# vb code is below
# Init_Rate_Short is the 1-year rate on start_date in Scenario Generator tab
# Init_Rate_Long is the 20-year rate
# InitialVol is D25 in parameters sheet, runparams$ir_generator$initialvol


# vb code:
# '***************************************** floor the generated rates at 0.0001 = 0.01% *****************
#   generatedRates(1) = max(shortRate, 0.0001)
#   generatedRates(2) = max(longRate, 0.0001)
#   logVolatility = logVol
# '***************************************** floor the generated rates at 0.0001 = 0.01% *****************
  
# spotRatesAvailable = False

# IntScenarioClass class includes the random numbers used, the generated
# short and long rates, and the 10-point interpolated yield curve.

# the VB code generates a single scenario at a time - we'll want to vectorize it
# first, it generates a yield curve for a single scenario


# vb notes parameters ----





