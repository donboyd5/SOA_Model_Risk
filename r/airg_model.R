
# TODO:
#   


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

runparams$nyears <- 100
runparams$nsims <- 10000


# interest rates ----------------------------------------------------------

## get correlated random numbers ----
a <- proc.time()
arnorm_cor <- intgen_rancorr(nyears=runparams$nyears,
                             nsims=runparams$nsims, runparams, seed=12345)
b <- proc.time()
b - a
cor(c(arnorm_cor[, 1, ]), c(arnorm_cor[, 2, ]))
runparams$ir_generator$correl12


## construct yield curves ----
ncurves <- runparams$nyears * 12 # number of curves PER SCENARIO



# IntScenarioClass class includes the random numbers used, the generated
# short and long rates, and the 10-point interpolated yield curve.

# the VB code generates a single scenario at a time - we'll want to vectorize it
# first, it generates a yield curve for a single scenario





# vb notes parameters ----
# CDbl converts to double





