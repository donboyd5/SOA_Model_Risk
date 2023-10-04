

# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "functions_utilities.r"))


# locations ------------------------------------------------------------------
ddir <- r"(E:\data\soa\acli)"
# ddata <- here::here("airg", "data")
(ddata <- path(ddir, "baseline"))

# E:\data\soa\acli\baseline
# baseline_ACLI_v1.46 Alt3 12-2020 100yrs.xlsm


# constants ---------------------------------------------------------------
# airg_fn <- "2022-academy-interest-rate-generator.xls"
acli_fn <- "baseline_ACLI_v1.46 Alt3 12-2020 100yrs.xlsm"

# ONETIME: get params from acli model -----------------------------------------
# where possible, I try to use column names consistent with those that the AIRG
# uses for output files

# AGGR.csv Aggressive or specialized equity
# BALANCED.csv Diversified balanced allocation
# FIXED.csv Diversified fixed income
# INT.csv Diversified international equity
# INTGOV.csv U.S. intermediate-term government bonds
# LTCORP.csv U.S. long-term corporate bonds
# SMALL.csv Intermediate risk equity
# US.csv Diversified large cap US equity


params <- list()

## interest rate generator ----
# a2:f39
ir_generator1 <- read_excel(path(ddata, acli_fn),
                  sheet="Parameters",
                  range = "a2:f39")
ir_generator1
summary(ir_generator1)

ir_generator <- ir_generator1 |> 
  setNames(c("symbol", "name", "used", "default", "junk", "desc")) |> 
  select(-junk) |> 
  filter(!is.na(name)) |> 
  mutate(type=ifelse(is.na(symbol), "extended", "basic")) |> 
  relocate(type)
ir_generator

params$ir_generator <- ir_generator
# djb return to other parameters ----

## bond index returns ----
# h6:l11
# Symbol,	Money	Market, US Intermed	Govt, US Long	Corporate, Description

df1 <- read_excel(path(ddata, airg_fn),
                  sheet="Parameters",
                  range = "h6:l11")
df1
glimpse(df1)

df2 <- df1 |> 
  lcnames() |> 
  rename(money=market,
         intgov=govt,
         ltcorp=corporate)
glimpse(df2)
df2
params$bond_index <- df2

## equity fund returns ----
# Note: we have to calculate mu, the Mean annualized log return

# h17:m28
df1 <- read_excel(path(ddata, airg_fn),
                  sheet="Parameters",
                  range = "h17:m28")
df1
glimpse(df1)

# symbol, US Diversified,	Int'l Diversified,	Intermed Risk,	Aggressive / Exotic, Description
df2 <- df1 |> 
  setNames(c("symbol", "us", "int", "intrisk", "aggr", "description"))

# calc mu, the Mean annualized log return
# calculation is stated as:
#   mu	= A + B*sigma + C*sigma*sigma	
# CAUTION: I am not yet sure if I am using the right sigma variable for this
# the choices are:
# sigma(0), Starting volatility
# sigma-, Minimum volatility (annualized)
# sigma+, Maximum volatility (annualized, before random component)
# sigma*, Maximum volatility (annualized, after random component)

# choose one of the following sigmas
sigmas <- c("sigma(v)", "sigma(0)", "sigma-", "sigma+", "sigma*")
mu <- df2 |> 
  select(-description) |> 
  pivot_longer(-symbol) |> 
  pivot_wider(names_from = symbol) |> 
  mutate(sigma=.data[[sigmas[2]]], # CAUTION: is this the correct sigma to use??)
         mu=A + B*sigma + C*sigma*sigma	) |> 
  select(name, mu) |> 
  pivot_wider(values_from = mu) |> 
  mutate(symbol="mu_calc", description="Boyd: Mean annualized log return, mu = A + B*sigma + C*sigma*sigma")
mu

df3 <- bind_rows(df2, mu)
df3

params$equity_fund <- df3

## stock correlation matrix ----
# h45:s56
df1 <- read_excel(path(ddata, airg_fn),
                  sheet="Parameters",
                  range = "h45:s56")
# dimnames <- df1 |> pull(1)

stock_cormat <- as.matrix(df1[, 2:ncol(df1)])
rownames(stock_cormat) <- colnames(stock_cormat)
stock_cormat

params$stock_cormat <- stock_cormat

## Mean reversion parameters (MRP) for AAA and NAIC ----
### AAA first
aaa1 <- read_excel(path(ddata, airg_fn),
                   sheet="MRP",
                   range = "c6:d8")
aaa <- aaa1 |> 
  mutate(type="AAA")

### NAIC
naic1 <- read_excel(path(ddata, airg_fn),
                    sheet="MRP",
                    range = "c11:d14")
naic <- naic1 |> 
  mutate(type="NAIC")

mrp <- bind_rows(aaa, naic) |> 
  lcnames() |> 
  select(type, weight, rate)
mrp

params$mrp <- mrp

## wrap up ----
names(params)
str(params)
params
saveRDS(params, path(ddata, "params_airg.rds"))


# ONETIME: get data from airg model -----------------------------------------
data <- list()

## Historical yield curves ----
# sheet Historical Curves
# a2:f25
df1 <- read_excel(path(ddata, airg_fn),
                  sheet="Historical Curves",
                  range = "a4:m830")
df1
glimpse(df1)
ht(df1)

# get better column names for the yields
(yields <- names(df1)[-c(1:3)])
(maturities <- str_remove(yields, " UST") |> str_remove("y"))
(newyields=paste0("UST_", maturities))

df2 <- df1 |> 
  filter(!is.na(Year)) |> 
  setNames(c("year", "month", "curvenum", newyields)) |> 
  mutate(date=ymd(paste(year, month, "1"))) |> 
  select(date, curvenum, starts_with("UST"))
df2
ht(df2)

data$hist_curves <- df2


## wrap up ----

saveRDS(data, path(ddata, "data_acli.rds"))
