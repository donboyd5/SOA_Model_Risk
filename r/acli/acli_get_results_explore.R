
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


# key filenames for results -----------------------------------------------

## airg filenames documentation ----
# old means not produced by acli

# AGGR.csv Aggressive or specialized equity
# BALANCED.csv Diversified balanced allocation
# FIXED.csv Diversified fixed income
# INT.csv Diversified international equity
# old -- INTGOV.csv U.S. intermediate-term government bonds
# old -- LTCORP.csv U.S. long-term corporate bonds
# SMALL.csv Intermediate risk equity
# US.csv Diversified large cap US equity


## acli csv files ----
(csvfiles <- dir_ls(path=ddata, glob="*.csv") |> 
  path_file() |> 
   sort())

# [1] "AGGR.csv"           "BALANCED.csv"       "CREDIT_SPREADS.csv" "EXCESS_RETURNS.csv" "FIXED.csv"         
# [6] "GOV_1_5.csv"        "GOV_5_10.csv"       "GOV_Long.csv"       "HY.csv"             "IG_1_5.csv"        
# [11] "IG_5_10.csv"        "IG_Long.csv"        "INT.csv"            "MM.csv"             "SMALL.csv"         
# [16] "US.csv"             "UST.csv"            "UST_1.csv"          "UST_10.csv"         "UST_11.csv"        
# [21] "UST_12.csv"         "UST_13.csv"         "UST_14.csv"         "UST_15.csv"         "UST_16.csv"        
# [26] "UST_17.csv"         "UST_18.csv"         "UST_19.csv"         "UST_2.csv"          "UST_20.csv"        
# [31] "UST_21.csv"         "UST_22.csv"         "UST_23.csv"         "UST_24.csv"         "UST_25.csv"        
# [36] "UST_26.csv"         "UST_27.csv"         "UST_28.csv"         "UST_29.csv"         "UST_3.csv"         
# [41] "UST_30.csv"         "UST_3m.csv"         "UST_4.csv"          "UST_5.csv"          "UST_6.csv"         
# [46] "UST_6m.csv"         "UST_7.csv"          "UST_8.csv"          "UST_9.csv"   

# NOTES:
#   different formats for the following files:
#     CREDIT_SPREADS.csv
#       cols: "// Scenario","Month","IG_1_5","IG_5_10","IG_Long","HY","MM","GOV_1_5","GOV_5_10","GOV_Long"
#       rows: 31002 (nsims x (nyears + 1) + 2)
#     EXCESS_RETURNS.csv
#       cols: "// Scenario","Month","IG_1_5","IG_5_10","IG_Long","HY","MM","GOV_1_5","GOV_5_10","GOV_Long"
#       rows: 30002 (nsims x nyears + 2)
#     UST.csv
#       cols: "// Scenario","Year",.25,.5,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30
#       rows: 31002 (nsims x (nyears + 1) + 2)
#   what are they?
#     UST looks like a yield curve


## airg csv files ----
airgdir <- r"(E:\data\soa\airg\50sims)"
(airgfiles <- dir_ls(path=airgdir, glob="*.csv") |> 
   path_file() |> 
   sort())

# [1] "AGGR.csv"                "BALANCED.csv"            "FIXED.csv"               "FundNormalDraws.csv"    
# [5] "INT.csv"                 "InterestNormalDraws.csv" "INTGOV.csv"              "LTCORP.csv"             
# [9] "MONEY.csv"               "SMALL.csv"               "US.csv"                  "UST.csv"                
# [13] "UST_10y.csv"             "UST_1y.csv"              "UST_20y.csv"             "UST_2y.csv"             
# [17] "UST_30y.csv"             "UST_3m.csv"              "UST_3y.csv"              "UST_5y.csv"             
# [21] "UST_6m.csv"              "UST_7y.csv"              "UST_Z1.csv"              "UST_Z2.csv"             
# [25] "UST_Z3.csv" 


## acli files not produced by airg ----
setdiff(csvfiles, airgfiles)
# [1] "CREDIT_SPREADS.csv" "EXCESS_RETURNS.csv" "GOV_1_5.csv"        "GOV_5_10.csv"       "GOV_Long.csv"      
# [6] "HY.csv"             "IG_1_5.csv"         "IG_5_10.csv"        "IG_Long.csv"        "MM.csv"            
# [11] "UST_1.csv"          "UST_10.csv"         "UST_11.csv"         "UST_12.csv"         "UST_13.csv"        
# [16] "UST_14.csv"         "UST_15.csv"         "UST_16.csv"         "UST_17.csv"         "UST_18.csv"        
# [21] "UST_19.csv"         "UST_2.csv"          "UST_20.csv"         "UST_21.csv"         "UST_22.csv"        
# [26] "UST_23.csv"         "UST_24.csv"         "UST_25.csv"         "UST_26.csv"         "UST_27.csv"        
# [31] "UST_28.csv"         "UST_29.csv"         "UST_3.csv"          "UST_30.csv"         "UST_4.csv"         
# [36] "UST_5.csv"          "UST_6.csv"          "UST_7.csv"          "UST_8.csv"          "UST_9.csv"  

## airg files not produced by acli ----
setdiff(airgfiles, csvfiles)
# [1] "FundNormalDraws.csv"     "InterestNormalDraws.csv" "INTGOV.csv"              "LTCORP.csv"             
# [5] "MONEY.csv"               "UST_10y.csv"             "UST_1y.csv"              "UST_20y.csv"            
# [9] "UST_2y.csv"              "UST_30y.csv"             "UST_3y.csv"              "UST_5y.csv"             
# [13] "UST_7y.csv"              "UST_Z1.csv"              "UST_Z2.csv"              "UST_Z3.csv"    


# read data files ----
sfnames <- "
AGGR.csv, Aggressive or specialized equity
BALANCED.csv, Diversified balanced allocation
FIXED.csv, Diversified fixed income
INT.csv, Diversified international equity
SMALL.csv, Intermediate risk equity
US.csv, Diversified large cap US equity"

fnames <- read_csv(sfnames, col_names = c("fname", "fdesc"))

test <- path(ddata, fnames$fname)
df1 <- vroom(test, 
             col_types = cols(.default = col_double()),
             col_names=FALSE, id="fname")
dim(df1)
glimpse(df1)
summary(df1)

df2 <- df1 |> 
  mutate(fname=path_file(fname)) |> 
  left_join(fnames, by = join_by(fname))
summary(df2)

df2 |> 
  summarise(X31=mean(X31), .by=c(fname, fdesc))

df2 |> 
  mutate(cagr=X31^(1/30)-1) |> 
  summarise(assets=mean(X31), cagr=mean(cagr), .by=c(fname, fdesc))


