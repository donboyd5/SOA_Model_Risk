

# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))


# locations ---------------------------------------------------------------

# dsims <- r"(E:\data\soa\airg\1000sims)"
dnaicsims <- r"(E:\data\soa\naic)"


# get data ----------------------------------------------------------------

fnz <- "1000_path_subset_1a_conning_gff_baseline_equity_123121.zip"
fn <- "1000_Path_Subset_1a_Conning_GFF_Baseline_Equity_123121.csv"

# 1.2 m rows for 1000 scenarios means 1200 time periods, 
# which implies 12 months for 100 years
# we need to convert to annual data



df1 <- vroom(path(dnaicsims, fnz), n_max=1201)

df1 <- vroom(path(dnaicsims, fnz))


df2 <- df1 |> 
  select(sim=Scen, yearmo=TIME, 
         lcap_price=`Large Cap Price`,
         lcap_income=`Large Cap Income`)

df3 <- df2 |> 
  mutate(month = ((yearmo - 1) %% 12) + 1,
         # factor=ifelse(yearmo==0, 1, 1 + lcap_price / 12),
         # lcapprice_accum=cumprod(factor),
         # factor=ifelse(yearmo==0, 1, 1 + lcap_income / 12),
         # lcapincome_accum=cumprod(factor),
         # assetval=ifelse(yearmo==0, 1, NA_real_),
         lcap_return=lcap_price + lcap_income,
         factor=ifelse(yearmo==0, 1, 1 + lcap_return / 12),
         factor2=ifelse(yearmo==0, 1, 1 + lcap_return)) |> 
  group_by(sim) |> 
  arrange(yearmo) |> 
  mutate(lcap_accum=cumprod(factor),
         lcap_accum2=cumprod(factor2)) |> 
  ungroup() |> 
  arrange(sim, yearmo)

df4 <- df3 |> 
  filter(yearmo==360)
summary(df4)
  
df3a <- df2 |> 
  arrange(sim, yearmo) |> 
  mutate(year=((yearmo - 1) %/% 12) + 1,
         lcap_return=lcap_price + lcap_income) |> 
  summarise(lcap_return1=sum(lcap_return), 
            lcap_return2=(cumprod(1+lcap_return) - 1)[n()] ,
            .by=c(sim, year))
summary(df3a)
sd(df3a$lcap_return1)
sd(df3a$lcap_return2)
quantile(df3a$lcap_return2, c(.05, seq(0, 1, .1), .95) |> sort())
 
rets <- c(.01, .03, .11, .08, .09)
cumprod(1 + rets)

# column numbers and variable names ----
# 1	Scen
# 2	TIME
# 3	US Treasury 1 Month Yield - Spot
# 4	US Treasury 3 Month Yield - Spot
# 5	US Treasury 6 Month Yield - Spot
# 6	US Treasury 1 Year Yield - Spot
# 7	US Treasury 2 Year Yield - Spot
# 8	US Treasury 3 Year Yield - Spot
# 9	US Treasury 4 Year Yield - Spot
# 10	US Treasury 5 Year Yield - Spot
# 11	US Treasury 6 Year Yield - Spot
# 12	US Treasury 7 Year Yield - Spot
# 13	US Treasury 8 Year Yield - Spot
# 14	US Treasury 9 Year Yield - Spot
# 15	US Treasury 10 Year Yield - Spot
# 16	US Treasury 11 Year Yield - Spot
# 17	US Treasury 12 Year Yield - Spot
# 18	US Treasury 13 Year Yield - Spot
# 19	US Treasury 14 Year Yield - Spot
# 20	US Treasury 15 Year Yield - Spot
# 21	US Treasury 16 Year Yield - Spot
# 22	US Treasury 17 Year Yield - Spot
# 23	US Treasury 18 Year Yield - Spot
# 24	US Treasury 19 Year Yield - Spot
# 25	US Treasury 20 Year Yield - Spot
# 26	US Treasury 21 Year Yield - Spot
# 27	US Treasury 22 Year Yield - Spot
# 28	US Treasury 23 Year Yield - Spot
# 29	US Treasury 24 Year Yield - Spot
# 30	US Treasury 25 Year Yield - Spot
# 31	US Treasury 26 Year Yield - Spot
# 32	US Treasury 27 Year Yield - Spot
# 33	US Treasury 28 Year Yield - Spot
# 34	US Treasury 29 Year Yield - Spot
# 35	US Treasury 30 Year Yield - Spot
# 36	US Treasury 1 Month Yield - Coupon
# 37	US Treasury 3 Month Yield - Coupon
# 38	US Treasury 6 Month Yield - Coupon
# 39	US Treasury 1 Year Yield - Coupon
# 40	US Treasury 2 Year Yield - Coupon
# 41	US Treasury 3 Year Yield - Coupon
# 42	US Treasury 4 Year Yield - Coupon
# 43	US Treasury 5 Year Yield - Coupon
# 44	US Treasury 6 Year Yield - Coupon
# 45	US Treasury 7 Year Yield - Coupon
# 46	US Treasury 8 Year Yield - Coupon
# 47	US Treasury 9 Year Yield - Coupon
# 48	US Treasury 10 Year Yield - Coupon
# 49	US Treasury 11 Year Yield - Coupon
# 50	US Treasury 12 Year Yield - Coupon
# 51	US Treasury 13 Year Yield - Coupon
# 52	US Treasury 14 Year Yield - Coupon
# 53	US Treasury 15 Year Yield - Coupon
# 54	US Treasury 16 Year Yield - Coupon
# 55	US Treasury 17 Year Yield - Coupon
# 56	US Treasury 18 Year Yield - Coupon
# 57	US Treasury 19 Year Yield - Coupon
# 58	US Treasury 20 Year Yield - Coupon
# 59	US Treasury 21 Year Yield - Coupon
# 60	US Treasury 22 Year Yield - Coupon
# 61	US Treasury 23 Year Yield - Coupon
# 62	US Treasury 24 Year Yield - Coupon
# 63	US Treasury 25 Year Yield - Coupon
# 64	US Treasury 26 Year Yield - Coupon
# 65	US Treasury 27 Year Yield - Coupon
# 66	US Treasury 28 Year Yield - Coupon
# 67	US Treasury 29 Year Yield - Coupon
# 68	US Treasury 30 Year Yield - Coupon
# 69	Int Govt Bonds Price
# 70	Int Govt Bonds Income
# 71	Long Inv Corp Bonds Price
# 72	Long Inv Corp Bonds Income
# 73	Money Market Price 
# 74	Money Market Income
# 75	Short Govt Bonds Price
# 76	Short Govt Bonds Income
# 77	Long Govt Bonds Price
# 78	Long Govt Bonds Income
# 79	Short Inv Corp Bonds Price 
# 80	Short Inv Corp Bonds Income
# 81	High Yield Corp Bonds Price
# 82	High Yield Corp Bonds Income
# 83	Int Inv Corp Bonds Price
# 84	Int Inv Corp Bonds Income
# 85	Large Cap Price
# 86	Large Cap Income
# 87	Small Cap Price
# 88	Small Cap Income
# 89	Aggressive US Equity Price
# 90	Aggressive US Equity Income
# 91	Mid Cap Price
# 92	Mid Cap Income
# 93	Aggressive Foreign Equity Price
# 94	Aggressive Foreign Equity Income
# 95	International Diversified Equity Price
# 96	International Diversified Equity Income
# 97	Aggressive Equity Price
# 98	Aggressive Equity Income
# 99	Diversified Fixed Income Price
# 100	Diversified Fixed Income Income
# 101	Diversified Balanced Allocation Price
# 102	Diversified Balanced Allocation Income


# Scen,TIME,US Treasury 1 Month Yield - Spot,US Treasury 3 Month Yield - Spot,US
# Treasury 6 Month Yield - Spot,US Treasury 1 Year Yield - Spot,US Treasury 2
# Year Yield - Spot,US Treasury 3 Year Yield - Spot,US Treasury 4 Year Yield -
# Spot,US Treasury 5 Year Yield - Spot,US Treasury 6 Year Yield - Spot,US
# Treasury 7 Year Yield - Spot,US Treasury 8 Year Yield - Spot,US Treasury 9
# Year Yield - Spot,US Treasury 10 Year Yield - Spot,US Treasury 11 Year Yield -
# Spot,US Treasury 12 Year Yield - Spot,US Treasury 13 Year Yield - Spot,US
# Treasury 14 Year Yield - Spot,US Treasury 15 Year Yield - Spot,US Treasury 16
# Year Yield - Spot,US Treasury 17 Year Yield - Spot,US Treasury 18 Year Yield -
# Spot,US Treasury 19 Year Yield - Spot,US Treasury 20 Year Yield - Spot,US
# Treasury 21 Year Yield - Spot,US Treasury 22 Year Yield - Spot,US Treasury 23
# Year Yield - Spot,US Treasury 24 Year Yield - Spot,US Treasury 25 Year Yield -
# Spot,US Treasury 26 Year Yield - Spot,US Treasury 27 Year Yield - Spot,US
# Treasury 28 Year Yield - Spot,US Treasury 29 Year Yield - Spot,US Treasury 30
# Year Yield - Spot,US Treasury 1 Month Yield - Coupon,US Treasury 3 Month Yield
# - Coupon,US Treasury 6 Month Yield - Coupon,US Treasury 1 Year Yield -
# Coupon,US Treasury 2 Year Yield - Coupon,US Treasury 3 Year Yield - Coupon,US
# Treasury 4 Year Yield - Coupon,US Treasury 5 Year Yield - Coupon,US Treasury 6
# Year Yield - Coupon,US Treasury 7 Year Yield - Coupon,US Treasury 8 Year Yield
# - Coupon,US Treasury 9 Year Yield - Coupon,US Treasury 10 Year Yield -
# Coupon,US Treasury 11 Year Yield - Coupon,US Treasury 12 Year Yield -
# Coupon,US Treasury 13 Year Yield - Coupon,US Treasury 14 Year Yield -
# Coupon,US Treasury 15 Year Yield - Coupon,US Treasury 16 Year Yield -
# Coupon,US Treasury 17 Year Yield - Coupon,US Treasury 18 Year Yield -
# Coupon,US Treasury 19 Year Yield - Coupon,US Treasury 20 Year Yield -
# Coupon,US Treasury 21 Year Yield - Coupon,US Treasury 22 Year Yield -
# Coupon,US Treasury 23 Year Yield - Coupon,US Treasury 24 Year Yield -
# Coupon,US Treasury 25 Year Yield - Coupon,US Treasury 26 Year Yield -
# Coupon,US Treasury 27 Year Yield - Coupon,US Treasury 28 Year Yield -
# Coupon,US Treasury 29 Year Yield - Coupon,US Treasury 30 Year Yield -
# Coupon,Int Govt Bonds Price,Int Govt Bonds Income,Long Inv Corp Bonds
# Price,Long Inv Corp Bonds Income,Money Market Price ,Money Market Income,Short
# Govt Bonds Price,Short Govt Bonds Income,Long Govt Bonds Price,Long Govt Bonds
# Income,Short Inv Corp Bonds Price ,Short Inv Corp Bonds Income,High Yield Corp
# Bonds Price,High Yield Corp Bonds Income,Int Inv Corp Bonds Price,Int Inv Corp
# Bonds Income,Large Cap Price,Large Cap Income,Small Cap Price,Small Cap
# Income,Aggressive US Equity Price,Aggressive US Equity Income,Mid Cap
# Price,Mid Cap Income,Aggressive Foreign Equity Price,Aggressive Foreign Equity
# Income,International Diversified Equity Price,International Diversified Equity
# Income,Aggressive Equity Price,Aggressive Equity Income,Diversified Fixed
# Income Price,Diversified Fixed Income Income,Diversified Balanced Allocation
# Price,Diversified Balanced Allocation Income
