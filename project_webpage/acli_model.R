

# Iouri Karpov

#  log returns (log(p[t]) / log(p[t-1])) of financial assets are often assumed to be normally distributed,
#  particularly in the context of models like Black-Scholes or geometric
#  Brownian motion. This assumption makes financial modeling more tractable.

# returns <- readRDS(here::here("data", "damodaran_data.rds"))
# note that Damodaran returns are simple returns (I checked the calcs)

getwd()


# includes ----------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))


# constants ---------------------------------------------------------------
dir <- r"(E:\data\soa\acli\test)"


# get scenario ------------------------------------------------------------

# system.time(df <- read_csv(path(dir, "US.csv")))
# system.time(df <- vroom(path(dir, "US.csv")))

# 

fname <- "US.csv"

df <- vroom(path(dir, fname), col_names=FALSE)
glimpse(df)

df2 <- df |> 
  mutate(sim=row_number(),
         class=str_remove(fname, ".csv")) |> 
  pivot_longer(-c(sim, class), names_to = "year", values_to = "asset") |> 
  mutate(year=str_remove(year, "X") |> as.integer()) |> 
  arrange(sim, year) |> 
  mutate(irsimp=asset / lag(asset) - 1,
         logret=log(asset / lag(asset)),
         irlogret=exp(logret)-1, # exact same as irsimp
         .by=c(class, sim))

glimpse(df2)
cor(df2 |> select(irsimp, logret, irlogret), use="complete.obs")

skim(df2)

tmp <- df2 |> filter(ir < -1)
tmp
tmp <- df2 |> filter(sim==921, year %in% 17:23)
tmp

(aratio <- 0.966 / 3.45)
(apch <- aratio - 1)
(logret <- log(aratio))
(logret2 <- exp(logret) - 1)

(apch==logret2)

ir <- rnorm(1000, mean = .07, sd = .10)

hist(ir)



