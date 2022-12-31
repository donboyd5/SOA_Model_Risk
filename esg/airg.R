

# libraries ---------------------------------------------------------------

source(here::here("r", "libraries.r"))


# locations ---------------------------------------------------------------

dsims <- r"(E:\data\soa\airg\1000sims)"
dsims <- r"(E:\data\soa\airg\10000sims)"


# get data ----------------------------------------------------------------

# AGGR.csv Aggressive or specialized equity
# BALANCED.csv Diversified balanced allocation
# FIXED.csv Diversified fixed income
# INT.csv Diversified international equity
# INTGOV.csv U.S. intermediate-term government bonds
# LTCORP.csv U.S. long-term corporate bonds
# SMALL.csv Intermediate risk equity
# US.csv Diversified large cap US equity

fn <- "AGGR.csv"
fn <- "BALANCED.csv"
fn <- "FIXED.csv"
fn <- "INT.csv"
fn <- "INTGOV.csv"
fn <- "LTCORP.csv"
fn <- "SMALL.csv"
fn <- "US.csv"

df <- read_csv(path(dsims, fn),
               col_names=paste0("y", 0:30))
glimpse(df)

dfl <- df |> 
  mutate(sim=row_number()) |> 
  pivot_longer(cols=-sim, names_to = "year", values_to = "asset") |> 
  mutate(year=str_remove(year, "y") |> as.integer())
glimpse(dfl)

cagr <- dfl |> 
  filter(year==30) |> 
  mutate(cagr=asset^(1/30) - 1)

cagr |> 
  ggplot(aes(x = cagr)) + 
  geom_histogram(aes(y = after_stat(density)),
                 colour = 1, fill = "cyan") +
  geom_density() +
  ggtitle(label=fn,
          subtitle="cagr") +
  geom_vline(xintercept = mean(cagr$cagr), size=1.25) +
  scale_x_continuous(breaks=seq(-1, 1, .02), labels=label_percent(accuracy=1)) +
  theme_bw()


# create df with all assets -----------------------------------------------
# we need vector of file paths
# dir_info(dsims, regexp="[.]csv$") |> pull(path)
# dir_walk(dsims, str, regexp="[.]csv$⁠")

(fnames <- dir_info(dsims, regexp="[.]csv$") |>
   filter(!path_file(path)=="UST.csv") |> # wrong number of coumns - it's a yield curve
  pull(path))

dfw <- vroom(fnames, col_names=paste0("y", 0:30), id="asset")
glimpse(dfw)
count(dfw, fname)

dfl <- dfw |> 
  mutate(sim=row_number(), .by=asset) |> 
  mutate(asset=path_file(asset) |> str_remove(".csv")) |> 
  pivot_longer(cols=-c(sim, asset), names_to = "year") |> 
  mutate(year=str_remove(year, "y") |> as.integer())
glimpse(dfl)  

dfl |> 
  filter(!str_starts(asset, "UST")) |> 
  summarise(value=sd(value) / median(value), .by=c(asset, year)) |> 
  ggplot(aes(year, value, colour=asset)) +
  geom_line()

saveRDS(dfl, path(dsims, "assets.rds"))

# make a portfolio, compare to normal -------------------------------------
dfl <- readRDS(path(dsims, "assets.rds"))
count(dfl, asset)
glimpse(dfl)

dfl |> 
  arrange(asset, sim, year) |> 
  mutate(return=value / lag(value) - 1, .by=c(asset, sim)) |> 
  filter(year > 0) |> 
  summarise(mean=mean(return), 
            median=median(return), 
            sd=sd(return),
            .by=asset)

aigr1 <- dfl |> 
  filter(asset %in% c("AGGR", "LTCORP")) |> 
  group_by(sim, asset) |> 
  arrange(year) |> 
  mutate(return=value / lag(value) - 1) |> 
  ungroup() |> 
  arrange(sim, asset, year) |> 
  select(-value) |> 
  pivot_wider(names_from = asset, values_from = return) |> 
  mutate(portfolio=.65 * AGGR + .35 * LTCORP) |> 
  pivot_longer(-c(sim, year), names_to = "asset", values_to = "return") |> 
  arrange(sim, asset, year) |> 
  group_by(sim, asset) |> 
  mutate(factor=ifelse(year==0, 1, 1 + return),
         value=cumprod(factor),
         cagr=value^(1/year) - 1) |> 
  ungroup()

count(aigr1, year)

musigma <- aigr1 |> 
  filter(year > 0) |> 
  summarise(mean=mean(return), sd=sd(return), .by=asset)
musigma


aigr1 |> filter(asset=="AGGR", sim==1, year==30)
dfl |> filter(asset=="AGGR", sim==1, year==30)

aigr1 |> filter(sim==1, year==30)

# what are median return and sd of the portfolio, by year
aigr2 <- aigr1 |> 
  filter(asset=="portfolio") |> 
  summarise(sd=sd(return), avgret=median(return), .by=c(year))

aigr2 |> 
  filter(year!=0) |> 
  pivot_longer(-year) |> 
  ggplot(aes(year, value)) +
  geom_line(colour="blue", size=1.25) +
  scale_y_continuous(breaks=seq(0, 30, .02), labels=label_percent(accuracy=.1), limits = c(0, NA)) +
  facet_wrap(~name, ncol=2, scales="fixed") +
  theme_bw()

# compare to normal distribution with 8% return, 18% SD, time-independent
# what are final cagrs?

# prep portfolio ---------------------------------------------------------------
dfl <- readRDS(path(dsims, "assets.rds"))
count(dfl, asset)

prep1 <- dfl |> 
  filter(asset %in% c("AGGR", "LTCORP")) |> 
  group_by(sim, asset) |> 
  arrange(year) |> 
  mutate(return=value / lag(value) - 1) |> 
  ungroup() |> 
  arrange(sim, asset, year) |> 
  select(-value) |> 
  pivot_wider(names_from = asset, values_from = return) |> 
  mutate(portfolio=.65 * AGGR + .35 * LTCORP)


# |> 
#   pivot_longer(-c(sim, year), names_to = "asset", values_to = "return") |> 
#   arrange(sim, asset, year) |> 
#   group_by(sim, asset) |> 
#   mutate(factor=ifelse(year==0, 1, 1 + return),
#          value=cumprod(factor),
#          cagr=value^(1/year) - 1) |> 
#   ungroup()

count(prep1, year)

# constants ---------------------------------------------------------------
musigma <- prep1 |> 
  filter(year > 0) |> 
  summarise(mean=mean(portfolio), sd=sd(portfolio))
musigma

n <- 10000
years <- 30
irseed <- 1234
# mu <- .08
# sigma <- .18
(mu <- musigma |> pull(mean))
(sigma <- musigma |> pull(sd))


set.seed(irseed) # 12345 yields positive mean 
ir.norm <- rnorm(n * years, mean = mu, sd = sigma)

dfnorm1 <- tibble(year=rep(0:years, times=n),
                 sim=rep(1:n, times=rep(years + 1, n)),
                 normal=ifelse(year==0, NA_real_, ir.norm))

prep2 <- prep1 |> 
  left_join(dfnorm1, by = join_by(sim, year)) |> 
  pivot_longer(-c(sim, year), names_to = "asset", values_to = "return") |> 
  arrange(asset, sim, year) |> 
  group_by(asset, sim) |> 
  mutate(annual_growfactor=ifelse(year==0, 1, 1 + return),
         assetval=cumprod(annual_growfactor),
         cagr=assetval^(1/year) - 1) |> 
  ungroup()
  
ht(prep2)


prep2 |> 
  filter(asset %in% c("portfolio", "normal"), year > 0) |> 
  # group_by(year, asset) |> 
  summarise(cagr=median(cagr), assetval=median(assetval), .by=c(year, asset)) |> 
  ggplot(aes(year, assetval, colour=asset)) +
  geom_line() +
  scale_y_continuous(breaks=seq(0, 10, .5), labels=label_number(accuracy=.1)) +
  theme_bw()

prep2 |> 
  # filter(asset %in% c("portfolio", "normal"), year > 0) |> 
  # group_by(year, asset) |> 
  summarise(cagr=median(cagr), assetval=p75(assetval), .by=c(year, asset)) |> 
  select(year, asset, assetval) |> 
  pivot_wider(names_from = asset, values_from = assetval) |> 
  mutate(pdiff=portfolio / normal - 1) |> 
  ggplot(aes(year, pdiff)) +
  geom_line(colour="blue") +
  geom_hline(yintercept = 0) +
  #scale_y_continuous(breaks=seq(-1, 1, .005), labels=label_percent(accuracy=.1)) +
  theme_bw()





aigr2 <- aigr1 |> 
  filter(asset=="portfolio") |> 
  summarise(sd=sd(return), avgret=median(return), .by=c(year))

comp <- aigr1 |> 
  filter(asset=="portfolio")

dfl |> 
  filter(asset %in% c("AGGR", "LTCORP")) |> 
  group_by(sim, asset) |> 
  arrange(year) |> 
  mutate(return=value / lag(value) - 1) |> 
  ungroup() |> 
  arrange(sim, asset, year) |> 
  select(-value) |> 
  pivot_wider(names_from = asset, values_from = return) |> 
  mutate(portfolio=.65 * AGGR + .35 * LTCORP) |> 
  pivot_longer(-c(sim, year), names_to = "asset", values_to = "return") |> 
  arrange(sim, asset, year) |> 
  group_by(sim, asset) |> 
  mutate(factor=ifelse(year==0, 1, 1 + return),
         value=cumprod(factor),
         cagr=value^(1/year) - 1) |> 
  ungroup()




  









