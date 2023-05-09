
# https://github.com/braverock/PortfolioAnalytics
# remotes::install_github("braverock/PortfolioAnalytics")  # ????
# remotes::install_github("braverock/PortfolioAnalytics")

# https://rdrr.io/github/R-Finance/PortfolioAnalytics/#vignettes

# https://github.com/R-Finance/PortfolioAnalytics/blob/master/demo/demo_efficient_frontier.R

# https://www.tidy-pm.com/s-4portfolios.html

# install.packages("remotes")
# remotes::install_github("R-Finance/PortfolioAnalytics")


library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(DEoptim)

# Load the data
data(indexes)

# Subset the data
index_returns <- indexes[, c(1:4)]

# Print the head of the data
head(index_returns)

# Create the portfolio specification
port_spec <- portfolio.spec(colnames(index_returns))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio =port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

# Solve the optimization problem
opt <- optimize.portfolio(index_returns, portfolio = port_spec, optimize_method = "ROI")


print(opt)
chart.Weights(opt)




# Create the portfolio specification
ret2 <- returns |> 
  mutate(date=strptime(year, "%Y")) |>
  select(-year, -cpiu)
xret <- as.xts(ret2 |> select(-date), ret2$date)  
ps2 <- portfolio.spec(colnames(xret))
ps2 <- add.constraint(portfolio =ps2, type = "full_investment")
ps2 <- add.constraint(portfolio = ps2, type = "long_only")
ps2 <- add.objective(portfolio = ps2, type = "risk", name = "StdDev")
# ps2 <- add.objective(portfolio = ps2, type = "return", name = "mean")
ps2

opt <- optimize.portfolio(xret, portfolio = ps2, optimize_method = "ROI")
opt

opt2 <- optimize.portfolio(xret, portfolio = ps2, optimize_method = "ROI", trace=TRUE)
opt2
# chart.EfficientFrontier(opt2)
opt2$weights

# opt2 <- optimize.portfolio(xret, portfolio = ps2, optimize_method = "ROI", targetReturn = .07)
# opt2

tmp <- extractEfficientFrontier(
  opt2,
  match.col = "ES",
  n.portfolios = 25,
  risk_aversion = NULL)

tmp <- extractEfficientFrontier(
  opt2,
  match.col = "StdDev",
  n.portfolios = 1000,
  risk_aversion = NULL)

str(tmp)
tmp$frontier
str(tmp$frontier)
class(tmp$frontier)

tmp$frontier[, 2]
nrow(tmp$frontier)
frontobj <- tmp

frontier_tbl <- function(frontobj){
  cnames <- colnames(frontobj$frontier)
  matrix(frontobj$frontier, nrow=nrow(frontobj$frontier)) |> 
    as_tibble() |> 
    setNames(cnames) |> 
    rename(sd=StdDev) |> 
    mutate(obs=row_number()) |> 
    relocate(obs)
}


df <- frontier_tbl(tmp)

df |>
  filter(obs==iclosest(mean, .112))

df |> 
  mutate(idx=which.min(abs(mean - 0.07))[1]) |> 
  filter(obs==idx)

df |> 
  ggplot(aes(sd, mean)) +
  geom_line()


cnames <- colnames(tmp$frontier)
matrix(tmp$frontier, nrow=9) |> 
  as_tibble() |> 
  setNames(cnames) |> 
  mutate(obs=row_number()) |> 
  relocate(obs)



t1 <- tibble(tmp$frontier)
colnames(t1) <- cnames
t1


colnames(tmp$frontier)


tmp$frontier["result.22", ]

as.matrix(tmp$frontier)
tmp$frontier |> 
  as_tibble()

meanvar.ef <- create.EfficientFrontier(R=xret, portfolio=ps2, type="mean-StdDev", n.portfolios = 100)
meanvar.ef
str(meanvar.ef)
summary(meanvar.ef, digits=3)
chart.EfficientFrontier(meanvar.ef, match.col="StdDev", type="l", RAR.text="Sharpe Ratio", pch=4)


# mean-var efficient frontier
meanvar.ef <- create.EfficientFrontier(R=xret, portfolio=ps2, type="mean-StdDev")
meanvar.ef
summary(meanvar.ef, digits=3)
meanvar.ef$frontier

chart.EfficientFrontier(meanvar.ef, match.col="StdDev", type="l", RAR.text="Sharpe Ratio", pch=4)
chart.EfficientFrontier(meanvar.ef, match.col="StdDev", type="b", rf=NULL)
chart.EF.Weights(meanvar.ef, colorset=bluemono, match.col="StdDev")


opt_meanvar <- optimize.portfolio(R=xret, portfolio=ps2, optimize_method="ROI", trace=TRUE)

# The efficient frontier is created from the 'opt_meanvar' object by getting
# The portfolio and returns objects and then passing those to create.EfficientFrontier
chart.EfficientFrontier(opt_meanvar, match.col="StdDev", n.portfolios=25, type="l")






# create the correlation matrix, means, and standard deviations
correlation_matrix <- matrix(c(1, 0.5, 0.3, 
                               0.5, 1, 0.2, 0.3,
                               0.2, 1), nrow = 3)
colnames(correlation_matrix) <- c("stocks", "bonds", "bills")
means <- c(0.05, 0.08, 0.10)
sd <- c(0.10, 0.15, 0.20)

# set the target return
target_return <- 0.08

# create the portfolio specification object
port_spec <- portfolio.spec(assets = colnames(correlation_matrix))

# add the portfolio constraints
port_spec <- add.constraint(port_spec, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
port_spec <- add.constraint(port_spec, type = "long_only")
port_spec <- add.constraint(port_spec, type = "box", min = 0, max = 0.5)

# add the objective function
port_spec <- add.objective(port_spec, type = "risk", name = "StdDev")

# create the portfolio optimization object
port_opt <- optimize.portfolio(R = means,
                               covmat = correlation_matrix * outer(sd, sd), 
                               portfolio = port_spec, 
                               targetReturn = target_return)

# print the optimal portfolio weights
port_opt$weights




# fat tailedness ----------------------------------------------------------

library(fBasics)

# Generate samples from a t-distribution with 3 degrees of freedom
set.seed(123)
x_t <- rt(1000, df = 3)

# Calculate the quantile ratio for the 95th percentile
# qr_t <- qtR(x_t, p = 0.95)
med <- median(x_t) # Calculate the median probability
df <- 3
tail_prob <- 1 - pt(qt(0.95, df = df), df = df) # Calculate the tail probability at the 95th percentile
tail_prob

# Calculate the quantile ratio
qr_t <- tail_prob/med
qr_t

# Plot density curves of the t-distribution and normal distribution
x_seq <- seq(-4, 4, length.out = 100)
dens_t <- dt(x_seq, df = 3)
dens_norm <- dnorm(x_seq)

plot(x_seq, dens_t, type = "l", col = "red", lwd = 2,
     main = "Comparison of t-distribution and normal distribution",
     xlab = "x", ylab = "Density")
lines(x_seq, dens_norm, col = "blue", lwd = 2)

legend("topright", c("t-distribution", "Normal distribution"), 
       col = c("red", "blue"), lwd = 2)

p <- ggplot(df, aes(sample = y))
p + stat_qq() + stat_qq_line()

xt <- rt(1000, df = 3)
summary(xt)
tibble(obs=1:length(xt), xt) |> 
  ggplot(aes(sample=xt))  + 
  stat_qq_line() + 
  stat_qq(colour="blue", size=0.75) +
  theme_bw()




