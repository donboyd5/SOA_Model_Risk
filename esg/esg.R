

# libraries ---------------------------------------------------------------

library(rlang)
library(tidyverse)
tprint <- 75  # default tibble print
options(tibble.print_max = tprint, tibble.print_min = tprint) # show up to tprint rows

library(fs)

# tools
library(vroom)
library(readxl)
library(openxlsx) # for writing xlsx files
library(lubridate)
library(RColorBrewer)
library(RcppRoll)
library(fredr)
library(tidycensus)

# boyd libraries
library(btools)
library(bdata)
library(bggtools)
library(bmaps)

# graphics
library(scales)
library(ggbeeswarm)
library(patchwork)
library(gridExtra)
library(ggrepel)
library(ggbreak)


# constants ---------------------------------------------------------------
n <- 10000
irseed <- 1234
mu = .07
sigma = .11


# mean reversion ----------------------------------------------------------



# fat tails ---------------------------------------------------------------

## normal ----
set.seed(irseed) # 12345 yields positive mean 
ir.norm <- rnorm(n, mean = mu, sd = sigma)


## t distribution fat tails ----
set.seed(irseed)
ir.t0 <- rt(n, df=5) # df is degrees of freedom parameter
(mean(ir.t0))
(sd(ir.t0))

# Transform the returns to have a mean of mu and a standard deviation of sigma
ir.t <- mu + sigma * ir.t0
(mean(ir.t))
(sd(ir.t))


## beta distribution fat tails ----
# In general, the larger the value of shape1 and shape2, the more peaked and
# narrow the distribution becomes, and the smaller the value of shape1 and
# shape2, the flatter and wider the distribution becomes. For example, if shape1
# and shape2 are both small, the distribution will be relatively flat and
# symmetrical, with a long tail on both sides. This means that the distribution
# will have a higher probability of generating extreme values, or fat tails.

# On the other hand, if shape1 and shape2 are both large, the distribution will
# be more peaked and narrow, with shorter tails on both sides. This means that
# the distribution will have a lower probability of generating extreme values.
# The location of the peak of the distribution is determined by the ratio of
# shape1 to shape2. If shape1 is larger than shape2, the peak of the
# distribution will be shifted towards the right, and if shape2 is larger than
# shape1, the peak of the distribution will be shifted towards the left. You can
# use the qbeta function from the stats package to compute the quantiles of the
# Beta distribution, and the curve function to plot the distribution, in order
# to visualize the effect of the shape parameters on the shape of the
# distribution.

set.seed(irseed)
# test out different shapes
f_beta <- function(shape1, shape2){
  x <- seq(0, 1, length.out = 100) # sequence of quantiles
  y <- qbeta(x, shape1 = shape1, shape2 = shape2) # beta values at each quantile
  # compute pdf and cdf of this beta
  pdf <- dbeta(x, shape1 = shape1, shape2 = shape2)
  cdf <- pbeta(x, shape1 = shape1, shape2 = shape2)
  # plot(x, pdf, type = "l", col = "blue", xlab = "Quantile", ylab = "Density")
  # lines(x, cdf, type = "l", col = "red")
  df <- tibble(qtile=x, pdf=pdf, cdf=cdf) |> 
    pivot_longer(cols=-qtile, names_to = "density")
  p <- df |> 
    ggplot(aes(qtile, value, colour=density)) +
    geom_line() +
    scale_color_manual(values=c(pdf="blue", cdf="red")) +
    ggtitle(paste0("Beta distribution with shape1= ",
                   shape1, "and shape2= ", shape2)) +
    theme_bw()
  p
}

f_beta(2, 5)
f_beta(4, 5)
f_beta(4, 2)
f_beta(2, 2)
f_beta(3, 3) + geom_density(y=pdf) 

ir.beta0 <- rbeta(n, shape1 = 2, shape2 = 5)
(mean(ir.beta0))
(sd(ir.beta0))

# Transform the returns to have a mean of mu and a standard deviation of sigma
ir.beta <- mu + sigma * ir.beta0
(mean(ir.beta))
(sd(ir.beta))



## cauchy distribution fat tails ----
set.seed(irseed)
ir.cauchy0 <- rcauchy(n, location = 0, scale = 0.5)
(mean(ir.cauchy0))
(sd(ir.cauchy0))

ir.cauchy <- mu + sigma * ir.cauchy0



# plot each distribution --------------------------------------------------
fplot <- function(ir, title=NULL){
  # histogram and density for a single investment return series
  tibble(x=ir) |> 
    ggplot(aes(x = x)) + 
    geom_histogram(aes(y = after_stat(density)),
                   colour = 1, fill = "cyan") +
    geom_density() +
    ggtitle(label=title) +
    theme_bw()
}

fplot(ir.norm)
fplot(ir.t)
fplot(ir.beta)
fplot(ir.cauchy)

pn / pt

## define distributions and labels ----
dists <- read_csv(
  "object, label
ir.norm, normal
ir.t, student
ir.beta, beta
ir.cauchy, cauchy
")
dists

# put investment returns into a data frame
df <- dists |> 
  rowwise() |> 
  mutate(ir=list(get(object))) |> 
  unnest(cols=c(ir))

# get summary stats by return breaks ----
df2 <- df |> 
  mutate(irgroup=cut(ir, breaks = c(-Inf, -1, seq(-.5, .5, .05), 1, Inf)),
         n_ir=n(),
         .by=label) |> 
  summarise(n_group=n(), .by=c(label, irgroup, n_ir)) |> 
  mutate(pct_group=n_group / n_ir) |> 
  select(label, irgroup, pct_group) |> 
  pivot_wider(names_from = label, values_from = pct_group, values_fill = 0) |> 
  arrange(irgroup) |> 
  mutate(across(-irgroup, cumsum))


# facet histogram plot with scale limits ----
df <- dists |> 
  rowwise() |> 
  mutate(ir=list(get(object))) |> 
  unnest(cols=c(ir))

# get mean of each distribution
dfmeans <- df |> 
  summarise(irmean=mean(ir), .by=label)

df |> 
  ggplot(aes(x = ir)) + 
  geom_histogram(aes(y = after_stat(density)),
                 colour = 1, fill = "cyan") +
  geom_density(colour="red") +
  geom_vline(aes(xintercept = irmean), data=dfmeans, colour="darkgreen", size=1.25) +
  scale_x_continuous(limits=c(-.5, .5), 
                     breaks=seq(-1, 1, .05), 
                     labels = label_percent(accuracy=1)) +
  ggtitle(label="Histogram and density") +
  # facet_wrap(~label, ncol=1, scales="free_y") +
  facet_wrap(~label, ncol=1, scales="fixed") +
  theme_bw()



# density plot in one panel, with scale limits ----

# create a data frame with list columns
df <- dists |> 
  rowwise() |>
  mutate(ir=list(get(object)),
         den=list(density(ir, n=1024)),
         ir.interp=list(den$x),
         ir.density=list(den$y))

# density plots
df |> 
  unnest(cols=c(ir.interp, ir.density)) |> 
  ggplot(aes(x=ir.interp, y=ir.density, colour=label, size=label)) +
  geom_line() +
  scale_size_manual(values=c(normal=2, student=1, beta=1, cauchy=1)) +
  scale_x_continuous(name="Interpolated investment return (truncated)",
                     limits=c(-.5, .5),
                     breaks=seq(-1, 1, 0.05),
                     labels=label_percent(accuracy=1)) +
  scale_y_continuous(name="density") +
  theme_bw()




# from chatgpt ----
# Load the stats package
library(stats)

# Set the number of returns to generate
n = 1000

# Set the mean and standard deviation of the distributions
mean = 0
sd = 1

# Generate the random returns from the normal and t distributions
returns_norm = rnorm(n, mean = mean, sd = sd)
returns_t = rt(n, df = 5, mean = mean, sd = sd)

# Calculate the kernel densities of the returns
density_norm = density(returns_norm)
density_t = density(returns_t)

# Plot the kernel densities
plot(density_norm, type = "l", col = "red", lwd = 2, xlab = "Returns", ylab = "Density")
lines(density_t, type = "l", col = "blue", lwd = 2)

# Add a legend
legend("topright", c("Normal", "Student's t"), lty = c(1, 1), col = c("red", "blue"))

# Load the required packages
library(ggplot2)
library(ggdensity)

# Set the number of returns to generate
n = 1000

# Set the mean and standard deviation of the distributions
mean = 0
sd = 1

# Generate the random returns from the normal and t distributions
returns_norm = rnorm(n, mean = mean, sd = sd)
returns_t = rt(n, df = 5, mean = mean, sd = sd)

# Create a data frame with the returns and a variable indicating the distribution
df = data.frame(returns = c(returns_norm, returns_t),
                distribution = c(rep("Normal", n), rep("Student's t", n)))

# Create the plot using ggplot
ggplot(df, aes(x = returns, color = distribution)) +
  geom_density(size = 1) +
  xlab("Returns") +
  ylab("Density")


mean <- -1
sd <- 10
a <- rlnorm(1e7, meanlog=mean, sdlog=sd)
quantile(a)
quantile(log(a))
mean(a)
mean(log(a))

tibble(ir=log(a)) |> 
  ggplot(aes(ir)) +
  geom_density(size = 1) +
  xlab("Returns") +
  ylab("Density")

quantile(rlnorm(1000, mean, sd))

mean <- .05
sd <- .01
a <- rlnorm(100, meanlog=mean, sdlog=sd)

# https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/comment-page-1/
# by definition, the logarithm of the log-normal distribution follows a normal
# distribution with the mean and standard deviation we just specified.
# Essentially, rlnorm(n = 1000000, meanlog = 7, sdlog = 75) and exp(rnorm(n =
# 1000000, mean = 7, sd = 75)) produce the same result.
n <- 1e5
mu <- 7
sigma <- 2

set.seed(1234); a <- rlnorm(n = n, meanlog = mu, sdlog = sigma)
set.seed(1234); b <- exp(rnorm(n=n, mean = mu, sd = sigma))
mean(a); mean(b)

# To get a sample of random data that follows a log-normal distribution and has
# arithmetic mean of 7 and a standard deviation of 75, you need to
# reparameterize things.  Roughly, you need to figure out what parameters should
# go into to the normal distribution such that when you exponentiate the draws,
# you end up with a mean of 7 and a standard deviation of 75.  Fortunately, that
# algebra has already been done and you can find the result on Wikipedia.
# https://en.wikipedia.org/wiki/Log-normal_distribution#Arithmetic_moments

m <- 7
s <- 75
location <- log(m^2 / sqrt(s^2 + m^2))
shape <- sqrt(log(1 + (s^2 / m^2)))
print(paste("location:", location))
print(paste("shape:", shape))
draws3 <- rlnorm(n=1000000, location, shape)
mean(draws3)


n <- 1e5
mu <- .07
sigma <- .11
(location <- log(mu^2 / sqrt(sigma^2 + mu^2)))
(shape <- sqrt(log(1 + (sigma^2 / mu^2))))

set.seed(1234); ir.lognormal <- rlnorm(n=n, meanlog=location, sdlog=shape)
set.seed(1234); ir.normal <- rnorm(n=n, mean=mu, sd=sigma)
mean(ir.normal)
sd(ir.normal)

mean(ir.lognormal); mean(exp(ir.normal))
mean(log(ir.lognormal))
sd(ir.lognormal)


