


# clayton copula ----

source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))

library(copula)

returns <- readRDS(here::here("data", "damodaran_data.rds")) # |> dplyr::filter(year %in% 1950:2022)

# estimate theta ----------------------------------------------------------
ret1 <- returns |> pull(sp500)
ret2 <- returns |> pull(baacorp)


ecdf1 <- ecdf(ret1)
ecdf2 <- ecdf(ret2)

u1 <- ecdf1(ret1)
u2 <- ecdf2(ret2)

U <- cbind(u1, u2)

claytonFit <- fitCopula(claytonCopula(), U, method = "ml")

(theta <- coef(claytonFit))





# specify the copula (in this case, a Clayton copula) ----

# The Clayton copula is a specific type of copula that is particularly good at
# modeling situations where the variables have a high degree of dependency in
# their lower tail. In other words, it can effectively capture the situation
# where low values of one variable are associated with low values of another
# variable.

# A common application of the Clayton copula in finance is in modeling asset
# returns. In this context, a Clayton copula can capture the observation that
# assets often decrease together in value during periods of market stress (i.e.,
# they have "lower tail dependence").

clayton_cop <- claytonCopula(dim = 2, param = 2) # cor .68
clayton_cop <- claytonCopula(dim = 2, param = 1) # cor .48
clayton_cop <- claytonCopula(dim = 2, param = 0.1) # cor .07

clayton_cop <- claytonCopula(dim = 2, param = theta) # cor .36

# generate uniform marginals using the copula
uniform_data <- rCopula(50000, clayton_cop)
cor(uniform_data)
plot(uniform_data)
hist(uniform_data[, 1])
hist(uniform_data[, 2])


# transform the uniform marginals to Gaussian marginals
gaussian_data <- qnorm(uniform_data)
cor(gaussian_data)
hist(gaussian_data[, 1])
hist(gaussian_data[, 2])

# plot the data
plot(gaussian_data)

# now make one variable follow  a t distribution
# transform the first variable to Gaussian and the second to a t-distribution with 3 degrees of freedom (heavier tails)
gaussian_t_data <- cbind(qnorm(uniform_data[, 1], mean=.07, sd=.10), qt(uniform_data[, 2], df = 14) |> scale_dist(.07, .10))
# gaussian_t_data <- cbind(qnorm(uniform_data[, 1], mean=.07, sd=.10), qnorm(uniform_data[, 2], mean=.07, sd=.10))
# gaussian_t_data <- cbind(qnorm(uniform_data[, 1], mean=.07, sd=.10), qnorm(uniform_data[, 2], mean=.07, sd=.10))
# gaussian_t_data <- cbind(qnorm(uniform_data[, 1]), qt(uniform_data[, 2], df = 14))
cor(gaussian_t_data)
hist(gaussian_t_data[, 1])
hist(gaussian_t_data[, 2])
plot(gaussian_t_data)


df <- crossing(sim=1:1000, year=1:50) |> 
  cbind(as_tibble(gaussian_t_data,
                  .name_repair = "unique")) |> 
  dplyr::rename(nvar=3, tvar=4) |> 
  mutate(port=.5 * nvar + .5 * tvar)
skim(df)

p <- df |> 
  ggplot(aes(sample=port)) +
  stat_qq_line() +
  stat_qq(colour="blue", size=0.75) +
  scale_x_continuous(labels=label_number(accuracy=0.1), limits=c(-4, 4), breaks=seq(-4, 4, .5)) +
  scale_y_continuous(labels=label_percent(accuracy=0.1), breaks=seq(-2, 2, .1))
p


dfl <- tibble(sim=1:1000, year=0, nvar=0, tvar=0, port=0) |> 
  bind_rows(df) |> 
  pivot_longer(c(nvar, tvar, port)) |> 
  arrange(year) |> 
  mutate(aa=cumprod(1 + value), .by=c(name, sim)) |> 
  dplyr::select(name, sim, year, ir=value, aa) |> 
  arrange(name, sim, year)

ptiles <- c(0, .001, .01, .05, .1, .25, .5, .75, .9, .95, .99, .999)
dfl |> 
  dplyr::filter(year %in% seq(0, 50, 5)) |> 
  reframe(q=list(quantile(aa, probs=ptiles)), .by=c(year, name)) |> 
  unnest_wider(col=q) |> 
  arrange(year, name)


# clayton_cop <- claytonCopula(dim = 2, param = .36)
# uniform_data <- rCopula(1000, clayton_cop)
