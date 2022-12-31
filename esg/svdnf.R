
# libraries ---------------------------------------------------------------
source(here::here("r", "libraries.r"))
library(SVDNF)

# locations ---------------------------------------------------------------

# constants ---------------------------------------------------------------

# examples ---------------------------------------------------------------


# discrete nonlinear filter (DNF) of Kitagawa (1987),  ---------------------------------------------------------
# DNF.dynamicsSVM

set.seed(1)
# Generate 200 returns from the DuffiePanSingleton model
DuffiePanSingleton_mod <- dynamicsSVM(model = "DuffiePanSingleton") 
summary(DuffiePanSingleton_mod)

## note that model sim creates the simulated returns djb ----
DuffiePanSingleton_sim <- modelSim(t = 200, dynamics = DuffiePanSingleton_mod) 
summary(DuffiePanSingleton_sim)

djb0 <- modelSim(t = 10000, dynamics = DuffiePanSingleton_mod) # fast enough for 10k obs

# what if we wanted 50 years and many sims?
f <-  function(sim) {
  t <- 50
  lsim <- modelSim(t = t, dynamics = DuffiePanSingleton_mod)
  df <- tibble(sim=rep(sim, t),
               time=1:t, 
               return=lsim$returns, 
               volatility=lsim$volatility_factor)
  df
}
f(1)  
df <- map_dfr(1:10000, f)
summary(df)
df |> 
  filter(sim %in% c(1, 7)) |> 
  ggplot(aes(time, returns, colour=as.factor(sim))) +
  geom_line() +
  geom_hline(yintercept = 0)



djb1 <- tibble(time=1:length(djb0$volatility_factor),
               return=djb0$returns,
               vol=djb0$volatility_factor)



djb1 <- tibble(time=1:length(DuffiePanSingleton_sim$volatility_factor),
               return=DuffiePanSingleton_sim$returns,
               vol=DuffiePanSingleton_sim$volatility_factor)
summary(djb1)

djb1 |> 
  ggplot(aes(time, return)) +
  geom_line(colour="blue") +
  geom_hline(yintercept = 0)

djb1 |> 
  ggplot(aes(time, vol)) +
  geom_line(colour="red") +
  geom_hline(yintercept = 0)

# Run DNF on the data
dnf_filter <- DNF(data = DuffiePanSingleton_sim$returns,
                  dynamics = DuffiePanSingleton_mod) 

# Print log-likelihood evaluation.
dnf_filter$log_likelihood

# Using a custom model.
# Here, we define the DuffiePanSingleton model as a custom model
# to get the same log-likelihood found using the built-in option

# Daily observations
h <- 1/252

# Parameter values 
mu <- 0.038; kappa <- 3.689; theta <- 0.032
sigma <- 0.446; rho <- -0.745; omega <- 5.125
delta <- 0.03; alpha <- -0.014; rho_z <- -1.809; nu <- 0.004

# Jump compensator
alpha_bar <- exp(alpha + 0.5 * delta^2) / (1 - rho_z * nu) - 1

# Returns drift and diffusion
mu_y <- function(x, mu, alpha_bar, omega, h) {
  return(h * (mu - x / 2 - alpha_bar * omega))
}
mu_y_params <- list(mu, alpha_bar, omega, h)

sigma_y <- function(x, h) {
  return(sqrt(h * pmax(x, 0)))
}
sigma_y_params <- list(h)

# Volatility factor drift and diffusion
mu_x <- function(x, kappa, theta, h) {
  return(x + h * kappa * (theta - pmax(0, x)))
}
mu_x_params <- list(kappa, theta, h)

sigma_x <- function(x, sigma, h) {
  return(sigma * sqrt(h * pmax(x, 0)))
}
sigma_x_params <- list(sigma, h)

# Jump distribution for the DuffiePanSingleton Model
jump_density <- dpois
jump_dist <- rpois
jump_params <- c(h * omega)

# Create the custom model
custom_mod <- dynamicsSVM(model = 'Custom',
                          mu_x = mu_x, mu_y = mu_y, sigma_x = sigma_x, sigma_y = sigma_y,
                          mu_x_params = mu_x_params, mu_y_params = mu_y_params,
                          sigma_x_params = sigma_x_params, sigma_y_params = sigma_y_params,
                          jump_params = jump_params, jump_dist = jump_dist, jump_density = jump_density,
                          nu = nu, rho_z = rho_z, rho = rho)
# Define the grid for DNF
N <- 50; R <- 1; K <- 20
var_mid_points <- seq(from = sqrt(0.0000001),
                      to = sqrt(theta + (3 + log(N)) * sqrt(0.5 * theta * sigma^2 / kappa)), length = N)^2
plot(var_mid_points)

j_nums <- seq(from = 0, to = R, by = 1)

jump_mid_points <- seq(from = 0.000001, to = (3 + log(K)) * sqrt(R) * nu, length = K)

grids <- list(var_mid_points = var_mid_points,
              j_nums = j_nums, jump_mid_points = jump_mid_points)

# Run the DNF function with the custom model
dnf_custom <- DNF(data = DuffiePanSingleton_sim$returns, grids = grids, 
                  dynamics = custom_mod)

# Check if we get the same log-likelihoods
dnf_custom$log_likelihood; dnf_filter$log_likelihood 

# dnfOptim ----
set.seed(1)

# Generating return data
Taylor_mod <- dynamicsSVM(model = "Taylor", phi = 0.9,
                          theta = -7.36, sigma = 0.363)
Taylor_sim <- modelSim(t = 30, dynamics = Taylor_mod, init_vol = -7.36)
plot(Taylor_sim$volatility_factor, type = 'l')
plot(Taylor_sim$returns, type = 'l')

# Initial values and optimization bounds
init_par <- c( 0.7, -5, 0.3)
lower <- c(0.01, -20, 0.01); upper <- c(0.99, 0, 1)

# Running DNFOptim to get MLEs
optim_test <- DNFOptim(data = Taylor_sim$returns,
                       dynamics = Taylor_mod,
                       par = init_par, lower = lower, upper = upper, method = "L-BFGS-B")

# Parameter estimates
optim_test$par


# dynamicsSVM ---------------------------------------------------------------------
# Create a dynamicsSVM object with model DuffiePanSingleton and default parameters
DuffiePanSingleton_mod <- dynamicsSVM(model = "DuffiePanSingleton") 

# Here, we define the same DuffiePanSingleton model 
# using the custom model option.

# Daily observations
h <- 1/252

# Parameter values 
mu <- 0.038; kappa <- 3.689; theta <- 0.032
sigma <- 0.446; rho <- -0.745; omega <- 5.125
delta <- 0.03; alpha <- -0.014; rho_z <- -1.809; nu <- 0.004

# Jump compensator
alpha_bar <- exp(alpha + 0.5 * delta^2) / (1 - rho_z * nu) - 1

# Returns drift and diffusion
mu_y <- function(x, mu, alpha_bar, omega, h) {
  return(h * (mu - x / 2 - alpha_bar * omega))
}
mu_y_params <- list(mu, alpha_bar, omega, h)
sigma_y <- function(x, h) {
  return(sqrt(h * pmax(x, 0)))
}
sigma_y_params <- list(h)

# Volatility factor drift and diffusion
mu_x <- function(x, kappa, theta, h) {
  return(x + h * kappa * (theta - pmax(0, x)))
}
mu_x_params <- list(kappa, theta, h)

sigma_x <- function(x, sigma, h) {
  return(sigma * sqrt(h * pmax(x, 0)))
}
sigma_x_params <- list(sigma, h)

# Jump distribution for the DuffiePanSingleton Model
jump_density <- dpois
jump_dist <- rpois
jump_params <- c(h * omega)

# Create the custom model
custom_DPS <- dynamicsSVM(model = 'Custom',
                          mu_x = mu_x, mu_y = mu_y, sigma_x = sigma_x, sigma_y = sigma_y,
                          mu_x_params = mu_x_params, mu_y_params = mu_y_params,
                          sigma_x_params = sigma_x_params, sigma_y_params = sigma_y_params,
                          jump_params = jump_params, jump_dist = jump_dist, jump_density = jump_density,
                          nu = nu, rho_z = rho_z)



# modelSim ----------------------------------------------------------------
set.seed(1)
# Generate 250 returns from the DuffiePanSingleton model
DuffiePanSingleton_mod <- dynamicsSVM(model = "DuffiePanSingleton") 
DuffiePanSingleton_sim <- modelSim(t = 200, dynamics = DuffiePanSingleton_mod) 

# Plot the volatility factor and returns that were generated
plot(DuffiePanSingleton_sim$volatility_factor, type = 'l',
     main = 'DuffiePanSingleton Model Simulated Volatility Factor', ylab = 'Volatility Factor')

plot(DuffiePanSingleton_sim$returns, type = 'l',
     main = 'DuffiePanSingleton Model Simulated Returns', ylab = 'Returns')

# Generate 250 steps from a custom model
# Set parameters
kappa <- 100; theta <- 0.05; sigma <- 2.3; h <- 1/252 ; mu <- 0.04
rho <- -0.8; omega <- 5; alpha <- -0.025; nu <- 0.01; rho_z <- -1; delta <- 0.025
# Jump compensator
alpha_bar <- exp(alpha + 0.5 * delta^2) / (1 - rho_z * nu) - 1

# Define returns drift and diffusion functions
# Returns drift and diffusion
mu_y <- function(x, mu, alpha_bar, omega, h){
  return(h * (mu - x / 2 - alpha_bar * omega))
}
mu_y_params <- list(mu, alpha_bar, omega , h)
sigma_y <- function(x, h, sigma){
  return(sigma * sqrt(h) * pmax(x,0))
}
sigma_y_params <- list(h, sigma)

# Volatility factor drift and diffusion functions
mu_x <- function(x, h, kappa, theta){
  return(x + h * kappa * pmax(0,x) * (theta - pmax(0,x)))
}
mu_x_params <- list(h, kappa, theta)

sigma_x <- function(x, sigma, h){
  return(sigma * sqrt(h) * pmax(0,x))
}
sigma_x_params <- list(sigma, h)

# Include simultaneous return and volatility factor jumps
# based on the Poisson distribution for jump times
jump_dist <- rpois
jump_params <- list(omega * h)
custom_mod <- dynamicsSVM(model = "Custom", mu_x = mu_x, mu_y = mu_y,
                          sigma_x = sigma_x, sigma_y = sigma_y,
                          mu_x_params = mu_x_params, mu_y_params = mu_y_params,
                          sigma_x_params = sigma_x_params, sigma_y_params = sigma_y_params,
                          jump_dist = jump_dist, jump_params = jump_params, 
                          nu = nu, rho_z = rho_z, omega = omega, alpha = alpha, delta = delta)
custom <- modelSim(t = 250, dynamics = custom_mod)

plot(custom$volatility_factor, type = 'l',
     main = 'Custom Model Simulated Volatility Factor', ylab = 'Volatility Factor')
plot(custom$returns, type = 'l',
     main = 'Custom Model Simulated Returns', ylab = 'Returns')


# modelSim.dynamicsSVM --------------------------------------------------------------------

set.seed(1)
# Generate 250 returns from the DuffiePanSingleton model
DuffiePanSingleton_mod <- dynamicsSVM(model = "DuffiePanSingleton") 
DuffiePanSingleton_sim <- modelSim(t = 200, dynamics = DuffiePanSingleton_mod) 

# Plot the volatility factor and returns that were generated
plot(DuffiePanSingleton_sim$volatility_factor, type = 'l',
     main = 'DuffiePanSingleton Model Simulated Volatility Factor', ylab = 'Volatility Factor')

plot(DuffiePanSingleton_sim$returns, type = 'l',
     main = 'DuffiePanSingleton Model Simulated Returns', ylab = 'Returns')

# Generate 250 steps from a custom model
# Set parameters
kappa <- 100; theta <- 0.05; sigma <- 2.3; h <- 1/252 ; mu <- 0.04
rho <- -0.8; omega <- 5; alpha <- -0.025; nu <- 0.01; rho_z <- -1; delta <- 0.025
# Jump compensator
alpha_bar <- exp(alpha + 0.5 * delta^2) / (1 - rho_z * nu) - 1

# Define returns drift and diffusion functions
# Returns drift and diffusion
mu_y <- function(x, mu, alpha_bar, omega, h){
  return(h * (mu - x / 2 - alpha_bar * omega))
}
mu_y_params <- list(mu, alpha_bar, omega , h)
sigma_y <- function(x, h, sigma){
  return(sigma * sqrt(h) * pmax(x,0))
}
sigma_y_params <- list(h, sigma)

# Volatility factor drift and diffusion functions
mu_x <- function(x, h, kappa, theta){
  return(x + h * kappa * pmax(0,x) * (theta - pmax(0,x)))
}
mu_x_params <- list(h, kappa, theta)

sigma_x <- function(x, sigma, h){
  return(sigma * sqrt(h) * pmax(0,x))
}
sigma_x_params <- list(sigma, h)

# Include simultaneous return and volatility factor jumps
# based on the Poisson distribution for jump times
jump_dist <- rpois
jump_params <- list(omega * h)
custom_mod <- dynamicsSVM(model = "Custom", mu_x = mu_x, mu_y = mu_y,
                          sigma_x = sigma_x, sigma_y = sigma_y,
                          mu_x_params = mu_x_params, mu_y_params = mu_y_params,
                          sigma_x_params = sigma_x_params, sigma_y_params = sigma_y_params,
                          jump_dist = jump_dist, jump_params = jump_params, 
                          nu = nu, rho_z = rho_z, omega = omega, alpha = alpha, delta = delta)
custom <- modelSim(t = 250, dynamics = custom_mod)
custom <- modelSim(t = 10000, dynamics = custom_mod)

plot(custom$volatility_factor, type = 'l',
     main = 'Custom Model Simulated Volatility Factor', ylab = 'Volatility Factor')
plot(custom$returns, type = 'l',
     main = 'Custom Model Simulated Returns', ylab = 'Returns')

custom$returns
mean(custom$returns)
mean((1 + custom$returns)^252 - 1)


# 50 years, 1000 paths, Heston model ----
set.seed(1)
# Generate 200 returns from the DuffiePanSingleton model
heston_mod <- dynamicsSVM(model = "Heston")  # default
heston_sim <- modelSim(t = 50, dynamics = heston_mod) 

# what if we wanted 50 years and many sims?
f <-  function(sim, mod) {
  t <- 50
  lsim <- modelSim(t = t, dynamics = mod)
  df <- tibble(sim=rep(sim, t),
               time=1:t, 
               return=lsim$returns, 
               volatility=lsim$volatility_factor)
  df
}
f(1)  
df <- map_dfr(1:1000, f)
summary(df)
df |> 
  filter(sim %in% c(1, 7, 900)) |> 
  ggplot(aes(time, return, colour=as.factor(sim))) +
  geom_line() +
  geom_hline(yintercept = 0)


f <-  function(sim, mod) {
  t <- 50
  lsim <- modelSim(t = t, dynamics = mod)
  df <- tibble(sim=rep(sim, t),
               time=1:t, 
               return=lsim$returns, 
               volatility=lsim$volatility_factor)
  df
}
f(1)  


bates_mod <- dynamicsSVM(model = "Bates", mu=.07, h=1) 
heston_mod <- dynamicsSVM(model = "Heston", mu=.07, h=1) 

df <- map_dfr(1:1000, f, heston_mod)
df |> 
  summarise(return_mean=mean(return), return_sd=sd(return), .by=time)

df |> 
  filter(sim %in% c(1, 7, 900)) |> 
  ggplot(aes(time, return, colour=as.factor(sim))) +
  geom_line() +
  geom_hline(yintercept = 0)
