// https://mc-stan.org/docs/2_22/stan-users-guide/stochastic-volatility-models.html

data {
  int<lower=0> N_t;   // # time points (equally spaced)
  vector[N_t] y;      // mean corrected return at time t
}
parameters {
  real mu;                     // mean log volatility
  real<lower=-1,upper=1> phi;  // persistence of volatility
  real<lower=0> sigma;         // white noise shock scale
  vector[N_t] h;                 // log volatility at time t
}
model {
  phi ~ uniform(-1, 1);
  sigma ~ cauchy(0, 5);
  mu ~ cauchy(0, 10);
  h[1] ~ normal(mu, sigma / sqrt(1 - phi * phi));
  for (t in 2:N_t)
    h[t] ~ normal(mu + phi * (h[t - 1] -  mu), sigma);
  for (t in 1:N_t)
    y[t] ~ normal(0, exp(h[t] / 2));
}
