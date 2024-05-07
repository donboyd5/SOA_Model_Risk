
data {
  int<lower=0> N_t;        // Number of time points
  real y[N_t];             // Log returns
}

parameters {
  real<lower=0> mu;      // Mean log volatility
  real<lower=0> phi;     // Mean reversion speed
  real<lower=0> sigma;   // Volatility of volatility
  real v[N_t];             // Latent log volatility
}

model {
  v[1] ~ normal(0, 1);   // Prior for the first volatility point
  for (t in 2:N_t) {
    v[t] ~ normal(mu + phi * (v[t-1] - mu), sigma);  // Prior for subsequent volatilities
  }
  for (t in 1:N_t) {
    y[t] ~ normal(0, exp(v[t] / 2));  // Observational model
  }
}
