data {
  int<lower=0> T;   // number of time points
  vector[T] r;      // observed log returns 
}
parameters {
  real mu;                    // mean log volatility
  real<lower=0> sigma_h;      // volatility of log volatility
  real<lower=0,upper=1> phi;  // persistence of log volatility
  real<lower=0> sigma_y;      // volatility of returns
  vector[T] h;                // log volatility at each time point
  real A;
  real B;
  real C;
  real<lower=-1,upper=1> rho; // correlation coefficient
}
model {
  // Priors
  mu ~ normal(0, 10);
  sigma_h ~ cauchy(0, 5);
  phi ~ beta(2, 2);
  sigma_y ~ cauchy(0, 5);
  A ~ normal(0, 10);
  B ~ normal(0, 10);
  C ~ normal(0, 10);
  rho ~ uniform(-1, 1);

  // Likelihood
  h[1] ~ normal(mu, sigma_h / sqrt(1 - phi^2)); // stationary distribution
  for (t in 2:T) {
    h[t] ~ normal(mu + phi * (h[t-1] - mu), sqrt(sigma_h^2 - pow(rho * sigma_h * sigma_y * exp(-0.5 * h[t-1]), 2)));
  }
  for (t in 1:T) {
    real mu_t = A + B * exp(h[t] / 2) + C * exp(h[t]);
    r[t] ~ normal(mu_t, sigma_y * exp(h[t] / 2));
  }
}
