data {
  int<lower=0> N_t;            // Number of time points
  vector[N_t] y;               // Log returns (not mean-centered)
}

parameters {
  real mu;                     // Mean log volatility
  real<lower=-1, upper=1> phi; // Persistence of volatility
  real<lower=0> sigma;         // Volatility shock scale
  real<lower=-1, upper=1> rho; // Correlation between shocks to volatility and returns
  vector[N_t] h_std;           // Standardized log volatility
  real A;                      // Intercept of the drift function
  real B;                      // Linear coefficient of the drift function
  real C;                      // Quadratic coefficient of the drift function
}

transformed parameters {
  vector[N_t] h;               // Log volatility
  cov_matrix[2] cm;            // Covariance matrix for shocks
  vector[N_t] mu_t;            // Time-varying drift
  real E_mu;
  real E_v = mu;
  real Var_v = sigma^2 / (1 - phi^2);
  real E_sigma = exp(E_v + 0.5 * Var_v);
  real E_sigma_sq = exp(2 * E_v + 2 * Var_v);
  
  E_mu = A + B * E_sigma + C * E_sigma_sq;

  // Initialize covariance matrix
  cm[1, 1] = sigma^2;          // Variance of volatility shocks
  cm[2, 2] = 1;                // Assumed variance of return shocks
  cm[1, 2] = rho * sigma;      // Symmetric covariance
  cm[2, 1] = cm[1, 2];

  // Setup the first volatility term
  h[1] = mu + h_std[1] * sigma / sqrt(1 - phi * phi);

  // Compute h values based on h_std
  for (t in 2:N_t) {
    h[t] = mu + phi * (h[t-1] - mu) + sigma * h_std[t]; // Update volatility
  }

  // Compute the time-varying drift
  for (t in 1:N_t) {
    mu_t[t] = A + B * exp(h[t]) + C * exp(2 * h[t]);
  }
}

model {
  // Priors for other parameters
  mu ~ normal(0, 10);
  phi ~ uniform(-1, 1);
  sigma ~ cauchy(0, 2);
  rho ~ uniform(-1, 1);
  h_std ~ normal(0, 1);

  // Target an unconditional expected return of 8.75%
  real target_return = log(1 + 0.0875) / 12;
  target += normal_lpdf(E_mu | target_return, 0.001);

  // Likelihood
  for (t in 1:N_t) {
    y[t] ~ normal(mu_t[t] / 12, exp(h[t] / 2) / sqrt(12));
  }
}

generated quantities {
  vector[N_t] y_rep;
  vector[2] shocks;            // To hold correlated shocks
  vector[N_t] h_std_adjusted = h_std;  // Create a copy of h_std to adjust
  
  // Handle t = 1 case
  y_rep[1] = normal_rng(mu_t[1] / 12, exp((mu + sigma * h_std_adjusted[1]) / 2) / sqrt(12));
  
  // Loop starts from 2 since t=1 is handled separately
  for (t in 2:N_t) {
      shocks = multi_normal_rng([0, 0], cm); // Draw correlated shocks
      h_std_adjusted[t] += shocks[1]; // Update the copy of h_std for generated data
      y_rep[t] = normal_rng(mu_t[t] / 12, exp((mu + phi * (h_std_adjusted[t-1] - mu) + sigma * h_std_adjusted[t]) / 2) / sqrt(12));
  }  
}
