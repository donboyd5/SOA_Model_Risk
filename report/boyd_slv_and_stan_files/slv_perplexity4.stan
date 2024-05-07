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
}

transformed parameters {
  vector[N_t] h;               // Log volatility
  vector[N_t] mu_t;            // Time-varying drift
  cov_matrix[2] cm;            // Covariance matrix for shocks
  real E_v;
  real Var_v;
  real E_sigma;
  real E_sigma_sq;
  real E_mu;
  real A;
  real B;
  real C;

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
  
  // Calculate unconditional moments based on estimated parameters
  E_v = mu;
  Var_v = sigma^2 / (1 - phi^2);
  E_sigma = exp(E_v + 0.5 * Var_v);
  E_sigma_sq = exp(2 * E_v + 2 * Var_v);
  
  // Solve for A, B, and C to achieve target unconditional expected return
  real target_return = log(1 + 0.0875) / 12;
  real E_mu_target = target_return;
  
  matrix[3, 3] M = [[1, E_sigma, E_sigma_sq],
                    [0, 1, 2 * E_sigma],
                    [0, 0, 2]];
  vector[3] y_eq = [E_mu_target, 0, 0]';
  
  vector[3] x = M \ y_eq;
  
  A = x[1];
  B = x[2];
  C = x[3];
    
  // Calculate unconditional expected drift based on solved A, B, and C
  E_mu = A + B * E_sigma + C * E_sigma_sq;
  
  // Calculate mu_t based on solved A, B, and C
  for (t in 1:N_t) {
    mu_t[t] = A + B * exp(h[t]) + C * exp(2 * h[t]);
  }  
}

model {
  // Priors
  mu ~ normal(0, 10);
  phi ~ uniform(-1, 1);
  sigma ~ normal(0, 2); // cauchy(0, 2);
  rho ~ uniform(-1, 1);
  h_std ~ normal(0, 1); // Prior for standardized log volatility

  // Likelihood
  // for (t in 1:N_t) {
  //   y[t] ~ normal(0, exp(h[t] / 2) / sqrt(12)); // Likelihood without drift term
  // }
  
  // Likelihood
  for (t in 1:N_t) {
    y[t] ~ normal(mu_t[t] / 12, exp(h[t] / 2) / sqrt(12)); // Likelihood with drift term
    // y[t] ~ normal(mu_t[t], exp(h[t] / 2)); // Likelihood with drift term
  }
}

generated quantities {
  vector[N_t] y_rep;
  vector[2] shocks;
  vector[N_t] h_std_adjusted = h_std;
  
  // Handle t = 1 case separately
  shocks = multi_normal_rng([0, 0], cm);
  h_std_adjusted[1] += shocks[1];
  // y_rep[1] = normal_rng(mu_t[1] / 12, exp(h[1] / 2) / sqrt(12));
  y_rep[1] = normal_rng(mu_t[1], exp(h[1] / 2));
  
  // Loop from t = 2 to N_t
  for (t in 2:N_t) {
    shocks = multi_normal_rng([0, 0], cm);
    h_std_adjusted[t] += shocks[1];
    // y_rep[t] = normal_rng(mu_t[t] / 12, exp(h[t] / 2) / sqrt(12));
    y_rep[t] = normal_rng(mu_t[t], exp(h[t] / 2));
  }
}
