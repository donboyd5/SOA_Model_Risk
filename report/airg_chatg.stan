data {
  int<lower=1> T;            // number of time points
  vector[T] log_returns;     // log returns of the stock
}

parameters {
  real<lower=0> tau;         // long run target volatility
  real<lower=0> phi;         // strength of mean reversion
  real<lower=0> sigma_v;     // std deviation of the log volatility process
  real<lower=-1, upper=1> rho; // correlation between shocks
  real A;                    // drift as volatility approaches zero
  real B;                    // quadratic coefficient
  real C;                    // quadratic coefficient
  matrix[2, T] Z;            // 2xT matrix of standard normal shocks
}

transformed parameters {
  vector[T] mu;         // mean log return
  vector[T] v;          // log volatility
  vector[T] s;          // stock return process shocks

  // Cholesky factor of the covariance matrix
  matrix[2,2] L;
  L[1,1] = 1;
  L[1,2] = 0;
  L[2,1] = rho;
  L[2,2] = sqrt(1 - square(rho));

  vector[2] correlated_shocks;

  v[1] = 0;  // Initialize the first value of v, if not given explicitly

  for (t in 2:T) {
    // Apply the Cholesky factor to generate correlated shocks at each time step
    correlated_shocks = L * col(Z, t);

    mu[t] = A + B * v[t-1] + C * square(v[t-1]);
    v[t] = phi * tau + (1 - phi) * v[t-1] + sigma_v * correlated_shocks[1];
    s[t] = mu[t] + sqrt(exp(v[t])) * correlated_shocks[2];
  }
}

model {
  // Priors (customize as necessary)
  tau ~ normal(0, 1);
  phi ~ normal(0, 1);
  sigma_v ~ normal(0, 1);
  rho ~ normal(0, 1);
  A ~ normal(0, 1);
  B ~ normal(0, 1);
  C ~ normal(0, 1);

  // Each column of Z follows a standard multivariate normal distribution
  for (t in 1:T) {
    Z[, t] ~ multi_normal(rep_vector(0, 2), diag_matrix(rep_vector(1, 2)));
  }

  // Likelihood
  for (t in 2:T) {
    log_returns[t] ~ normal(s[t-1], exp(v[t]));
  }
}


