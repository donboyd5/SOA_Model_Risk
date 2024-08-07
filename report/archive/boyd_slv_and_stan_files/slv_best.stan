// https://github.com/m-clark/models-by-example/blob/main/bayesian-stochastic-volatility.Rmd
data {
  int<lower = 0> N_t;                       // N time points (equally spaced)
  vector[N_t] y;                            // mean corrected response at time t
}

parameters {
  real mu;                                  // mean log volatility
  real<lower = -1,upper = 1> phi;           // persistence of volatility
  real<lower = 0> sigma;                    // white noise shock scale
  vector[N_t] h_std;                        // standardized log volatility at time t
}

transformed parameters{
  vector[N_t] h;                            // log volatility at time t
  
  h    = h_std * sigma;
  h[1] = h[1] / sqrt(1-phi * phi);
  h = h + mu;
  
  for (t in 2:N_t)
    h[t] = h[t] + phi * (h[t-1] - mu);
}

model {
  //priors
  phi   ~ uniform(-1, 1);
  sigma ~ cauchy(0, 5);
  mu    ~ cauchy(0, 10);
  h_std ~ normal(0, 1);

  //likelihood
  y ~ normal(0, exp(h/2));
}

generated quantities{
  vector[N_t] y_rep;

  for (t in 1:N_t){
    y_rep[t] = normal_rng(0, exp(h[t]/2));
  }  
}
