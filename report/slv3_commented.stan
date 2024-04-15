# https://mc-stan.org/docs/2_22/stan-users-guide/stochastic-volatility-models.html

data {
  int<lower=0> T;   // # time points (equally spaced)
  vector[T] y;      // mean corrected return at time t
}
parameters {
  real mu;                     // mean log volatility
  real<lower=-1,upper=1> phi;  // persistence of volatility
  real<lower=0> sigma;         // white noise shock scale
  // vector[T] h;                 // log volatility at time t
  vector[T] h_std;  // std log volatility time t
}
// new block 
transformed parameters {
  vector[T] h = h_std * sigma;  // now h ~ normal(0, sigma)
  h[1] /= sqrt(1 - phi * phi);  // rescale h[1]
  h += mu;
  for (t in 2:T)
    h[t] += phi * (h[t-1] - mu);
}
model {
  phi ~ uniform(-1, 1);
  sigma ~ cauchy(0, 5);
  mu ~ cauchy(0, 10);
  // h[1] ~ normal(mu, sigma / sqrt(1 - phi * phi));
  // for (t in 2:T)
  //   h[t] ~ normal(mu + phi * (h[t - 1] -  mu), sigma);
  h_std ~ std_normal();
  // for (t in 1:T)
  //   y[t] ~ normal(0, exp(h[t] / 2));
  y ~ normal(0, exp(h / 2));
}