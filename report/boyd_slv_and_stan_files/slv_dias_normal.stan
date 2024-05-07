data{
  int<lower=0> T;   // time points (equally spaced)
  vector[T] y;      // mean corrected return at time t
}
parameters{
  real mu;                      // mean log volatility
  real<lower=-1,upper=1> phiT;  // persistence of volatility
  real<lower=0> s2;             // white noise shock scale
  vector[T] h;                  // log volatility at time t
}
transformed parameters{
 real<lower=-1,upper=1> phi;
 real<lower=0> sigma;
 phi = (2*phiT - 1);
 sigma = pow(s2,2);
}
model{
  mu ~ normal(-10, 1);
  phiT ~ beta(20, 1.5);
  s2 ~ gamma(0.5, 1/(0.1*2)); //s2 ~ inv_gamma(2.5,0.025); // s2 ~ scaled_inv_chi_square(10, 0.05);            // perguntar s2 ~ cauchy(0,5);

  //--- Sampling volatilitys:
  h[1] ~ normal(mu, sigma/sqrt(1 - phi*phi));
  for(t in 2:T)
     {
      h[t] ~ normal(mu + phi*(h[t - 1] - mu), sigma);
     }
  
  //--- Sampling observations:  
  for(t in 1:T)
     {
      y[t] ~ normal(0, exp(h[t]/2));
     }
}
//--- To calculate the DIC, WAIC and LOO:
generated quantities{
 real loglik;
 vector[T] loglik1;
 loglik = 0;
 for(t in 1:T)
    {
     loglik = loglik + normal_lpdf(y[t]| 0, exp(h[t]/2));      //dic
     loglik1[t] = normal_lpdf(y[t]| 0, exp(h[t]/2));           //waic
    }
}
