data {
  int<lower=0> n; //
  vector[n] y; // Y Vector
  vector[n] x; // X Vector
}

parameters {
  real beta0;  // intercept
  real beta1;  // slope
  real<lower=min(x), upper=max(x)> tau;    // changepoint
  real<lower=0> sigma;   // variance
}

model {
    vector[n] yhat;

  // Priors
    sigma ~ inv_gamma(0.001, 0.001); 
    beta0 ~ normal(112,30);
    beta1 ~ normal(0, 2);
    tau ~ uniform(min(x), max(x));
  // Define the mean
  for(i in 1:n)
    yhat[i] = beta0 + int_step(x[i]-tau)*beta1*(x[i] - tau);
 
  // likelihood / probability model   
    y ~ normal(yhat, sigma); 
}
