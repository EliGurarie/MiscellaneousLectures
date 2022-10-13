data {
  int<lower=0> n; //
  vector[n] y; // Y Vector
  vector[n] x; // X Vector
}

parameters {
  real beta0;  					// intercept
  real beta1;  					// slope
  real<lower=0> sigma;	// standard deviation
}

model {
  //Declare variable
  vector[n] yhat;

  //Priors
  sigma ~ inv_gamma(0.001, 0.001); 
  beta0 ~ normal(0,1e2);
  beta1 ~ normal(0,1e2);

  //model   
  yhat = beta0 + beta1 * (x - mean(x));
  y ~ normal(yhat, sigma); 
}
