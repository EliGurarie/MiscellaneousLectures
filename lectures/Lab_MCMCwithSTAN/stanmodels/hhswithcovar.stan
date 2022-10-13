data {
  int<lower=1> ncovars;               // 3 = intercept + elevation + latitude
  int<lower=0> n;                     // number of total observations
  int<lower=1> nsites;                // number of sites
  int<lower=1,upper=nsites> site[n];  // vector of site ID's
  real y[n];                          // flowering date
  real x[n];                          // year
  row_vector[ncovars] covars[n];      // matrix of: cbind(1, elevation, latitude)
} 
  
parameters {
  real mu_beta0[ncovars];   // regression coefficients of beta0
  real<lower=0> sd_beta0;   // s.d. of beta0
  real mu_beta1;            // mean of beta1
  real<lower=0> sd_beta1;   // s.d. of beta1
  
  // overall change point and s.d. of process
  real<lower=min(x), upper = max(x)> tau;    
  real<lower=0> sigma;    
  
  vector[ncovars] beta0[nsites]; 
  vector[nsites] beta1;
}
  
  
model {
  vector[n] yhat;
  vector[n] x_beta0;
  
  // Priors
  sigma ~ normal(8, 4); 
  
  // Establish (fairly well-informed) hierarchical priors
  
  mu_beta0[1] ~ normal(130,20);
  mu_beta0[2] ~ normal(0,20); // per degree latitude
  mu_beta0[3] ~ normal(0,20); // per 1km elevation 
  sd_beta0 ~ normal(4,4);
  
  mu_beta1 ~ normal(0, 10);
  sd_beta1 ~ normal(0.2, 0.2);
  tau ~ uniform(min(x), max(x));
   
  for (i in 1:nsites){
    beta0[i] ~ normal(mu_beta0, sd_beta0);
    beta1[i] ~ normal(mu_beta1, sd_beta1);  
  }

  // The model!
    
  for (i in 1:n){
      x_beta0[n] = covars[i] * beta0[site[i]];
      yhat[i] = x_beta0[n] + int_step(x[i]-tau) * beta1[site[i]] * (x[i] - tau);
  }
  
  // likelihood / probability model   
  y ~ normal(yhat, sigma); 
} 