data {
	int<lower=0> n;                     // number of total observations
	int<lower=1> nsites;                // number of sites
	int<lower=1,upper=nsites> site[n];  // vector of site IDs
	real y[n];                        // flowering date
	real x[n];                        // year
} 

parameters {
	real mu_beta0;  // mean intercept
	real<lower=0> sd_beta0;  // s.d. of intercept
	
	real mu_beta1;  // mean slope
	real<lower=0> sd_beta1;  // s.d. of intercept
	
	// overall changepoint and s.d.
	real<lower = min(x), upper = max(x)> tau;
	real<lower=0> sigma;    
	
	vector[nsites] beta0; // 
	vector[nsites] beta1; // unique intercept/slope site
}

model {
	vector[n] yhat;
	
	// Priors
	sigma ~ inv_gamma(0.001, 0.001);
	
	mu_beta0 ~ normal(130,20);
	sd_beta0 ~ inv_gamma(0.001, 0.001);
	
	mu_beta1 ~ normal(-.5, .5);
	sd_beta1 ~ inv_gamma(0.001, 0.001);
	
	tau ~ uniform(min(x), max(x));
	
	for (i in 1:nsites){
		beta0[i] ~ normal(mu_beta0, sd_beta0);
		beta1[i] ~ normal(mu_beta1, sd_beta1);  
	}
	
	// The model!
		for (i in 1:n)
			yhat[i] = beta0[site[i]] + int_step(x[i]-tau) * beta1[site[i]] * (x[i] - tau);
	
	// likelihood / probability model   
	y ~ normal(yhat, sigma); 
}
