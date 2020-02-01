data {
  int<lower=0> N; 
  vector[N] chl; // chlorophyll
  real daily_fec[N]; //fecundity
} 
parameters {
  real log_alpha; 
  real log_beta; 
  real<lower=0> tau;
} 
transformed parameters {
real sigma; 
  real m[N];
  for (i in 1:N) 
    m[i] = (log_alpha) * log(chl[i]) / (log(chl[i]) + (log_beta)) ;
  sigma = 1 / sqrt(tau); 
}
model {
  // priors
  log_alpha ~ lognormal(0,1); // maybe should be lognormal to constrain to biologically meaningful- then need to exp in model
  log_beta ~ lognormal(0,1);
  tau ~ cauchy(0,2);
  m ~ lognormal(0,1);
  daily_fec ~ normal(exp(m), sigma);   
}







