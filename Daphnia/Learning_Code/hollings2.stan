data {
  int<lower=0> N; 
  vector[N] chl; // chlorophyll
  real daily_fec[N]; //fecundity
} 
parameters {
  real alpha; 
  real beta; 
  real<lower=0> tau;
} 
transformed parameters {
real sigma; 
  real m[N];
  for (i in 1:N) 
    m[i] = alpha * chl[i] / (1+chl[i]*beta*alpha) ;
  sigma = 1 / sqrt(tau); 
}
model {
  // priors
  alpha ~ normal(0,100); // maybe should be lognormal to constrain to biologically meaningful- then need to exp in model
  beta ~ normal(0,100);
  tau ~ cauchy(0,2);
  m ~ normal(0,10);
  daily_fec ~ normal(m, sigma);   
}



