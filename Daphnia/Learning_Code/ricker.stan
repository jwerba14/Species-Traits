data {
  int<lower=0> N; 
  vector[N] chl; // chlorophyll
  real daily_fec[N]; //fecundity
} 
parameters {
  real a; 
  real b; 
  real<lower=0> tau;
} 
transformed parameters {
real sigma; 
  real m[N];
  for (i in 1:N) 
    m[i] = a*chl[i]*exp(-b*chl[i]);
  sigma = 1 / sqrt(tau); 
}
model {
  // priors
  a ~ normal(0,10); 
  b ~ normal(0,10);
  tau ~ cauchy(0,2);
  m ~ normal(0,10);
  daily_fec ~ normal(m, sigma);   
}

