data {
  int<lower=0> N; 
  vector[N] chl; // initial chlorophyll
  real diff [N]; // chl change
} 
parameters {
  real m; 
  real b;
  real tau;
} 
transformed parameters {
real sigma; 
  sigma = 1 / sqrt(tau); 
}
model {
  // priors
  m ~ normal(0.0, 1000); 
  b ~ normal(0.0, 1000);
  tau ~ gamma(.0001, .0001);
  diff ~ normal(m*chl + b, sigma);   
}


