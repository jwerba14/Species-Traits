data {
  int<lower=0> N; 
  real diff [N]; // nh4 change
} 
parameters {
  real b;
  real tau;
} 
transformed parameters {
real sigma; 
  sigma = 1 / sqrt(tau); 
}
model {
  // priors
  b ~ normal(0.0, 1000);
  tau ~ gamma(.0001, .0001);
  diff ~ normal(b, sigma);   
}
