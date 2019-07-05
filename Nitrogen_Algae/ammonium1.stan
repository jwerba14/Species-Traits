data {
  int<lower=0> N;  //number of data points
  int V[N]; // index of num measure per rep
  real y[N];  // change in ammonium 
}
parameters {
  real alpha[N];   // intercept- intercept only model 
  real mu_alpha;  // mean of alpha 
  real<lower=0> sigmasq_y;
  real<lower=0> sigmasq_alpha;
 
}
transformed parameters {
  real<lower=0> sigma_y;       // sigma in original bugs model
  real<lower=0> sigma_alpha; 
  sigma_y = sqrt(sigmasq_y);
  sigma_alpha = sqrt(sigmasq_alpha);
  
}
model {
  mu_alpha ~ normal(0, 100);
  sigmasq_y ~ inv_gamma(0.001, 0.001);
  sigmasq_alpha ~ inv_gamma(0.001, 0.001);
  alpha ~ normal(mu_alpha, sigma_alpha); // vectorized
 
  for (n in 1:N)
      y[n] ~ normal(alpha[V[n]], sigma_y); // alpha with whichever treatment n is assoc with

}


