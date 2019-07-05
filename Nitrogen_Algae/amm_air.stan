data {
  int<lower=0> N; 
  vector[N] lagN; // lag day Nh4
  real NH [N]; // NH4
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
  NH ~ normal(m*lagN + b, sigma);   
}

generated quantities{  // not a necessity but is giving predictions and error -- posterior on mean and predictions
  real Y_mean[N]; 
  real Y_pred[N]; 
  for(i in 1:N){
    // Posterior parameter distribution of the mean
    Y_mean[i] = m*lagN[i] + b;
    // Posterior predictive distribution random number generator from normal
    Y_pred[i] = normal_rng(Y_mean[i], sigma);   
}
}

