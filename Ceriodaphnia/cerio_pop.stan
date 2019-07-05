data {
  int<lower=0> N; 
  vector[N] chl; // lag week chlorophyll
  real pop [N]; //ceriodaphnia population change by week
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
  pop ~ normal(m*chl + b, sigma);   
}

generated quantities{  // not a necessity but is giving predictions and error -- posterior on mean and predictions
  real Y_mean[N]; 
  real Y_pred[N]; 
  for(i in 1:N){
    // Posterior parameter distribution of the mean
  //  Y_mean[i] = alpha * chl[i] / (chl[i] + beta);
    Y_mean[i] = m*chl[i] + b;
    // Posterior predictive distribution random number generator from normal
    Y_pred[i] = normal_rng(Y_mean[i], sigma);   
}
}

