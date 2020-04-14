data {
  int<lower=0> N; 
  vector[N] chl; // chlorophyll
  real days_to_adult[N]; //growth
} 
parameters {
  real alpha; 
  real beta; 
  real tau;
} 
transformed parameters {
real sigma; 
  real m[N];
  for (i in 1:N) 
    m[i] = (exp(alpha) * chl[i]) / (exp(beta) + chl[i]) ; // exp param to force whole equation to be postitive
  sigma = 1 / sqrt(tau); 
}
model {
  // priors
  alpha ~ normal(0.0, 1000); 
  beta ~ normal(0.0, 1000);
  tau ~ gamma(.0001, .0001);
  days_to_adult ~ normal(m, sigma);   
}

generated quantities{  // not a necessity but is giving predictions and error -- posterior on mean and predictions
  real Y_mean[N]; 
  real Y_pred[N]; 
  for(i in 1:N){
    // Posterior parameter distribution of the mean
  //  Y_mean[i] = alpha * chl[i] / (chl[i] + beta);
    Y_mean[i] = (exp(alpha) * chl[i]) / (exp(beta) + chl[i]);
    // Posterior predictive distribution random number generator from normal
    Y_pred[i] = normal_rng(Y_mean[i], sigma);   
}
}





