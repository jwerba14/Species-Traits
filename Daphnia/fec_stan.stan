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
    m[i] = alpha * chl[i] / (chl[i] + beta) ;
  sigma = 1 / sqrt(tau); 
}
model {
  // priors
  alpha ~ normal(0.0, 1000); // maybe should be lognormal to constrain to biologically meaningful- then need to exp in model
  beta ~ normal(0.0, 1000);
  tau ~ cauchy(0,2);
  m ~ normal(0,1000);
  daily_fec ~ normal(m, sigma);   
}

generated quantities{  // not a necessity but is giving predictions and error -- posterior on mean and predictions
  real Y_mean[4]; 
  real Y_pred[4]; 
  for(i in 1:4){
    // Posterior parameter distribution of the mean
  //  Y_mean[i] = alpha * chl[i] / (chl[i] + beta);
    Y_mean[i] = alpha * chl[i] / (chl[i] + beta);
    // Posterior predictive distribution random number generator from normal
    Y_pred[i] = normal_rng(Y_mean[i], sigma);   
}
}





