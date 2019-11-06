data {
  int<lower=0> N; 
  int<lower=0> L;
  vector[N] chl; // chlorophyll
  real daily_fec[N]; //fecundity
  vector[L] chl_lit; // chlorophyll from literature
  real daily_fec_lit[L]; // fecundity from literature
  real sd_lit[L]; //  sd from literature
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
    m[i] = alpha * chl[i] / (chl[i] + beta) ;
  sigma = 1 / sqrt(tau); 
  for (i in 1:L) 
    m[i] = alpha * chl_lit[i] / (chl_lit[i] + beta) ;
  sigma[i] = sd_lit[i];
}
model {
  // priors
  alpha ~ normal(0.0, 1000); 
  beta ~ normal(0.0, 1000);
  tau ~ gamma(.0001, .0001);
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





