data {
  int<lower=0> N; 
  vector[N] chl; // chlorophyll
  real daily_fec[N]; //fecundity
  int<lower = 0, upper = 1> run_estimation; // a switch to evaluate the likelihood
} 
parameters {
  real<lower=0> alpha; 
  real<lower=0> beta; 
  real<lower=0> tau;
} 
transformed parameters {
real sigma; 
  real m[N];
  for (i in 1:N) 
    m[i] = alpha * (chl[i]) / (chl[i] + beta) ;
  sigma = 1 / sqrt(tau); 
}
model {
  // priors
  alpha ~ normal(0,100); 
  beta ~ normal(0,100);
  tau ~ cauchy(0,2);
  m ~ normal(0,100);
  if(run_estimation==1){
  daily_fec ~ normal(m, sigma);   
}
}

generated quantities {
  vector[N] y_sim;
  for(i in 1:N) {
    y_sim[i] = normal_rng(m[i], sigma);
  }
}

