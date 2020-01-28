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
  real<lower=0> tau;
  real alpha_bar;
  real<lower=0> log_sigma_alpha;
  real beta_bar;
  real<lower=0> log_sigma_beta;
  vector[L+1] eps_alpha;
  vector[L+1] eps_beta;
} 
transformed parameters {
  real alpha[L+1];
  real beta[L+1];
  for(i in 1:L+1){
      alpha[i] = alpha_bar + exp(log_sigma_alpha)*eps_alpha[i];
      beta[i] = beta_bar + exp(log_sigma_beta)*eps_beta[i];
  }
  
}
// allows for different alpha/beta by study
// results
model {
  // priors
  real m[N]; // daily fecundity
  real q[L]; // ?? BMB: maybe call this m_lit?
  
  tau ~ cauchy(0,3); // BMB: OK, but could be improved
 
  // sigma ~ cauchy(0,3)    // BMB: ?
  alpha_bar ~ normal(0.0, 1000);
  log_sigma_alpha ~ lognormal(0,1);
  beta_bar ~ normal(0,1000); //should be correlated with alpha? 
  log_sigma_beta ~ lognormal(0,1);
  
  for (i in 1:L+1){
  eps_alpha[i] ~ normal(0,1);
  eps_beta[i] ~ normal(0,1);
      }
  
  for (i in 1:N) { 
      m[i] = alpha[1] * chl[i] / (chl[i] + beta[1]) ;
      daily_fec[i] ~ normal(m[i], 1/sqrt(tau));
  }
  for (i in 1:L) {
      daily_fec_lit[i] ~ normal(alpha[i+1], sd_lit);
  }
}


  






