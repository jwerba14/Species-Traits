data {
  int<lower=0> L;
  vector[L] chl_lit; // chlorophyll from literature
  real daily_fec_lit[L]; // fecundity from literature
  real sd_lit[L]; //  sd from literature
} 
parameters {
  real<lower=0> tau;
  real log_alpha_bar;
  real<lower=0> sigma_alpha;
  real log_beta_bar;
  real<lower=0> sigma_beta;
  vector[L] eps_alpha;
  vector[L] eps_beta;
} 
transformed parameters {
  real alpha[L];
  real beta[L];
  for(i in 1:L){
      alpha[i] = (log_alpha_bar) + sigma_alpha*eps_alpha[i];
      beta[i] = (log_beta_bar) + sigma_beta*eps_beta[i];
  }
  
}
// allows for different alpha/beta by study
// results
model {
  // priors
  real q[L]; // ?? BMB: maybe call this m_lit?
  
  // BMB: these are probably too broad!
 // alpha ~ normal(0.0, 1000); 
  //beta ~ normal(0.0, 1000);
  // especially tau: works for this example but try (half-)t or (half-)Cauchy
  //  prior on sigma
  tau ~ cauchy(0,3); // BMB: OK, but could be improved
 
  // sigma ~ cauchy(0,3)    // BMB: ?
  log_alpha_bar ~ lognormal(0, 1);
  sigma_alpha ~  cauchy(0,3);
  log_beta_bar ~ lognormal(0,1); //should be correlated with alpha? 
  sigma_beta ~ cauchy(0,3);
  
  for (i in 1:L){
  eps_alpha[i] ~ normal(0,1);
  eps_beta[i] ~ normal(0,1);
    // alpha[i] ~ normal(alpha_bar,sigma_alpha); // random effect, alpha random draw from distribution with mean alpha_bar, sd sigma_alpha
  }
  
  for (i in 1:L) {
      q[i] = alpha[i] * chl_lit[i] / (chl_lit[i] + beta[i]) ;
      daily_fec_lit[i] ~ normal(q[i], sd_lit);
  }
}










