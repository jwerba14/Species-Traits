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
  real<lower=0> sigma_alpha;
  real beta_bar;
  real<lower=0> sigma_beta;
  vector[L+1] eps_alpha;
  vector[L+1] eps_beta;
} 
transformed parameters {
  real alpha[L+1];
  real beta[L+1];
  for(i in 1:L+1){
      alpha[i] = alpha_bar + sigma_alpha*eps_alpha[i];
      beta[i] = beta_bar + sigma_beta*eps_beta[i];
  }
  
}
// allows for different alpha/beta by study
// results
model {
  // priors
  real m[N]; // daily fecundity
  real q[L]; // ?? BMB: maybe call this m_lit?
  
  // BMB: these are probably too broad!
 // alpha ~ normal(0.0, 1000); 
  //beta ~ normal(0.0, 1000);
  // especially tau: works for this example but try (half-)t or (half-)Cauchy
  //  prior on sigma
  tau ~ cauchy(0,3); // BMB: OK, but could be improved
 
  // sigma ~ cauchy(0,3)    // BMB: ?
  alpha_bar ~ normal(0.0, 1000);
  sigma_alpha ~ lognormal(0,1);
  beta_bar ~ normal(0,1000); //should be correlated with alpha? 
  sigma_beta ~ lognormal(0,1);
  
  for (i in 1:L+1){
  eps_alpha[i] ~ normal(0,1);
  eps_beta[i] ~ normal(0,1);
    // alpha[i] ~ normal(alpha_bar,sigma_alpha); // random effect, alpha random draw from distribution with mean alpha_bar, sd sigma_alpha
  }
  
  for (i in 1:N) { 
      m[i] = alpha[1] * chl[i] / (chl[i] + beta[1]) ;
      daily_fec[i] ~ normal(m[i], 1/sqrt(tau));
  }
  for (i in 1:L) {
      q[i] = alpha[i+1] * chl_lit[i] / (chl_lit[i] + beta[i+1]) ;
      daily_fec_lit[i] ~ normal(q[i], sd_lit);
  }
}


// OR ("non-centred parameterization"):
  // for(i in 1:L+1)
    // beta[i] ~ normal(beta_bar, sigma_beta);
  // might need to have a fairly informative prior (not much data)
  // could parameterize sigma_alpha as a *coefficient of variation*
  // i.e. relative to the value of alpha (maybe sd_log?)
  // may be easier to work with log_alpha and then alpha[i] = exp(log_alpha[i])

// http://nross626.math.yorku.ca/ICPSR2017/#5_non-linear_models_for_normal_responses:_asymptotic_functions_of_time
// http://www.ling.uni-potsdam.de/~vasishth/JAGSStanTutorial/SorensenVasishthMay12014.pdf
 // http://mc-stan.org/rstanarm/reference/priors.html
  // https://mc-stan.org/docs/2_21/functions-reference/cauchy-distribution.html
  // https://mc-stan.org/users/documentation/case-studies/divergences_and_bias.html

//generated quantities{  // not a necessity but is giving predictions and error -- posterior on mean and predictions
  //real Y_mean[4]; 
  //real Y_pred[4]; 
  //for(i in 1:4){
    // Posterior parameter distribution of the mean
  //  Y_mean[i] = alpha * chl[i] / (chl[i] + beta);
    //Y_mean[i] = alpha * chl[i] / (chl[i] + beta);
    // Posterior predictive distribution random number generator from normal
    //Y_pred[i] = normal_rng(Y_mean[i], sigma);   






