data {
  int<lower = 1> L;            // literature length 
  int<lower=1> M;              // unique studies length
  vector[L] lit_chl;           // literature algal conc
  vector[L] diff_lit;          // literature feeding rates
  vector<lower = 0>[L] sd_lit; // literatrue SD
 
}

parameters {

  // for model
  real slope_bar;
  real<lower = 0> sigma_slope; 
  vector<lower=0>[L] eps_slope;
  
} 

model {

  // priors
  real fr_lit[L];
  real slope[M];

  for(i in 1:M){
   //assume there's no measurement error in the study
    slope[i] = slope_bar + sigma_slope*eps_slope[i];
  }
 
  // all_sd ~ lognormal(meanlog, sdlog);
  
    
// sigma     ~ cauchy(0,2);
sigma_slope ~ cauchy(0,2);
 

 slope_bar   ~ normal(0,10);
 
 
  
 eps_slope ~ lognormal(0,1);

// likelihood
 
for(i in 1:M){
  fr_lit[i] = slope_bar*lit_chl[i]; // expected change
  diff_lit[i] ~ normal(fr_lit[i], sd_lit[i]); 
 }

}


