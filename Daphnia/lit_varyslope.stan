data {
  int<lower = 1> L;            // literature length 
  vector[L] lit_chl;           // literature algal conc
  vector[L] diff_lit;          // literature feeding rates
  vector<lower = 0>[L] sd_lit; // literatrue SD
 
}

parameters {

  // for model
  real slope_bar;
  //real<lower = 0> sigma;
  real<lower = 0> sigma_slope; 
  vector<lower=0>[L] eps_slope;
  
  
  
} 



model {

  // priors
  real fr_lit[L];
  real slope[L];

  for(i in 1:L){
   //assume there's no measurement error in the study
    slope[i] = slope_bar + sigma_slope*eps_slope[i];
  }
 
  // all_sd ~ lognormal(meanlog, sdlog);
  
    
// sigma     ~ cauchy(0,2);
sigma_slope ~ cauchy(0,2);
 

 slope_bar   ~ lognormal(0,0.5);
 // sigma_slope ~ lognormal(0,0.5);
 // meanlog     ~ lognormal(-1,0.5);
 // sdlog       ~ lognormal(-2,0.5); 
 
  
 eps_slope ~ lognormal(0,1);

// likelihood
 
for(i in 1:L){
  fr_lit[i] = slope_bar*lit_chl[i]; // expected change
  diff_lit[i] ~ normal(fr_lit[i], sd_lit[i]); 
 }

}


