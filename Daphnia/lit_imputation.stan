data {
  int<lower = 1> L;            // literature length 
  int<lower = 1> miss;         // number lit without SD
  vector[L] lit_chl;           // literature algal conc
  vector[L] diff_lit;          // literature feeding rates
  vector<lower = 0>[L] sd_lit; // literatrue SD
  int sd_index[miss];          // index of missing sd, of length miss
}

parameters {

  // for model
  real slope_bar;
  //real<lower = 0> sigma;
  // real<lower = 0> sigma_slope; 
  // vector[L] eps_slope;
  
  // for imputation
  // real meanlog;    
  // real<lower = 0> sdlog;
  
  // missing
  vector<lower = 0>[miss] imp_sd;
  
} 

transformed parameters {

  vector<lower = 0>[L] all_sd;

  all_sd = sd_lit;
  all_sd[sd_index] = imp_sd;
    
}

model {

  // priors
  real fr_lit[L];
  real slope[L];

  for(i in 1:L){
    // forget about estimating different slopes for every
    // study? that OR assume there's no measurement error in the study
    slope[i] = slope_bar; // + sigma_slope*eps_slope[i];
  }
 
  // all_sd ~ lognormal(meanlog, sdlog);
  all_sd ~ lognormal(-1,1);
    
// sigma     ~ cauchy(0,2);
// sigma_slope ~ cauchy(0,2);
 
 //sigma       ~ lognormal(-2,0.5); 
 slope_bar   ~ lognormal(0,0.5);
 // sigma_slope ~ lognormal(0,0.5);
 // meanlog     ~ lognormal(-1,0.5);
 // sdlog       ~ lognormal(-2,0.5); 
 
  
 // eps_slope ~ normal(0,1);

// likelihood
 
for(i in 1:L){
  fr_lit[i] = slope_bar*lit_chl[i]; // expected change
  diff_lit[i] ~ normal(fr_lit[i], all_sd[i]); 
 }

}


