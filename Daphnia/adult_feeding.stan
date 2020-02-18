data {
  int<lower=1> N; // data point length
  int<lower=1> L; // literature length 
  int<lower =1> miss; // number lit without SD
  vector[N] chl; // initial chlorophyll
  real diff[N]; // chl change
  vector[L] lit_chl; // literature algal conc
  vector[L] diff_lit; // literature feeding rates
  vector[L] sd_lit; // literatrue SD
  int sd_index[miss]; // index of missing sd, of length miss
}

parameters {
  //for model
  real<lower=0> slope_bar;
  real<lower=0> sigma;
  real<lower=0> sigma_slope; 
  vector[L+1] eps_slope;
  
  //for imputation
  real meanlog;
  real<lower=0> sdlog;
  
  //missing
  vector<lower = 0>[miss] imp_sd;
  
} 

transformed parameters {
   // Recombine data
   
   vector<lower = 0>[L] all_sd;
    all_sd = sd_lit;
    all_sd[sd_index] = imp_sd;
    
}

model {
  // priors
  //m ~ normal(0.0, 1000); 
  //b ~ normal(0.0, 1000);
  real fr[N]; // daily feeding rate
  real fr_lit[L];
  
  real slope[L+1];
  for(i in 1:(L+1)){
      slope[i] = exp(slope_bar) + exp(sigma_slope)*eps_slope[i];
  }
 
   all_sd ~ lognormal(meanlog, sdlog);
    
 sigma ~ cauchy(0,2);
 slope_bar ~ lognormal(1,0.5);
 sigma_slope ~ lognormal(0,0.5);
 meanlog ~ lognormal(-1,0.5);
 sdlog ~ lognormal(-2,0.5); 
 
 for (i in 1:(L+1)){
  eps_slope[i] ~ normal(0,1);
  }

// likelihood
 for (i in 1:N){
 fr[i] = slope[1] * chl[i];
 diff[i] ~ normal(fr[i], sigma);
 }


 for(i in 1:L){
   fr_lit[i] = slope[i+1]*lit_chl[i];
   diff_lit[i] ~ normal(fr_lit[i], exp(all_sd[i])); 
 }

}


