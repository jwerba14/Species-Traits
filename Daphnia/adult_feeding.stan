data {
  int<lower=1> N; // data point length
  int<lower=1> L; // literature length 
  vector[N] chl; // initial chlorophyll
  real diff[N]; // chl change
  vector[L] lit_chl; // literature algal conc
  vector[L] diff_lit; // literature feeding rates
  vector[L] sd_lit; // literatrue SD
}

parameters {
  //for model
  real<lower=0> slope_bar;
  real<lower=0> sigma;
  real<lower=0> sigma_slope; 
  vector[L+1] eps_slope;
  
} 



model {
  // priors
  
  real fr[N]; // daily feeding rate
  real fr_lit[L];
  
  real slope[L+1];
  for(i in 1:(L+1)){
      slope[i] = (slope_bar) + (sigma_slope)*eps_slope[i];
  }
 
    
 sigma ~ cauchy(0,2);
 slope_bar ~ lognormal(0,0.5);
 sigma_slope ~ lognormal(0,0.5);


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
   diff_lit[i] ~ normal(fr_lit[i], sd_lit[i]); 
 }

}


