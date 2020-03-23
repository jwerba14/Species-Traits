data {
  int<lower=1> N; // data point length
  int<lower=1> L; // literature point length 
  int<lower=1> M; // unique studies
  vector[N] chl; // initial chlorophyll
  real diff[N]; // chl change
  vector[L] lit_chl; // literature algal conc
  vector[L] diff_lit; // literature feeding rates
  vector[L] sd_lit; // literatrue SD
}

parameters {
  //for model
  real slope_bar;
  real<lower=0> sigma;
  real<lower=0> sigma_slope; 
  vector[M+1] eps_slope;
  
} 

model {
  // priors
  
  real fr[N]; // daily feeding rate
  real fr_lit[L];
  
  real slope[M+1];
  for(i in 1:(M+1)){
     slope[i] = slope_bar + sigma_slope*eps_slope[i];
  }
 
    
 sigma ~ cauchy(0,2);
 slope_bar ~ normal(0,10);
 sigma_slope ~ cauchy(0,2);


 for (i in 1:(M+1)){
  eps_slope[i] ~ normal(0,1);
  }

// likelihood
 for (i in 1:N){
 fr[i] = slope[1] * chl[i];
 diff[i] ~ normal(fr[i], sigma);
 }


 for(i in 1:M){
   fr_lit[i] = slope[i+1]*lit_chl[i];
   diff_lit[i] ~ normal(fr_lit[i], sd_lit[i]); 
 }

}

