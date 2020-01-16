data {
  int<lower=1> N; // data point length
  int<lower=1> L; // literature length 
  vector[N] chl; //  chlorophyll
  real daily_fec[N]; // daily fecundity
  vector[L] chl_lit; // literature algal conc
  real daily_fec_lit[L]; // literature feeding rates
  real sd_lit[L]; // literatrue SD
}

parameters {
  real slope_bar;
  real<lower=0> sigma;
  real<lower=0> sigma_slope; 
  vector[L+1] eps_slope;
} 

transformed parameters {
  real slope[L+1];
  for(i in 1:(L+1)){
      slope[i] = slope_bar + sigma_slope*eps_slope[i];
  }
  
}

model {
  // priors
  //m ~ normal(0.0, 1000); 
  //b ~ normal(0.0, 1000);
  real fr[N]; // daily feeding rate
  real fr_lit[L];
  
 sigma ~ cauchy(0,2);
 slope_bar ~ lognormal(0,1);
 sigma_slope ~ cauchy(0,2);
 
 for (i in 1:(L+1)){
  eps_slope[i] ~ normal(0,1);
  }

// likelihood
 for (i in 1:N){
 fr[i] = slope[1] * chl[i];
 daily_fec[i] ~ normal(fr[i], sigma);
 }

 for(i in 1:L){
   fr_lit[i] = slope[i+1]*chl_lit[i];
   daily_fec_lit[i] ~ normal(fr_lit[i], sd_lit[i]);
 }

}


