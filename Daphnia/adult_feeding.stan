data {
  int<lower=1> N; // data point length
  int<lower=1> L; // literature length 
  int<lower =1> M; // number lit with SD
  int<lower =1> P; // number lit without SD
  vector[N] chl; // initial chlorophyll
  real diff[N]; // chl change
  vector[L] lit_chl; // literature algal conc
  real diff_lit[L]; // literature feeding rates
  real sd_lit[M]; // literatrue SD
  
}

parameters {
  real slope_bar;
  real<lower=0> sigma;
  real<lower=0> sigma_slope; 
  vector[L+1] eps_slope;
  real<lower=0> miss_sd;
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
 miss_sd ~ cauchy(0,2); // want to actually estimate based on known SDs and allow to vary by study...
 
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
   diff_lit[i] ~ normal(fr_lit[i], sd_lit[i]); //need to incorporate unk estimates here
 }

}


