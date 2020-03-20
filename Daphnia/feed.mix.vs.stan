data {
  int<lower=1> N; // data point length
  int<lower=1> L; // literature length 
  vector[N] chl; // initial chlorophyll
  real diff[N]; // chl change
  vector[L] lit_chl; // literature algal conc
  vector[L] diff_lit; // literature feeding rates
  vector[L] sd_lit; // literatrue SD
  int<lower = 1> miss;         // number lit without SD
  int sd_index[miss];          // index of missing sd, of length miss
}
  
  


parameters {
  //for model
  real<lower=0> slope_bar;
  real<lower=0> sigma;
  real<lower=0> sigma_slope; 
  vector<lower = 0>[miss] imp_sd;
} 



model {
  // priors
  
  real fr[N]; // daily feeding rate
  real fr_lit[L];

  
  real slope[L];
  real all_sd[L];
  
  for(i in 1:L){
    slope[i] = slope_bar;
  }

  for (i in 1:L) {
    all_sd[i] = sd_lit[i];
  }
  for (i in 1:miss) {
    imp_sd[i] ~ lognormal(-1,1);
    all_sd[sd_index[i]] = imp_sd[i];
  }
 
    
 sigma ~ cauchy(0,2);
 slope_bar ~ lognormal(0,0.5);
 sigma_slope ~ lognormal(0,0.5);


// likelihood
 for (i in 1:N){
 fr[i] = slope[1] * chl[i];
 diff[i] ~ normal(fr[i], sigma);
 }


 for(i in 1:L){
   fr_lit[i] = slope[i]*lit_chl[i];
   diff_lit[i] ~ normal(fr_lit[i], all_sd[i]); 
 }

}


