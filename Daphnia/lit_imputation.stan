data {
  int<lower=1> L; // literature length 
  int<lower =1> miss; // number lit without SD
  vector[L] lit_chl; // literature algal conc
  vector[L] diff_lit; // literature feeding rates
  vector[L] sd_lit; // literatrue SD
  int sd_index[miss]; // index of missing sd, of length miss
}

parameters {
  //for model
  real slope_bar;
  real<lower=0> sigma;
  real<lower=0> sigma_slope; 
  vector[L] eps_slope;
  
  //for imputation
  vector[1] shape;
  vector[1] scale;
  
  //missing
  vector[miss] imp_sd;
  
} 

transformed parameters {
   // Recombine data
    matrix[L,3] Data; // 1: feeding, 2: chl, 3: sd
    Data[,2] = lit_chl;
    Data[,3] = sd_lit; Data[sd_index,3] = imp_sd;
    Data[,1] = diff_lit;
    
}

model {
  // priors
  real fr_lit[L];
  
  real slope[L];
  for(i in 1:(L)){
      slope[i] = exp(slope_bar) + sigma_slope*eps_slope[i];
  }
 
  for(i in 1:L){
        Data[i,3] ~ gamma(shape,scale);
    }
    
 sigma ~ cauchy(0,2);
 slope_bar ~ lognormal(0,1);
 sigma_slope ~ cauchy(0,2);
 shape ~ gamma(0.001,0.001);
 scale ~ gamma(0.001,0.001); 
 
 for (i in 1:(L)){
  eps_slope[i] ~ normal(0,1);
  }

// likelihood
 

 for(i in 1:L){
   fr_lit[i] = slope[i]*lit_chl[i];
   diff_lit[i] ~ normal(fr_lit[i], sd_lit[i]); 
 }

}


