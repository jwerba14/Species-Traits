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
  real<lower=0> shape;
  real<lower=0> scale;
  
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
  //m ~ normal(0.0, 1000); 
  //b ~ normal(0.0, 1000);
  real fr[N]; // daily feeding rate
  real fr_lit[L];
  
  real slope[L+1];
  for(i in 1:(L+1)){
      slope[i] = (slope_bar) + sigma_slope*eps_slope[i];
  }
 
  for(i in 1:L){
        Data[i,3] ~ gamma(shape,(scale));
    }
    
 sigma ~ cauchy(0,2);
 slope_bar ~ normal(0,10);
 sigma_slope ~ cauchy(0,3);
 shape ~ normal(0,10);
 scale ~ normal(0,10); 
 
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


