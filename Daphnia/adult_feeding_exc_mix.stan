data {
  int<lower=1> N; // data point length
  int<lower=1> L; // literature point length 
  int<lower=1> M; // unique studies
  vector[N] chl; // initial chlorophyll
  real diff[N]; // chl change
  vector[L] lit_chl; // literature algal conc
  vector[L] diff_lit; // literature feeding rates
  vector[L] sd_lit; // literatrue SD
  real diff_nh4 [N]; // nh4 change
}

parameters {
  //for model
  real slope_bar;
  real<lower=0> sigma;
  real<lower=0> sigma_slope; 
  vector[M+1] eps_slope;
  real er_slope; // slope excretion rate
  real<lower=0> er_sigma; // slope tau excretion rate
} 

model {
  // priors
  
  real fr[N]; // daily feeding rate
  real fr_lit[L];
  real er[N];
  
  real slope[M+1];
  for(i in 1:(M+1)){
     slope[i] = slope_bar + sigma_slope*eps_slope[i];
  }
 
    
 sigma ~ student_t(6,0,1.5);  //changed degrees of freedom from 7 to 5-- didnt work worked at 6
 slope_bar ~ normal(0,10); 
 sigma_slope ~ student_t(6,0,1.5); //changed sd to 2 from 1
 er_sigma ~ cauchy(0,3);
 er_sigma ~ cauchy(0,3);

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
 
for (i in 1:N){
 er[i] = fr[i] * er_slope;
 diff_nh4[i] ~ normal(er[i], er_sigma); 
 
} 

}

