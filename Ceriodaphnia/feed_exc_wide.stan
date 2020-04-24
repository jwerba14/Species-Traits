
data {
  int<lower=0> N; // length of data
  real diff_nh4 [N]; // nh4 change
  real diff_chl [N]; // chl change
  real chl [N]; // initial chl
}

parameters {
  real fr_slope;  // slope feeding rate
  real<lower=0> fr_sigma;//  slope tau feeding rate
  real er_slope; // slope excretion rate
  real<lower=0> er_sigma; // slope tau excretion rate
}




model {
  
  real fr[N]; // daily feeding rate
  real er[N];    
  
  fr_slope ~ lognormal(0,1);
  er_slope ~ lognormal(0,1);
  fr_sigma ~ cauchy(0,2);
  er_sigma ~ cauchy(0,2);

  
for (i in 1:N){
 fr[i] = fr_slope * chl[i];
 diff_chl[i] ~ normal(fr[i], fr_sigma);
 }
 
for (i in 1:N){
 er[i] = fr[i] * er_slope;
 diff_nh4[i] ~ normal(er[i], er_sigma); 
 
}
 }

