data {
  int<lower=0> N; // data point length
  vector[N] chl; // initial chlorophyll
  real diff [N]; // chl change
  int<lower=0> L; // literature length 
  vector[L] lit_chl; // literature algal conc
  real diff_lit [L]; // literature feeding rates
} 
parameters {
  real slope[L+1]; 
  real tau;
  vector[2] beta; //fixed intercept and slope
  real<lower=0> sigma_e; //error sd
  real<lower=0> sigma_u; //subj sd

} 

}
model {
  // priors
  //m ~ normal(0.0, 1000); 
  //b ~ normal(0.0, 1000);
  real mu;
 //priors
 u ~ normal(0, sigma_u); //subj random effects

// likelihood
 for (i in 1:N){
 mu = beta[1] + u[subj[i]]] + beta[2] * chl[i];
diff[i] ~ lognormal(mu, sigma_e);
     
}


