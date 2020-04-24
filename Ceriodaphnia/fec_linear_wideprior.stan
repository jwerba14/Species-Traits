data {
  int<lower=1> N; // data point length
  vector[N] chl; //  chlorophyll
  real daily_fec[N]; // daily fecundity
}

parameters {
  real slope_bar;
  real<lower=0> sigma;
} 

model {
  //prior
  sigma ~ cauchy(0,2);
  slope_bar ~ normal(0,10);
  //likelihood
  daily_fec ~ normal(chl*slope_bar, sigma);

}

