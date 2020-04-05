data { 
   int<lower=0> N; 
   // int<lower=0> L;
   vector[N] days; 
   vector[N] survival;
 } 
  
 parameters { 
   real<lower=0> b; 
   real tau;
 } 
transformed parameters {
real sigma; 
sigma = 1 / sqrt(tau); 
}
  
 model { 
   b ~ lognormal(0.0,1);
   tau ~ gamma(.0001, .0001);
   survival ~ normal(exp(days/b),sigma);
   
 } 
 
 
