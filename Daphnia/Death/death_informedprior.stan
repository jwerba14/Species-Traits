data { 
   int<lower=0> N; //rows of my data
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
   b ~ normal(55,22);
   tau ~ gamma(.0001, .0001);
   survival ~ normal(exp(-days/b),sigma);
   
 } 
 
 
