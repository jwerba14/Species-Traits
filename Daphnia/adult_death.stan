data { 
   int<lower=0> N; 
   real days; 
   real survival; 
 } 
  
 parameters { 
   real b; 
   real tau;
 } 
transformed parameters {
real sigma; 
  real m[N];
  for (i in 1:N) 
    m[i] = exp(-days/b) ;
  sigma = 1 / sqrt(tau); 
}
  
 model { 
   b ~ normal(0.0, 1000);
   tau ~ gamma(.0001, .0001);
   survival ~ normal(m,sigma);
   
 } 
 
 
