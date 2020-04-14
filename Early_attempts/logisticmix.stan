functions {
  real[] logisticmix(real t,
                  real[] y,
                  real[] theta_fit,
                  real[] x_r,
                  int[] x_i
                  ) {
    real dydt[x_i[1]];
    for (i in 1:x_i[1]){
      dydt[i] = (theta_fit[1]+theta_fit[2+x_i[1+i]]) * y[i] * (1-y[i]/(theta_fit[2]+theta_fit[8+x_i[1+i]])); 
    }
    return dydt;
  } 
}
data {
  int<lower=2> T;
  int<lower=1> rep;
  int<lower =1> ttt; //  treatment numbers repeated correctly- treat id
  real y0[rep]; 
  real z[T,rep];
  real t0;
  real ts[T];
  int<lower =1> K; // number of treatments 
  int<lower =1, upper = K> item[rep]; // treat id 
}
transformed data {
  real x_r[0];
  int x_i[1];
  x_i[1] = rep;
  
}
parameters {
  real theta[2];
  real<lower=0> sigma;
  vector[K] theta_r1; //treat deviations of theta [1]
  vector[K] theta_r2; //treat deviations of theta [2]
  real <lower = 0> sigma_r1; // treat sd of theta[1]
  real <lower = 0> sigma_r2; //treat sd of theta[2]
}
transformed parameters {
  real theta_fit[14];
  theta_fit[1] = theta[1];
  theta_fit[2] = theta[2];
  theta_fit[3] = theta_r1[1];
  theta_fit[4] = theta_r1[2];
  theta_fit[5] = theta_r1[3];
  theta_fit[6] = theta_r1[4];
  theta_fit[7] = theta_r1[5];
  theta_fit[8] = theta_r1[6];
  theta_fit[9] = theta_r2[1];
  theta_fit[10] = theta_r2[2];
  theta_fit[11] = theta_r2[3];
  theta_fit[12] = theta_r2[4];
  theta_fit[13] = theta_r2[5];
  theta_fit[14] = theta_r2[6];
}

model {
  real y_hat[T,rep];
  
  //priors
  theta ~ cauchy(0,2.5);
  sigma ~ normal(0,0.01);
  theta_r1 ~ normal(0, sigma_r1);
  theta_r2 ~ normal(0, sigma_r2);  
  //sigma_r1 ~ currently uniform zero, inf bc that is def
  //sigma_r2 ~ 
  
  //mod
  
  y_hat = integrate_ode_rk45(logisticmix, y0, t0, ts, theta_fit, x_r, x_i);
  for (t in 1:T) {
    for (i in 1:rep) {
      z[t,i] ~ normal(y_hat[t,i], sigma);
    }
  }
}
generated quantities{
  real y_pred[T,rep];
  real z_pred[T,rep];
  y_pred = integrate_ode_rk45(logisticmix, y0, t0, ts, theta_fit,x_r, x_i);
  for (t in 1:T) {
    for(i in 1:rep){
      z_pred[t,i] = y_pred[t,i] + normal_rng(0,sigma);
    }
  }
}


