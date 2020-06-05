functions {
    real[] myodes(
        real t, //time
        real[] y,  //state y[1] = ammonium rep 1 y[2] = chlorophyll rep 1 
        real [] p, //parameters  // p[1]=alpha;  p[2] = kappa p[3] = r p[4]=death p[6]= ammloss p[5] = e  //lets remove p6 for now
        real[] x_r, //data real
        int[] x_i // data integer
        )
        {
            real dydt[4];
            dydt[1] = -(y[2] * y[1]* p[1] / (p[2]+y[1])) + p[3]*p[4]*y[2] - p[6]*y[1];
            dydt[2] = (y[2] * y[1]* p[1] / (p[2]+y[1]))*p[5] - p[4]*y[2];
            return dydt;
        }
    real[] ifun(real t, real t0, real alpha, real beta, real omega, real gamma, real death1, real delta, real[] y0) { 
    real y_ini[2]; 
    real p[6];
    p[1] = alpha; 
    p[2] = beta; 
    p[3] = omega; 
    p[4] = gamma; 
    p[5] = death1; 
    p[6] = delta; 
    y_ini[1] = 40; 
    y_ini[2] = 15; 

    return y_ini;
  }

  real mufun(real t, real t0, real alpha, real beta, real omega, real gamma, real death1, real delta, real[] y0, real[] y_hat){
    return ((1 - delta) * y_hat[2] + delta * y_hat[1]);
  }

  real tmpfun(real t, real t0, real alpha, real beta, real omega, real gamma, real death1, real delta, real[] y0, real mu_hat) { 
    return mu_hat;
  }
} 
data { 
  int<lower=1> N;  // number of observations
  vector[N] Y;  // response variable
  int<lower=1> K_alpha;  // number of population-level effects
  matrix[N, K_alpha] X_alpha;  // population-level design matrix
  int<lower=1> K_beta;  // number of population-level effects
  matrix[N, K_beta] X_beta;  // population-level design matrix
  int<lower=1> K_omega;  // number of population-level effects
  matrix[N, K_omega] X_omega;  // population-level design matrix
  int<lower=1> K_gamma;  // number of population-level effects
  matrix[N, K_gamma] X_gamma;  // population-level design matrix
  int<lower=1> K_death1;  // number of population-level effects
  matrix[N, K_death1] X_death1;  // population-level design matrix
  int t0[N];
  // covariate vectors
  //vector[N] C_1;
  //vector[N] C_2;
  //vector[N] C_3;
  //int<lower=1> K_sigma;  // number of population-level effects
 // matrix[N, K_sigma] X_sigma;  // population-level design matrix
 //int which_t0[N]; 
  //int prior_only;  // should the likelihood be ignored? 
}     



transformed data {
  real x_r[0];
  int x_i[0];
}

parameters {
    real<lower=0> y0[4]; // init
    vector<lower=0>[2] sigma;
    real<lower=0> p[12];
}


transformed parameters {
    real y_hat[N, 2];
    y_hat = integrate_ode_rk45(myodes, y0, t0, t_obs, p, x_r, x_i);
}



model {
    sigma ~ normal(0, 1);
    p[1] ~ lognormal(0,0.5);
    p[2] ~ lognormal(0,0.5);
    p[3] ~ lognormal(0,0.5);
    p[4] ~ lognormal(0,0.5);
    p[5] ~ lognormal(0,0.5);
    p[6] ~ lognormal(0,0.5);
    y0[1] ~ normal(0, 10);
    y0[2] ~ normal(0, 10);
    for (t in 1:N)
        y[t] ~ normal(y_hat[t], sigma);
}





