functions { 
  real[] gfun(real t, 
              real[] y, 
              real[] params, 
              real[] x_r, 
              int[] x_i) { 
    real dydt[2]; 

    dydt[1] = -y[2] * y[1] * params[1] * params[3]/(params[3] + y[1]); 
    dydt[2] = params[2] * y[2] * y[1] * params[3]/(params[3] + y[1]); 

    return dydt;
  }

  real[] ifun(real t, real t0, real alpha, real beta, real omega, real delta, real[] y0) { 
    real y_ini[2]; 
    real params[4];
    params[1] = alpha; 
    params[2] = beta; 
    params[3] = omega; 
    params[4] = delta; 
    y_ini[1] = 40; 
    y_ini[2] = 15; 

    return y_ini;
  }

  real[] simfun(real t, real t0, real alpha, real beta, real omega, real delta, real[] y0, real[] x_r, int[] x_i){
    real y_hat[1,2];
    real params[4];
    params[1] = alpha; 
    params[2] = beta; 
    params[3] = omega; 
    params[4] = delta; 
    y_hat = integrate_ode_rk45(gfun, y0, t0, rep_array(t, 1), params, x_r, x_i); 

    return y_hat[1,];
  }

  real mufun(real t, real t0, real alpha, real beta, real omega, real delta, real[] y0, real[] y_hat){
    return ((1 - delta) * y_hat[2] + delta * y_hat[1]);
  }

  real tmpfun(real t, real t0, real alpha, real beta, real omega, real delta, real[] y0, real mu_hat) { 
    return mu_hat;
  }
} 
data { 
  int<lower=1> N;  // total number of observations 
  vector[N] Y;  // response variable
  int<lower=1> K_alpha;  // number of population-level effects
  matrix[N, K_alpha] X_alpha;  // population-level design matrix
  int<lower=1> K_beta;  // number of population-level effects
  matrix[N, K_beta] X_beta;  // population-level design matrix
  int<lower=1> K_omega;  // number of population-level effects
  matrix[N, K_omega] X_omega;  // population-level design matrix
  // covariate vectors
  vector[N] C_1;
  vector[N] C_2;
  vector[N] C_3;
  int<lower=1> K_sigma;  // number of population-level effects
  matrix[N, K_sigma] X_sigma;  // population-level design matrix
  int which_t0[N]; 
  int prior_only;  // should the likelihood be ignored? 
} 
transformed data { 
    real x_r[0];
    int x_i[0];
} 
parameters { 
  // parameters are defined on unconstrained scales
  vector[K_alpha] b_alpha;  // population-level effects
  vector[K_beta] b_beta;  // population-level effects
  vector[K_omega] b_omega;  // population-level effects
  vector[K_sigma] b_sigma;  // population-level effects
} 
transformed parameters { 
  real y_hat[N,2];
  real y0[N,2];
  vector[N] nlp_alpha = X_alpha * b_alpha;
  vector[N] nlp_beta = X_beta * b_beta;
  vector[N] nlp_omega = X_omega * b_omega;
  vector[N] mu;
  vector[N] sigma = X_sigma * b_sigma;
  for (n in 1:N) { 
    sigma[n] = exp(sigma[n]);
    nlp_alpha[n] = exp(nlp_alpha[n]); 
    nlp_beta[n] = exp(nlp_beta[n]); 
    nlp_omega[n] = exp(nlp_omega[n]); 
    if (which_t0[n]==n) {
    // compute non-linear predictor
      y0[n,] = ifun(C_1[n] , C_2[n] , nlp_alpha[n] , nlp_beta[n] , nlp_omega[n] , C_3[n], rep_array(0.1, 0));
    } else {
      y0[n,] = y_hat[which_t0[n],];
    }
    // compute non-linear predictor
    y_hat[n,] = simfun(C_1[n] , C_2[n] , nlp_alpha[n] , nlp_beta[n] , nlp_omega[n] , C_3[n], y0[n,], x_r, x_i);
    // compute non-linear predictor
    mu[n] = mufun(C_1[n] , C_2[n] , nlp_alpha[n] , nlp_beta[n] , nlp_omega[n] , C_3[n], y0[n,], y_hat[n,]);
  } 
} 
model { 
  // priors including all constants 
  target += exponential_lpdf(exp(b_alpha) | 0.1)
    - 1 * exponential_lccdf(0 | 0.1);
  target += exponential_lpdf(exp(b_beta) | 0.1)
    - 1 * exponential_lccdf(0 | 0.1);
  target += exponential_lpdf(exp(b_omega) | 0.1)
    - 1 * exponential_lccdf(0 | 0.1);
  target += (b_alpha);
  target += (b_beta);
  target += (b_omega);
  // likelihood including all constants 
  if (!prior_only) { 
    target += normal_lpdf(Y | mu, sigma);
  } 
} 
generated quantities { 
}