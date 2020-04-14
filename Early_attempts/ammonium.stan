// generated with brms 2.9.0
functions {
}
data {
  int<lower=1> N;  // number of observations
  vector[N] Y;  // response variable
  vector<lower=0>[N] weights;  // model weights
  // data for group-level effects of ID 1
  int<lower=1> N_1; // number of reps
  int<lower=1> M_1; // only 1 because only one random effect
  int<lower=1> J_1[N]; // rep id index
  vector[N] Z_1_1;  
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  real temp_Intercept;  // temporary intercept
  real<lower=0> sigma;  // residual SD
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations- variation among rep (random effect)
  vector[N_1] z_1[M_1];  // unscaled group-level effects
}
transformed parameters {
  // group-level effects
  vector[N_1] r_1_1 = (sd_1[1] * (z_1[1]));  // r_1_1 are conditional modes -- causes random effects to be scaled standard normal
}
model {
  vector[N] mu = temp_Intercept + rep_vector(0, N);
  for (n in 1:N) {
    mu[n] += r_1_1[J_1[n]] * Z_1_1[n];  // alpha[ttt[i]] but left separate- mu = alpha from my model and z_1_1 is scaling it back ?
  }
  // priors including all constants
  target += student_t_lpdf(temp_Intercept | 3, 1, 10);
  target += student_t_lpdf(sigma | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  target += student_t_lpdf(sd_1 | 3, 0, 10)
    - 1 * student_t_lccdf(0 | 3, 0, 10);
  target += normal_lpdf(z_1[1] | 0, 1);
  // likelihood including all constants
  if (!prior_only) {
    for (n in 1:N) {
      target += weights[n] * normal_lpdf(Y[n] | mu[n], sigma); // directly weight likelihood
    }
  }
}
generated quantities {
  // actual population-level intercept
  real b_Intercept = temp_Intercept;
}

