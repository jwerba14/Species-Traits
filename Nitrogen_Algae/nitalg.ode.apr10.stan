functions {
    real[] myodes(
        real t, //time
        real[] y,  //state y[1] = ammonium y[2] = chlorophyll
        real[] p, //parameters  // p[1]=alpha;  p[2] = kappa p[3] = r p[4]=death p[6]= ammloss p[5] = e  //lets remove p6 for now
        real[] x_r, //data real
        int[] x_i // data integer
        )
        {
            real dydt[2];
            dydt[1] = -(y[2] * y[1]* p[1] / (p[2]+y[1])) + p[3]*p[4]*y[2] - .028*y[1];
            dydt[2] = (y[2] * y[1]* p[1] / (p[2]+y[1]))*p[5] - p[4]*y[2];
            return dydt;
        }
}

data {
   int<lower=1> N;
    int<lower=1> T;
    real y[N, 2];
    real t0;
    real t_obs[N];
}

transformed data {
  real x_r[0];
  int x_i[0];
}

parameters {
    real<lower=0> y0[2]; // init
    vector<lower=0>[2] sigma;
    real<lower=0> p[5];
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
    y0[1] ~ normal(0, 10);
    y0[2] ~ normal(0, 10);
    for (t in 1:N)
        y[t] ~ normal(y_hat[t], sigma);
}


//generated quantities {
    //real y_hat_n[T, 2];
   // real y_hat_sigma[T, 2];
//y_hat_n = integrate_ode_rk45(myodes, y0, t0, t_sim, p, x_r, x_i);
    // Add error with estimated sigma
    //for (i in 1:T) {
       // y_hat_sigma[i, 1] = y_hat_n[i, 1] + normal_rng(0, sigma[1]);
       // y_hat_sigma[i, 2] = y_hat_n[i, 2] + normal_rng(0, sigma[2]);
      //  }
//}



