functions {
    real[] myodes(
        real t, //time
        real[] y,  //state y[1] = ammonium y[2] = chlorophyll
        real[] p, //parameters  // p[1]=a;  p[2] = k p[3] = l p[4]=death1  p[5] = f 
        real[] x_r, //data real
        int[] x_i // data integer
        )
        {
            real dydt[2];
            dydt[1] = -(y[2] * y[1]* p[1] / (p[2]+y[1])) + p[3]*p[4]*y[2]; //- .028*y[1]; 
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
    int<lower = 0, upper = 1> run_estimation; // a switch to evaluate the likelihood
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
    p[1] ~ lognormal(8, 0.5);
    p[2] ~ lognormal(7, 0.5);
    p[3] ~ lognormal(0, 0.5);
    p[4] ~ lognormal(-3.3, 0.5);
    p[5] ~ lognormal(-12, 0.5);
    y0[1] ~ normal(1000, 1);
    y0[2] ~ normal(50, 1);
   
    if(run_estimation==1){
    
    for (t in 1:N)
        y[t] ~ normal(y_hat[t], sigma);
        //print(y_hat);
        }
}


generated quantities{
  real y_pred[N,2];
  y_pred = integrate_ode_rk45(myodes, y0, t0, t_obs, p, x_r, x_i);
  
}



