data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> N_1;  // number of grouping levels for ego (equivalent to Nplots)
  int<lower=1> N_2;  // number of grouping levels for alter (equivalent to Ndates)
  array[N] int<lower=1> J_1;  // ego indicator per observation (equivalent to plot)
  array[N] int<lower=1> J_2;  // alter indicator per observation (equivalent to date)
  vector[N] Z_1_1;  // group-level predictor for ego
  vector[N] Z_2_1;  // group-level predictor for alter
  int prior_only;  // should the likelihood be ignored?
}

parameters {
  vector[K] b;  // population-level effects
  real Intercept;  // temporary intercept
  vector[N_1] r_1;  // ego-level effects (equivalent to eps)
  vector[N_2] r_2;  // alter-level effects (equivalent to gamma)
  real<lower=0> sigma;  // residual SD
  real<lower=0> sigma_1;  // SD of ego random effects
  real<lower=0> sigma_2;  // SD of alter random effects
}

transformed parameters {
  vector[N_1] r_1_star;  // identifiable ego random effects
  vector[N_2] r_2_star;  // identifiable alter random effects
  real Intercept_star;  // identifiable intercept
  real ave_r_1 = mean(r_1);
  real ave_r_2 = mean(r_2);
  
  for (n in 1:N_1) {
    r_1_star[n] = r_1[n] - ave_r_1;
  }
  for (n in 1:N_2) {
    r_2_star[n] = r_2[n] - ave_r_2;
  }
  Intercept_star = Intercept + ave_r_2 + ave_r_1;
}

model {
  // likelihood including constants
  if (!prior_only) {
    vector[N] mu = Intercept + X * b;
    for (n in 1:N) {
      mu[n] += r_1[J_1[n]] * Z_1_1[n] + r_2[J_2[n]] * Z_2_1[n];
    }
    Y ~ normal(mu, sigma);
  }
  
  // priors including constants
  b ~ normal(0, 2.5);
  Intercept ~ student_t(3, 0, 2.5);
  r_1 ~ normal(0, sigma_1);
  r_2 ~ normal(0, sigma_2);
  sigma ~ student_t(3, 0, 2.5);
  sigma_1 ~ student_t(3, 0, 2.5);
  sigma_2 ~ student_t(3, 0, 2.5);
}

generated quantities {
  vector[N] log_lik;
  vector[N] y_rep;
  
  {
    vector[N] mu = Intercept + X * b;
    for (n in 1:N) {
      mu[n] += r_1[J_1[n]] * Z_1_1[n] + r_2[J_2[n]] * Z_2_1[n];
    }
    
    for (n in 1:N) {
      log_lik[n] = normal_lpdf(Y[n] | mu[n], sigma);
      y_rep[n] = normal_rng(mu[n], sigma);
    }
  }
} 
