data {
  // Dimensions
  int<lower=1> N;  // total number of observations
  array[N] int Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int<lower=1> Kc;  // number of population-level effects after centering
  // data for group-level effects of ID 1 (ego)
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  array[N] int<lower=1> J_1;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_1;
  // data for group-level effects of ID 2 (alter)
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  array[N] int<lower=1> J_2;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_1;
  int prior_only;  // should the likelihood be ignored?
}

parameters {
  vector[Kc] b;  // population-level effects
  vector[M_1] r_1_1;  // ego-level effects
  vector[M_2] r_2_1;  // alter-level effects
  real<lower=0> sigma_1;  // SD of ego random effects
  real<lower=0> sigma_2;  // SD of alter random effects
  real Intercept;  // temporary intercept for centered predictors
}

transformed parameters {
  vector[N_1] r_1_1_star;  // identifiable ego random effects
  vector[N_2] r_2_1_star;  // identifiable alter random effects
  real Intercept_star;  // identifiable intercept
  real ave_r_1 = mean(r_1_1); # means of the random effects
  real ave_r_2 = mean(r_2_1);
  
  for (n in 1:N_1) {
    r_1_1_star[n] = r_1_1[n] - ave_r_1; # centered random effects
  }
  for (n in 1:N_2) {
    r_2_1_star[n] = r_2_1[n] - ave_r_2;
  }
  Intercept_star = Intercept + ave_r_1 + ave_r_2;
}

model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = Intercept + X * b;
    for (n in 1:N) {
      // add more terms to the linear predictor
      mu[n] += r_1_1[J_1[n]] * Z_1_1[n] + r_2_1[J_2[n]] * Z_2_1[n];
    }
    Y ~ bernoulli_logit(mu);
  }
  // priors including constants
  b ~ normal(0, 2.5);
  r_1_1 ~ normal(0, sigma_1);
  r_2_1 ~ normal(0, sigma_2);
  sigma_1 ~ student_t(3, 0, 2.5);
  sigma_2 ~ student_t(3, 0, 2.5);
  Intercept ~ student_t(3, 0, 2.5);
}

generated quantities {
  // actual population-level intercept
  real b_Intercept = Intercept_star;
  // identifiable random effects
  vector[N_1] r_1_1_identify = r_1_1_star * sigma_1;
  vector[N_2] r_2_1_identify = r_2_1_star * sigma_2;
}

