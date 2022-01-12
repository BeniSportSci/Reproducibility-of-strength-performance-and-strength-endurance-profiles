data {
  int<lower=1> nSubjects;
  real<lower=0> Repetitions[nSubjects, 4, 2];
  real<lower=0> Load[nSubjects, 4, 2];
  
  vector<lower=0>[2] prior_sigma;
  vector<lower=0>[2] prior_tau_u1;
  vector<lower=0>[2] prior_tau_u2;
  vector<lower=0>[2] prior_tau_u3;
  vector<lower=0>[2] prior_tau_v1;
  vector<lower=0>[2] prior_tau_v2;
  vector<lower=0>[2] prior_tau_v3;
  
  vector[2] prior_alpha;
  vector[2] prior_beta;
  vector[2] prior_gamma;
  vector[2] prior_d_alpha;
  vector[2] prior_d_beta;
  vector[2] prior_d_gamma;
}


parameters {
  // group-level parameters
  real<lower=0> sigma;
  
  real<lower=0> alpha; //Lprime raw
  real<upper=0> beta; //k
  real gamma; //CL
  
  real d_alpha;
  real d_beta;
  real d_gamma;
  
  // cholesky factorization u
  matrix[3, nSubjects] z_u;
  cholesky_factor_corr[3] L_u;
  vector<lower=0>[3] tau_u;
  
  // cholesky factorization v
  matrix[3, nSubjects] z_v;
  cholesky_factor_corr[3] L_v;
  vector<lower=0>[3] tau_v;
}


transformed parameters {
  matrix[nSubjects, 3] u;
  matrix[nSubjects, 3] v;
  vector<lower=0>[nSubjects] a;
  vector<upper=0>[nSubjects] b;
  vector[nSubjects] c;
  vector[nSubjects] d_a;
  vector[nSubjects] d_b;
  vector[nSubjects] d_c;
  
  // matrix of correlated parameters (Cholesky factorization)
  u = (diag_pre_multiply(tau_u, L_u) * z_u)';
  v = (diag_pre_multiply(tau_v, L_v) * z_v)';
  
  // calculating absolute subject level parameters; 
  a = alpha + u[,1];
  b = beta + u[,2];
  c = gamma + u[,3];
  d_a = d_alpha + v[,1];
  d_b = d_beta + v[,2];
  d_c = d_gamma + v[,3];
} 


model {
  // group-level prior
  target += cauchy_lpdf(sigma | prior_sigma[1], prior_sigma[2]);
  target += normal_lpdf(alpha | prior_alpha[1], prior_alpha[2]);
  target += normal_lpdf(beta | prior_beta[1], prior_beta[2]);
  target += normal_lpdf(gamma | prior_gamma[1], prior_gamma[2]);
  target += cauchy_lpdf(d_alpha | prior_d_alpha[1], prior_d_alpha[2]);
  target += cauchy_lpdf(d_beta | prior_d_beta[1], prior_d_beta[2]);
  target += cauchy_lpdf(d_gamma | prior_d_gamma[1], prior_d_gamma[2]);
  
  // generate uncorrelated vectors for Cholesky factorization
  target += std_normal_lpdf(to_vector(z_u));
  target += std_normal_lpdf(to_vector(z_v));
  
  // prior for Cholesky factorization
  target += lkj_corr_cholesky_lpdf(L_u | 2);
  target += lkj_corr_cholesky_lpdf(L_v | 2);
  
  // priors for scaling of correlated parameters
  target += cauchy_lpdf(tau_u[1] | prior_tau_u1[1], prior_tau_u1[2]);
  target += cauchy_lpdf(tau_u[2] | prior_tau_u2[1], prior_tau_u2[2]);
  target += cauchy_lpdf(tau_u[3] | prior_tau_u3[1], prior_tau_u3[2]);
  target += cauchy_lpdf(tau_v[1] | prior_tau_v1[1], prior_tau_v1[2]);
  target += cauchy_lpdf(tau_v[2] | prior_tau_v2[1], prior_tau_v2[2]);
  target += cauchy_lpdf(tau_v[3] | prior_tau_v3[1], prior_tau_v3[2]);
  
  // likelihood
  for (s in 1:nSubjects) { // for each subject
    for (t in 1:4) { // for each load completed (70%, 80%, 90%, 100% 1-RM)
      for (z in 1:2) { // for each occasion (T1 or T2)
          target += normal_lpdf(Load[s, t, z] | (100 * (a[s] + (d_a[s] * (z-1)))) / (Repetitions[s, t, z] - (b[s] + (d_b[s] * (z-1)))) + (c[s] + (d_c[s] * (z-1))), sigma);
      }
    }
  }
}


generated quantities {
  matrix[3,3] tau_m;
  cov_matrix[3] Sigma;
  vector[3] sim;
  
  // random draw from group-level posterior
  tau_m = diag_matrix(tau_v);
  Sigma = tau_m * L_v * L_v' * tau_m;
  sim = multi_normal_rng([d_alpha, d_beta, d_gamma], Sigma);
}
