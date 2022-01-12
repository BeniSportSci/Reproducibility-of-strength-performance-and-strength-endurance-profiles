data {
  int<lower=1> nSubjects;
  real<lower=0> Repetitions[nSubjects, 4, 2];
  real<lower=0> Load[nSubjects, 4, 2];
  
  vector<lower=0>[2] prior_sigma;
  vector<lower=0>[2] prior_tau_u1;
  vector<lower=0>[2] prior_tau_u2;
  vector<lower=0>[2] prior_tau_v1;
  vector<lower=0>[2] prior_tau_v2;
  
  vector[2] prior_alpha;
  vector[2] prior_beta;
  vector[2] prior_d_alpha;
  vector[2] prior_d_beta;
}


parameters {
  // group-level parameters
  real<lower=0> sigma;
  
  real alpha;
  real beta;
  
  real d_alpha;
  real d_beta;
  
  // cholesky factorization
  matrix[2, nSubjects] z_u;
  cholesky_factor_corr[2] L_u;
  vector<lower=0>[2] tau_u;
  
  matrix[2, nSubjects] z_v;
  cholesky_factor_corr[2] L_v;
  vector<lower=0>[2] tau_v;
}


transformed parameters {
  // matrix of correlated parameters (Cholesky factorization)
  matrix[nSubjects, 2] u = (diag_pre_multiply(tau_u, L_u) * z_u)';
  matrix[nSubjects, 2] v = (diag_pre_multiply(tau_v, L_v) * z_v)';
  
  // calculating absolute subject level parameters; 
  vector[nSubjects] a = alpha + u[,1];
  vector[nSubjects] b = beta + u[,2];
  vector[nSubjects] d_a = d_alpha + v[,1];
  vector[nSubjects] d_b = d_beta + v[,2];

} 


model {
  // group-level priors
  target += cauchy_lpdf(sigma | prior_sigma[1], prior_sigma[2]);
  target += normal_lpdf(alpha | prior_alpha[1], prior_alpha[2]);
  target += normal_lpdf(beta | prior_beta[1], prior_beta[2]);
  target += normal_lpdf(d_alpha | prior_d_alpha[1], prior_d_alpha[2]);
  target += normal_lpdf(d_beta | prior_d_beta[1], prior_d_beta[2]);
  
  // generate uncorrelated vectors for Cholesky factorization
  target += std_normal_lpdf(to_vector(z_u));
  target += std_normal_lpdf(to_vector(z_v));
  
  // prior for Cholesky factorization
  target += lkj_corr_cholesky_lpdf(L_u | 2);
  target += lkj_corr_cholesky_lpdf(L_v | 2);
  
  // priors for scaling of correlated parameters
  target += cauchy_lpdf(tau_u[1] | prior_tau_u1[1], prior_tau_u1[2]);
  target += cauchy_lpdf(tau_u[2] | prior_tau_u2[1], prior_tau_u2[2]);
  target += cauchy_lpdf(tau_v[1] | prior_tau_v1[1], prior_tau_v1[2]);
  target += cauchy_lpdf(tau_v[2] | prior_tau_v2[1], prior_tau_v2[2]);
  
  // likelihood
  for (s in 1:nSubjects) { // for each subject
    for (t in 1:4) { // for each load completed (70%, 80%, 90%, 100% 1-RM)
      for (z in 1:2) { // for each occasion (T1 or T2)
          target += normal_lpdf(Load[s, t, z] | (a[s] + (d_a[s] * (z-1))) + (b[s] + (d_b[s] * (z-1))) * Repetitions[s, t, z], sigma);
      }
    }
  }
}


generated quantities {
  matrix[2,2] tau_m;
  cov_matrix[2] Sigma;
  vector[2] sim;
  
  // random draw from group-level posterior
  tau_m = diag_matrix(tau_v);
  Sigma = tau_m * L_v * L_v' * tau_m;
  sim = multi_normal_rng([d_alpha, d_beta], Sigma);
}
