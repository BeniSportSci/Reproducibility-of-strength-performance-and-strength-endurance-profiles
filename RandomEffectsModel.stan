data {
  int<lower=0> nSubjects;
  vector<lower=0>[2] prior_mu;
  vector[2] prior_t;
  vector<lower=0>[2] prior_sigma_e;
  vector<lower=0>[2] prior_sigma_s;
  matrix<lower=0>[nSubjects, 2] P;
}


parameters {
  real<lower=0> mu;
  vector[nSubjects] u_raw;
  real time;
  real<lower=0> sd_u;
  real<lower=0> sigma;
}


transformed parameters {
  // non-centered reparameterization of u
  vector[nSubjects] u;
  u = sd_u * u_raw;
}


model {
  //priors
  target += normal_lpdf(mu | prior_mu[1], prior_mu[2]);
  target += cauchy_lpdf(sd_u | prior_sigma_s[1], prior_sigma_s[2]);
  target += normal_lpdf(u_raw | 0, 1);
  target += normal_lpdf(time | prior_t[1], prior_t[2]);
  target += cauchy_lpdf(sigma | prior_sigma_e[1], prior_sigma_e[2]);
  //model
  for (s in 1: nSubjects){
    for (t in 1:2){
      target += normal_lpdf(P[s, t] | ((mu + u[s]) + time * (t-1)), sigma);
    }
  }
}


generated quantities {
  // calculate ICC
  real<lower=-1, upper=1> ICC = square(sd_u) / (square(sd_u) + square(sigma));
}
