data {
  int<lower=0> N;  // Number of observations
  int<lower=1> K;  // Number of cities (3 in our data)
  int<lower=1, upper=K> city[N];  // city numeric (ID)
  vector[N] total;   // total income per purchase
}

parameters {
  vector[K] log_mu;   // total average price per city
  real<lower=0> sigma;   // standard deviation (must be positive)
}


model {
  target += normal_lpdf(log_mu[1] | 3.8, 1);  // Average purchase aroung 45$ (exp(3.8)).
  target += normal_lpdf(log_mu[2] | 3.8, 1);  // Average purchase aroung 45$ (exp(3.8)).
  target += normal_lpdf(log_mu[3] | 3.8, 1);  // Average purchase aroung 45$ (exp(3.8)).
  target += student_t_lpdf(sigma | 3, 0, 1);  // Nuisance parameter



  // Likelihood
  for (n in 1:N) {
    target += normal_lpdf(log(total[n]) | log_mu[city[n]], sigma);
  }
}

