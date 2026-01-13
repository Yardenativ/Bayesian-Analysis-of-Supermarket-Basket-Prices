data {
  int<lower=1> N;                        // number of observations
  array[N] int<lower=0, upper=1> male;   // 0 = Female, 1 = Male
  vector[N] log_total;                   // log(Total)
}
parameters {
  real alpha;                            // Female baseline (log scale)
  real beta_male;                        // Male – Female effect (log scale)
  real<lower=0> sigma;                   // residual SD (log scale)
}
model {
  // Priors
  target += normal_lpdf(alpha     | 3.8, 0.5);
  target += normal_lpdf(beta_male | 0,   0.5);
  target += normal_lpdf(sigma     | 0,   1);  

  // Likelihood
  for (n in 1:N) {
    target += normal_lpdf(log_total[n] | alpha + beta_male * male[n], sigma);
  }
}
