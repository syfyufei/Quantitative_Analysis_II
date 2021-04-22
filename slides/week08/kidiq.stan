data {
  int<lower=0> N;
  vector<lower=0, upper=200>[N] kidScore;
  vector<lower=0, upper=200>[N] momIq;
  vector<lower=0, upper=1>[N] momHs;
}
transformed data {           // interaction
  vector[N] inter;
  inter = momHs .* momIq;    // .* matrix multiplication
}
parameters {
  vector[4] beta;
  real<lower=0> sigma;
}
model {
  sigma ~ cauchy(0, 2.5);
  //kidScore ~ normal(beta[1] + beta[2] * momHs + beta[3] * momIq + beta[4] * inter, sigma);
  target += normal_lpdf(kidScore| beta[1] + beta[2] * momHs + beta[3] * momIq + beta[4] * inter, sigma);
}
// 如何提高速度 把model的东西扔到trans