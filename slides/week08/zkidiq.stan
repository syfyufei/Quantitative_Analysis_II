data {
  int<lower=0> N;
  vector<lower=0, upper=200>[N] kidScore;
  vector<lower=0, upper=200>[N] momIq;
  vector<lower=0, upper=1>[N] momHs;
}
transformed data {           
  vector[N] inter;
  vector[N] zMomHs;
  vector[N] zMomIq;
  zMomHs = (momHs - mean(momHs)) / (2*sd(momHs)); // standarization
  zMomIq = (momIq - mean(momIq)) / (2*sd(momIq));
  inter = zMomHs .* zMomIq;  
}
parameters {
  vector[4] beta;
  real<lower=0> sigma;
}
model {
  sigma ~ cauchy(0, 2.5);
  kidScore ~ normal(beta[1] + beta[2] * zMomHs + beta[3] * zMomIq + beta[4] * inter, sigma);
}
