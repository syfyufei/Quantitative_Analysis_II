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
transformed parameters{
  real mu[N];
  
  for(i in 1:N){
    mu[i] = beta[1] + beta[2] * momHs[i] + beta[3] * momIq[i] + beta[4] * inter[i];
  }
}
model {
  sigma ~ cauchy(0, 2.5);
  kidScore ~ normal(mu, sigma);
}
generated quantities{
    real yhat[N];
    
    yhat = normal_rng(mu, sigma);
}
