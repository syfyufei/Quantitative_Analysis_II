data {
  int<lower=0> N;
  int<lower=0> J;
  vector[N] y;
  int<lower=0,upper=1> x[N];
  int county[N]; // in numeric order
}
parameters {
  vector[2] mu;
  real<lower=0> sigma;
  vector<lower=0>[2] sigmaB;
  vector[2] Beta[J];
  corr_matrix[2] Omega;
}
transformed parameters{
  cov_matrix[2] S;  
  S = quad_form_diag(Omega, sigmaB);
  
}
model {
  vector[N] yhat;

  mu ~ normal(0, 100);
  sigma ~ cauchy(0,10);
  Omega ~ lkj_corr(2);
  Beta ~ multi_normal(mu, S);
  
  
  for (i in 1:N){
    yhat[i] = Beta[county[i]][1] + Beta[county[i]][2] * x[i];
  }

  y ~ normal(yhat, sigma);
}
