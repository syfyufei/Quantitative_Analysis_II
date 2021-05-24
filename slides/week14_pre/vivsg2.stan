data {
  int<lower=0> N;
  int<lower=0> J;
  vector[N] y;
  vector[J] u;
  int<lower=0,upper=1> x[N];
  int<lower=1,upper=J> county[N];
}
parameters {
  real<lower=0> sigma;
  real gA0;
  real gA1;
  real gB0;
  real gB1;
  matrix[J,2] Beta;
  vector<lower=0>[2] sigmaB;
  cholesky_factor_corr[2] Lcorr;
}
transformed parameters {
  vector[N] yhat;
  matrix[J,2] mu;

  mu[,1] = gA0 + gA1 * u;
  mu[,2] = gB0 + gB1 * u;

  for (i in 1:N){
    yhat[i] = Beta[county[i],1] + Beta[county[i],2] * x[i];
  }
}
model {
  Lcorr ~ lkj_corr_cholesky(1);
  
  for(j in 1:J){
     Beta[j,1:2] ~ multi_normal_cholesky(mu[j,1:2], diag_pre_multiply(sigmaB, Lcorr));
  }
  

  gA1 ~ normal(0, 100);
  gA1 ~ normal(0, 100);
  gB0 ~ normal(0, 100);
  gB1 ~ normal(0, 100);

  sigma ~ cauchy(0,10);
  sigmaB ~ cauchy(0,10);

  y ~ normal(yhat, sigma);
}
generated quantities {
  corr_matrix[2] Omega;
  cov_matrix[2] S;
  Omega = multiply_lower_tri_self_transpose(Lcorr);
  S = quad_form_diag(Omega, sigmaB); 
}
