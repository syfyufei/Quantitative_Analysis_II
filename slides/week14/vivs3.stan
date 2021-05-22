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
  cholesky_factor_corr[2] Lcorr;
}
model {
  vector[N] yhat;
    
  mu ~ normal(0, 100);
  sigma ~ cauchy(0,10);
  
  Lcorr ~ lkj_corr_cholesky(1);
  Beta ~ multi_normal_cholesky(mu, diag_pre_multiply(sigmaB, Lcorr));
  
  
  for (i in 1:N){
    yhat[i] = Beta[county[i]][1] + Beta[county[i]][2] * x[i];
  }

  y ~ normal(yhat, sigma);
}
generated quantities {
  corr_matrix[2] Omega;
  cov_matrix[2] S;
  Omega = multiply_lower_tri_self_transpose(Lcorr);
  S = quad_form_diag(Omega, sigmaB); 
}
