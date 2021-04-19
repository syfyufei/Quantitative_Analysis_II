data {
  int<lower=0> N;
  vector[2] yt[N];
  vector[N] z;
}
parameters {
  real b;
  real d;
  real a;
  real g;
  real<lower=-1,upper=1> rho_yt;
  real<lower=0,upper=100> sigma_t;
  real<lower=0,upper=100> sigma_y;
}
transformed parameters{
  matrix[2,2] Sigma_yt;
  vector[2] yt_hat[N];

  for (i in 1:N) {
    yt_hat[i,2] = g + d * z[i];
    yt_hat[i,1] = a + b * yt[i,2];
  }

  //data level
  Sigma_yt[1,1] = pow(sigma_y,2);
  Sigma_yt[2,2] = pow(sigma_t,2);
  Sigma_yt[1,2] = rho_yt*sigma_y*sigma_t;
  Sigma_yt[2,1] = Sigma_yt[1,2];
}
model {
  
  //data level
  sigma_y ~ uniform (0, 100);
  sigma_t ~ uniform (0, 100);
  rho_yt ~ uniform(-1, 1);
  d ~ normal (0, 100);
  b ~ normal (0, 100);

  //data model
  for (i in 1:N)
    yt[i] ~ multi_normal(yt_hat[i],Sigma_yt);

}
