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
  real<lower=0> sigmaA;
  real<lower=0> sigmaB;
  real gA0;
  real gA1;
  real gB0;
  real gB1;
  vector[J] a;
  vector[J] b;
}
transformed parameters {
  vector[N] yhat;
  vector[J] ahat;
  vector[J] bhat;
  
  ahat = gA0 + gA1 * u;
  bhat = gB0 + gB1 * u;
  
  for (i in 1:N){
    yhat[i] = a[county[i]] + b[county[i]] * x[i];
  }
}
model {

  a ~ normal(ahat, sigmaA);
  b ~ normal(bhat, sigmaB);

  gA1 ~ normal(0, 100);
  gA1 ~ normal(0, 100);
  gB0 ~ normal(0, 100);
  gB1 ~ normal(0, 100);

  sigma ~ cauchy(0,2.5);
  sigmaA ~ cauchy(0,2.5);
  sigmaB ~ cauchy(0,2.5);

  y ~ normal(yhat, sigma);
}
