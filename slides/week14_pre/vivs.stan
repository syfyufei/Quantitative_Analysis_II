data {
  int<lower=0> N;
  int<lower=0> J;
  vector[N] y;
  int<lower=0,upper=1> x[N];
  int county[N]; // in numeric order
}
parameters {
  real<lower=0> sigma;
  real<lower=0> sigmaA;
  real<lower=0> sigmaB;
  real muA;
  real muB;
  real<lower=-1,upper=1> rho;
  vector[2] Btemp;
}
model {
  vector[N] yhat;
  real a[J];
  real b[J];
  matrix[2,2] SigmaB;
  vector[2] Bhat;
  matrix[2,J] B;

  muA ~ normal(0, 100);
  muB ~ normal(0, 100);
  rho ~ uniform(-1, 1);

  SigmaB[1,1] = pow(sigmaA, 2);
  SigmaB[2,2] = pow(sigmaB, 2);
  SigmaB[1,2] = rho * sigmaA * sigmaB;
  SigmaB[2,1] = SigmaB[1,2];

  
  for (j in 1:J) {
    Bhat[1] = muA;
    Bhat[2] = muB; 
    Btemp ~ multi_normal(Bhat, SigmaB);
    B[1,j] = Btemp[1]; 
    B[2,j] = Btemp[2]; 
  }

  for (j in 1:J) {
    a[j] = B[1,j];
    b[j] = B[2,j];
  }

  for (i in 1:N){
    yhat[i] = a[county[i]] + b[county[i]] * x[i];
  }

  y ~ normal(yhat, sigma);
}
