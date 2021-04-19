data {
  int<lower=0> N;
  vector<lower=0, upper=200>[N] kidScore;
  int momWork[N];
}
transformed data {           // interaction
  vector[N] work2;
  vector[N] work3;
  vector[N] work4;
  for(i in 1:N){
    work2[i] = momWork[i] == 2;
    work3[i] = momWork[i] == 3;
    work4[i] = momWork[i] == 4;
  }

}
parameters {
  vector[4] beta;
  real<lower=0> sigma;
}
model {
  sigma ~ cauchy(0, 2.5);
  kidScore ~ normal(beta[1] + beta[2] * work2 + beta[3] * work3 + beta[4] * work4, sigma);
}
