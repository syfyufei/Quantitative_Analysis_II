data{
    int<lower=0> N;
    int<lower=0> K;
    row_vector[K] X[N];
    real offset[N];
    int<lower=0> y[N];
}
parameters{
    vector[K] beta;
}
transformed parameters{
    real lp[N];
    real<lower=0> lambda[N];
    
    lambda = exp(X*beta + offset);    
}
model{
    y ~ poisson(mu); 
}
