data{
    int<lower=0> N;
    int<lower=0,upper=1> y[N];
    vector[N] dist;
}
transformed data{
    vector[N] dist100;
    dist100 = dist/100.0;
}
parameters{
    vector[2] beta;
}
model{
    y ~ bernoulli_logit(beta[1] + beta[2]*dist100);
}

