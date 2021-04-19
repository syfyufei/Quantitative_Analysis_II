data{
    int<lower=0> N;
    int<lower=0> K;
    row_vector[K] X[N];
    int<lower=0> y[N];
}
parameters{
    vector[K] beta;
    real<lower=0> tau;
    vector[N] lambda;
}
transformed parameters{
    real<lower=0> sigma;
    
    sigma = 1.0/sqrt(tau);
    
}
model{
    tau ~ gamma(0.001,0.001);
    for(i in 1:N){
        lambda[i] ~ normal(0, sigma);
        y[i] ~ poisson_log(lambda[i] + X[i]*beta); 
    }
}
