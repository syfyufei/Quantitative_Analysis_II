data{
    int N; // number of observations
    int K; // number of response categories
    int D; // number of predictors
    int<lower=1, upper=K> y[N];  // the outcome
    row_vector[D] x[N]; // predictors
}
parameters{
    ordered[K-1] kappa; // cut points
    vector[D] beta; //coefficients
}
model{
    for(n in 1:N){
        y[n] ~ ordered_logistic(x[n]*beta, kappa);
    }
}

