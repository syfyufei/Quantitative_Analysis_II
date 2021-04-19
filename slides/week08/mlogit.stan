data{
    int N; // number of observations
    int K; // number of response categories
    int D; // number of predictors
    int<lower=1, upper=K> y[N];  // the outcome
    matrix[N,D] x; // predictors
}
transformed data{
    row_vector[D] zeros = rep_row_vector(0,D);
}
parameters{
    matrix[K-1,D] betaRaw;
}
transformed parameters{
    matrix[K,D] beta;
    beta = append_row(zeros, betaRaw);
}
model{
    matrix[N,K] xbeta = x * beta';
    
    target += normal_lpdf(to_vector(beta)|0,10);
    to_vector(beta) ~ normal(0,10);
//    for(k in 1:K){
//        beta[k,] ~ normal(0,10);
//    }
    
    for(i in 1:N){
        y[i] ~ categorical_logit(xbeta[i]');
    }
}
