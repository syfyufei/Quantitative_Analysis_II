
    data{
        int N; // number of observations
        int K; // number of response categories
        int J; // number of groups
        int D; // number of predictors
        int<lower=1, upper=K> y[N];  // the outcome
        row_vector[D] x[N]; // predictors
        int group[N];  // map observations to groups
    }
    parameters{
        ordered[K-1] kappa; // cut points
        vector[D] beta; //coefficients
        real zeta[J];
        real<lower=0,upper=10> sigma;
    }
    model{
        for(i in 1:N){
            y[i] ~ ordered_logistic(x[i]*beta + zeta[group[i]], kappa);
        }
        target += normal_lpdf(zeta|0, sigma);
        target += normal_lpdf(kappa|0,10);
        target += uniform_lpdf(sigma|0,10);
    }

