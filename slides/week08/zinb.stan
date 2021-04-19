 data {
     /* Dimensions */
     int<lower=0> N;
     int<lower=0> K;
     /* Design Matrix */
     matrix[N,K] X;
     /* Outcome */
     int<lower=0> y[N];
     /* Hyperparameters*/
//     real<lower=0> s[K];
//     real<lower=0> s_theta[K];
 }

 parameters {
     vector[K] beta;
     vector[K] beta_theta;
     real<lower=0> alpha;
 }

 transformed parameters {
     vector[N] eta;
     vector[N] eta_theta;
     real<lower=0> theta;

     eta = X * beta;
     eta_theta = X * beta_theta;
     theta = 1/alpha;
 }  

 model {
     /* Prior */
    beta ~ normal(0, 1);
    beta_theta ~ normal(0, 1);
    alpha ~ gamma(.01, .01);

     /* Likelihood */
      for (i in 1:N) {
         if (y[i] == 0) {
             /* Zero case */
             target += log_sum_exp(
                bernoulli_logit_lpmf(1 | eta_theta[i]),              /* Structural zero */
                bernoulli_logit_lpmf(0 | eta_theta[i]) + neg_binomial_2_lpmf(0 | eta[i], theta)); /* NB zero */
         } else {
             /* Non-zero case */
             /* First term means not structural zero. */
             target += bernoulli_logit_lpmf(0 | eta_theta[i]) +
                       neg_binomial_2_lpmf(y[i] | eta[i], theta);             /* y[i] is relevant only here. */
         }
     }
 }
generated quantities {
     int y_new[N];
     vector[N] log_lik;

     for (i in 1:N) {

         if (bernoulli_logit_rng(eta_theta[i]) == 1) {
             /* Structural zero */
             y_new[i] = 0;
         } else {
             /* Not structural zero */
             if (eta[i] > 15) {
                 /* To avoid erros like the below during the warmup. */
                 /* Check posterior predictive. */
                 y_new[i] = -1;
             } else {
                 y_new[i] = neg_binomial_2_rng(eta[i], theta);
             }
         }


         if (y[i] == 0) {
             /* Zero case */
             log_lik[i] = log_sum_exp(/* Structural zero */
                                      bernoulli_logit_lpmf(1 | eta_theta[i]),
                                      /* NB zero */
                                      bernoulli_logit_lpmf(0 | eta_theta[i]) +
                                      neg_binomial_2_lpmf(0 | eta[i], theta));
         } else {
             /* Non-zero case */
             /* First term means not structural zero. */
             log_lik[i] = bernoulli_logit_lpmf(0 | eta_theta[i]) +
                 /* y[i] is relevant only here. */
                 neg_binomial_2_lpmf(y[i] | eta[i], theta);
         }

     }
}

