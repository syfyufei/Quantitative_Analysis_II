 data {
     /* Dimensions */
     int<lower=0> N;
     int<lower=0> K;
     /* Design Matrix */
     matrix[N,K] X;
     /* Outcome */
     int<lower=0> y[N];
 }

 parameters {
     vector[K] beta;
     real<lower=0> alpha;
 }

 transformed parameters {
     vector[N] lambda;
     real<lower=0> theta;

     lambda = exp(X * beta);
     theta = 1 / alpha;
 }

 model {
     /* Prior */
     beta ~ normal(0, 1);
     alpha ~ gamma(.01, .01);

     /* Likelihood */
     for (i in 1:N) {
         y[i] ~ neg_binomial_2(lambda[i], theta);
     }
 }

// generated quantities {
//     int y_new[N];
//     vector[N] log_lik;
//
//     for (i in 1:N) {
//         /* eta[i] is the log(mean). */
//         if (eta[i] > 15) {
//             /* To avoid erros like the below during the warmup. */
//             /* neg_binomial_2_rng: Random number that came from gamma
//distribution is 3.02668e+39, but must be less than 1.07374e+09 */
//             /* https://groups.google.com/forum/#!topic/stan-users/4g2hbwtRELQ
//*/
//             /* Check posterior predictive for anomaly. */
//             y_new[i] = -1;
//         } else {
//             y_new[i] = neg_binomial_2_rng(exp(eta[i]), phi);
//         }
//
//         log_lik[i] = neg_binomial_2_lpmf(y[i] | exp(eta[i]), phi);
//     }
// }
//
