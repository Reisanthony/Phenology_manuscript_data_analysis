data{
	int<lower=1> N; /* # of observations */
	int<lower=1> K; /* # of covariates */
	int<lower=1> L; /* # of species */
	int<lower=0, upper=1> y[N]; /* PA data */
	int<lower=1, upper=L> ll[N]; /* species number */
	row_vector[K] X[N]; 
	
}

parameters{
	real alpha[L]; /* intercepts */
	real mu[K]; /* hierarchical means */
	real<lower=1> sigma[K]; /* hierarchical sds */
	vector[K] beta[L]; /* matrix of regression coefficients */
	
}

model{
	target += normal_lpdf(mu | 0, 20);
	target += gamma_lpdf(sigma| 2, 0.1);	
	for(i in 1:L){
		target += normal_lpdf(alpha[i] | 0, 1.5);
		target += normal_lpdf(beta[i] | mu, sigma);
	}	
	for(n in 1:N){
		target += bernoulli_logit_lpmf(y[n] | alpha[ll[n]] + X[n] * beta[ll[n]]);
	}
}
generated quantities {
	real<lower=0, upper=1> pp[N]; /* presence probability */
	vector[N] log_lik; /* log likelihood for each observation */
 	for(n in 1:N){
		pp[n] = inv_logit(alpha[ll[n]] + X[n] * beta[ll[n]]);
		log_lik[n] = bernoulli_logit_lpmf(y[n] | alpha[ll[n]] + X[n] * beta[ll[n]]);
	}
 	
}

