
data{
int<lower=0> N;
int FT[N];
int FTA[N];
real prior_sa;
}
parameters{
real <lower=0> sigma_alpha;
vector[N] alphaR;
real mu_alpha;
}
transformed parameters{
vector[N] alpha;
// Matt trick ups efficiency
alpha <- mu_alpha + sigma_alpha * alphaR;
}
model{
// priors
mu_alpha ~ normal(0, 10);
sigma_alpha ~ cauchy(0, prior_sa);
// parameter model
alphaR ~ normal(0, 1); // Matt trick
// likelihood
FT ~ binomial_logit(FTA, alpha);
}
