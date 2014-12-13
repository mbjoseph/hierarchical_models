
data{
int N;
real Y[N];
int K;
int id[N];
real prior_sa;
}
parameters{
real <lower=0> sigma_Y;
real <lower=0> sigma_alpha;
real alpha[K];
real mu_alpha;
}
model{
# priors
mu_alpha ~ normal(0, 100);
sigma_Y ~ uniform(0, 100);
sigma_alpha ~ cauchy(0, prior_sa);
alpha ~ normal(mu_alpha, sigma_alpha);
for (i in 1:N){
Y[i] ~ normal(alpha[id[i]], sigma_Y);
}
}
