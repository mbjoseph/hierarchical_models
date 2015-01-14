### Simulating Alot blood parasite dataset
J <- 25 # number of sites
n_j <- round(runif(J, 2, 20))
individual <- NULL
site <- NULL
size <- NULL
for (i in 1:length(n_j)){
  individual <- c(individual, 1:n_j[i])
  site <- c(site, rep(i, n_j[i]))
  size <- c(size, rexp(n_j[i], 1))
}
d <- data.frame(individual=individual, 
                site=paste("site", LETTERS[site]), 
                body_size=size)

# set up model parameters & simulate response
mu_alpha <- 2
sigma_beta <- 3
mu_beta <- 3

sigma_site <- 3
sigma_y <- 3
Sigma_sim <- matrix(c(sigma_site^2, 0, 
                    0, sigma_beta^2), 
                    nrow=2)
library(MASS)
effs <- mvrnorm(J, mu=c(mu_alpha, mu_beta), 
                Sigma = Sigma_sim)
site_eff <- effs[, 1]
beta_size <- effs[, 2]

#plot(site_eff, beta_size, xlim=c(-10, 10), ylim=c(-10, 10))

mu <- beta_size[site] * size + site_eff[site]
err <- rnorm(nrow(d), mean=0, sd=sigma_y)
d$log_parasites <- mu + err

