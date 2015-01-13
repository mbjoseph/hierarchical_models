### Simulating Alot blood parasite dataset
J <- 25 # number of sites
n_j <- round(runif(J, 3, 20))
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
grand_mean <- 2
sigma_beta <- 3
mu_beta <- 3
beta_size <- rnorm(J, mean=mu_beta, sd=sigma_beta)
sigma_site <- 3
sigma_y <- 3
site_eff <- rnorm(J, mean=0, sd=sigma_site)
mu <- grand_mean + beta_size[site] * size + site_eff[site]
err <- rnorm(nrow(d), mean=0, sd=sigma_y)
d$log_parasites <- mu + err

