### Simulating Alot blood parasite dataset
J <- 12 # number of sites
n_j <- round(runif(J, 3, 15))
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

# set up model parameters
grand_mean <- 2
beta_size <- -1
sigma_site <- 1
sigma_y <- 1
site_eff <- rnorm(J, mean=0, sd=sigma_site)
mu <- grand_mean + beta_size * size + site_eff[site]
err <- rnorm(nrow(d), mean=0, sd=sigma_y)
d$log_parasites <- mu + err

write.csv(d, file="R_examples/alot/alot.csv")

# exploratory plots
ggplot(d, aes(y=log_parasites, x=body_size)) + 
  geom_point(pch=1) + 
  facet_wrap(~site) + 
  theme_bw() + 
  xlab("Body size (tons)") + 
  ylab("Blood parasite intensity")

