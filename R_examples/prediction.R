## Predictions for hierarchical models
# max joseph dec 2014

# conditional P(Y_new | alpha_k): observed groups
# marginal P(Y_new|alpha_k+1) : unobserved groups

# pretty difficult to do well with lme or lme4
# (can't account for error in variance parameters)
# if you figure it out - awesome & please share

# Bayesian approach is easier because MCMC automatically
# produces marginal posterior distributions of all
# parameters including group means and variance components.
rm(list=ls())
setwd("R_examples")
library(scales)
library(lme4)
library(rstan)
library(doMC)
library(lattice)
library(modeest)
source("HDI.R")

K <- 10 # number of groups
mu_alpha <- 0 # population mean
sigma_alpha <- 8 # among group sd
sigma_Y <- 3 # residual sd
e_nmin <- .001 # exp(minimum group sample size)
e_nmax <- 3.5 # exp(maximum group sample size)
n_j <- floor(exp(seq(e_nmin, e_nmax, length.out=K))) 
# view distribution of sample sizes
id <- rep(1:K, times=n_j)
N <- length(id)

# simulate vals
alpha_j <- rnorm(K, mu_alpha, sigma_alpha)
Y <- rnorm(N, alpha_j[id], sigma_Y)

MLmod <- lmer(Y ~ 1 + (1|factor(id)))

# for bayesian hierarchical models (especially with low K)
# hierarchical variance term posterior distributions
# tend to be sensitive to prior distributions
# see Gelman 2006 for discussion and recommendations
# here we use a half-cauchy prior centered around zero
# with a dispersion/scale term
prior_sa <- 1 # half-cauchy scale term

# visualize prior distribution here
xprior <- seq(0, 20, length.out=500)
plot(x=xprior, 
     y=dcauchy(xprior, 0, prior_sa), 
     type="l", 
     ylim=c(0, .4))

# write model statement
cat("
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
    ", file="randint.stan")

data_vec <- c('N', 'Y', 'K', 'id', 'prior_sa')
params <- c('sigma_Y', 'sigma_alpha',
            'alpha', 'mu_alpha')

# Run an initial model
if (("mod_init" %in% ls()) == FALSE){
  mod_init <- stan('randint.stan', data=data_vec, chains=0)
}

# define function to run chains in parallel
estimate_params <- function(model, data, 
                            iter=1000, cores=3, 
                            chains.per=1, thin=1, 
                            warmup=iter/2){
  sflist <- mclapply(1:cores, mc.cores = cores,
                     function(i) stan(fit = model, data = data,
                                      chains = chains.per, chain_id = i,
                                      iter=iter, thin=thin,
                                      refresh = -1, 
                                      warmup=warmup))
  fit <- sflist2stanfit(sflist)
  return(fit)
}

# run parallel chains
mod_fit <- estimate_params(model=mod_init, data=data_vec, 
                           iter=2000, cores=2,
                           warmup=100)

# non-parallel chains 
#mod_fit <- stan(fit=mod_init, data=data_vec, 
#                iter=3000, chains=2)

# evaluate convergence
mod_fit
traceplot(mod_fit, "sigma_alpha")
post <- extract(mod_fit)


# plot the posterior with prior and likelihood
xmax <- sigma_alpha * 3
tpr <- profile(MLmod)
par(mai=c(1.2, 1.2, 1, 1), mfrow=c(1, 1))

plot(x=densityplot(tpr)$panel.args[[1]]$x, 
     y=densityplot(tpr)$panel.args[[1]]$y, 
     type="l", 
     xlim=c(0, xmax), 
     ylim=c(0, max(c(densityplot(tpr)$panel.args[[1]]$y, 
                     density(post$sigma_alpha)$y))), 
     col="green", 
     lwd=3, 
     xlab=expression(sigma[alpha]), 
     ylab="Probability density")

hist(post$sigma_alpha, add=TRUE, freq=FALSE, breaks=50, 
     col=alpha("black", .3))
prior_x <- seq(0, xmax, length.out=100)
prior_y <- dcauchy(prior_x, 0, prior_sa)
lines(x=prior_x, y=prior_y, col="blue")
abline(v=sigma_alpha, col="red", lty=2, lwd=3)
legend("topright", lty=c(1, 1, 2, 0), lwd=c(3, 1, 3, 4),
       pch=c(NA, NA, NA,15), pt.cex=c(1, 1, 1, 2),
       col=c("green", "blue", "red", alpha("black", .5)), 
       legend=c("Likelihood", "Prior", 
                "True value", "Posterior"), 
       bty="n")
title(expression(paste(sigma[alpha], " posterior density components")))
# change the half cauchy scale parameter for the
# prior on mu_alpha, see what happens

##-----------------------------------##
## Making predictions with our model ##
##-----------------------------------##
# store a bunch of values
# related to predictions
# and group means
nsims <- length(post[[1]])
alpha_post <- array(dim=c(nsims, K))
alpha_post_ci <- array(dim=c(2, K))
alpha_post_m <- rep(NA, K)
y_pred_c <- array(dim=c(nsims, K))
y_pred_ci <- array(dim=c(2, K))
y_pred_cm <- rep(NA, K)


for (i in 1:K){
  # conditional predictions (from observed groups)
  y_pred_c[, i] <- rnorm(nsims, 
                         mean=post$alpha[, i], 
                         sd=post$sigma_Y)
  # credible intervals for conditional predictions
  y_pred_ci[, i] <- HDI(y_pred_c[, i])
  # posterior mode for conditional predictions
  y_pred_cm[i] <- mlv(y_pred_c[, i], method="shorth")$M
  # posterior for alpha values (group means), known groups
  alpha_post[, i] <- post$alpha[, i]
  # credible intervals for group means
  alpha_post_ci[, i] <- HDI(alpha_post[, i])
  # modes for group means
  alpha_post_m[i] <- mlv(alpha_post[, i], method="shorth")$M
}

# Generate marginal predictions
alpha_pred <- rnorm(nsims, post$mu_alpha, sigma_alpha)
y_pred_m <- rnorm(nsims, mean=alpha_pred, sd=post$sigma_Y)
y_pred_mci <- HDI(y_pred_m)



# show posterior credible intervals for group means
plot(x=id + runif(length(id), -.2, .2),# added jitter
     y=Y, 
     xlab="group", 
     cex=1, xlim=c(1, K+1), 
     ylim=c(min(c(Y, alpha_j))*1.3, 
            max(c(Y, alpha_j))*1.5), 
     col=alpha("black", .3), 
     ylab="value")
points(x=c(1:K)-.2, y=alpha_post_m, 
       col=alpha("red", .5), cex=1.5, 
       pch=19)
segments(x0=c(1:K)-.2, x1=c(1:K)-.2, 
         y0=alpha_post_ci[1, ], 
         y1=alpha_post_ci[2, ], 
         lwd=3, col=alpha("red", .5))
points(x=c(1:K)+.2, alpha_j, 
       col=alpha("blue", .5), cex=1.5, pch=19)
title("Recovery of group means")
legend("topright", 
       legend=c("Observations", 
                expression(paste("P(", alpha[j], " | ", Y, ")")), 
                expression(paste("True ", alpha[j]))), 
       col=c(alpha("black", .3), alpha("red", .5), alpha("blue", .5)),
       pch=c(1, 19, 19), lty=c(0, 1, 0), pt.cex=c(1, 1.5, 1.5), 
       bty="n"
)

# show posterior prediction intervals
plot(x=id + runif(length(id), -.2, .2), # added jitter
     y=Y, 
     xlab="group", 
     cex=1, xlim=c(1, K+1), 
     ylim=c(min(c(Y, alpha_j))*2, 
            max(c(Y, alpha_j))*2), 
     col=alpha("black", .3), 
     ylab="value")
points(x=c(1:K)+.2, alpha_j, 
       col=alpha("blue", .5), cex=1.5, pch=19)
points(x=c(1:K)-.2, y=y_pred_cm, col=alpha("green", .5), 
       cex=2, pch=19)
segments(x0=c(1:K)-.2, x1=c(1:K)-.2,
         y0=y_pred_ci[1, ], y1=y_pred_ci[2, ], 
         col=alpha("green", .5), lwd=3)

# add marginal predictions
segments(x0=K+1, x1=K+1, 
         y0=y_pred_mci[1], y1=y_pred_mci[2], 
         col="purple")
points(x=K+1, y=mlv(y_pred_m, method="mfv")$M, 
       col=alpha("purple", .5), cex=2, pch=19)
title("Posterior prediction intervals")
par(mfrow=c(1, 1))

legend("topleft", 
       legend=c("Observations", 
                expression(paste("Predicted ", tilde(Y[j]), " for observed groups")), 
                expression(paste("True ", alpha[j])), 
                expression(paste("Predicted ", tilde(Y[j]), " for new group"))
       ), 
       col=c(alpha("black", .3), alpha("green", .5), 
             alpha("blue", .5), alpha("purple", .5)),
       pch=c(1, 19, 19, 19), lty=c(0, 1, 0, 1), 
       pt.cex=c(1, 1.5, 1.5, 1.5), 
       cex=.75,
       bty="n"
)
