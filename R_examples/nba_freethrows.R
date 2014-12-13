# Figure out who was the best NBA 
# freethrow shooter in a season
# max joseph Dec 2014
rm(list=ls())
setwd("R_examples")
library(rstan)
library(modeest)
library(scales)
library(wordcloud) # don't worry, I promise: no wordclouds
library(plotrix)
library(doMC)
op <- par(no.readonly = TRUE)

# choose your favorite nba season
#d <- read.csv("nba_data/nba_86-87.csv")
d <- read.csv("nba_data/nba_91-92.csv")
#d <- read.csv("nba_data/nba_92-93.csv")
#d <- read.csv("nba_data/nba_09-10.csv")
#d <- read.csv("nba_data/nba_13-14.csv")
# data from http://www.basketball-reference.com

str(d)
d <- d[d$Tm!="TOT", ] # remove player totals, leaving raw data

# some players played on multiple teams over this season
# so let's add their values together, and structure the
# data so that each player gets one row
library(reshape2)
ftd <- aggregate(cbind(FTA, FT) ~ Player, d, sum)
ftd$percent <- ftd$FT/ftd$FTA
# sort the data based on shooting percentage
ftd <- ftd[with(ftd, order(-percent)), ]
ftd <- ftd[!is.na(ftd$percent), ]

# note extremely unequal sample sizes (free throw attempts)
hist(ftd$FTA, breaks=20)



# plot empirical free throw shooting averages
par(mfrow=c(1, 2))
plot(ftd$percent, 
     ylab="Proportion of free throws made")
# any evidence that extreme values are more likely for
# players who took fewer shots?
plot(ftd$FTA, ftd$percent,
     xlab="Free throw attempts", 
     ylab="Proportion of free throws made")
par(mfrow=c(1,1))
# yep, looks like we could use some shrinkage
# Goal is to estimate:
# - true free throw shooting % for each player
# - probability that a player was the best FT shooter
#   (a derived parameter)

N <- nrow(ftd)

# scale parameter for half-cauchy prior on 
# varying intercept (among-player) sigma
prior_sa <- 1 

# write model statement
# basically just a varying intercept model
# where the logit probability of making a FT is 
# normally distributed around a league mean (mu_alpha)
# and there is some variation among players (sigma_alpha)

cat("
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
    ", file="randint2.stan")

stan_d <- list(N=N, FT=ftd$FT, FTA=ftd$FTA, prior_sa=1)

params <- c('sigma_alpha',
            'alpha', 'mu_alpha')

# initialize model if necessary
if (("mod_init" %in% ls()) == FALSE){
  mod_init <- stan('randint2.stan', data=stan_d, chains=0)
}

# estimate parameters
mod_fit <- stan(fit=mod_init, data=stan_d,
                warmup=100, pars=params,
                iter=1000, chains=3)

# check convergence
traceplot(mod_fit, "sigma_alpha", inc_warmup=FALSE)
traceplot(mod_fit, "mu_alpha", inc_warmup=FALSE)
par(op)

# look @ Rhat
mod_fit

# put all posterior simulations in a list
post <- extract(mod_fit)
nsims <- length(post[[1]])

# visualize the posterior of the league mean, 
# and the player's posterior probabilities of making FT
# logit-scale (what we actually modeled)
plot(density(post$mu_alpha), 
     main="Logit-scale", 
     xlim=range(post$alpha), 
     lwd=5, 
     xlab="Logit-probability of making freethrow", 
     ylab="Posterior density")
for (i in 1:ncol(post$alpha)){
  lines(density(post$alpha[, i]), 
        col=alpha("black", .3))
}

# probability scale via antilogit (plogis)
plot(density(plogis(post$mu_alpha)), 
     main="Probability scale", 
     xlim=range(plogis(post$alpha)), 
     lwd=5, 
     xlab="Probability of making freethrow", 
     ylab="Posterior density")
for (i in 1:ncol(post$alpha)){
  lines(density(plogis(post$alpha[, i])), 
        col=alpha("black", .3))
}



# for fun, plot Pr(make freethrow | y), random players
players <- sample(ftd$Player, size=6)
id <- rep(NA, length(players))
par(mfrow=c(1, 1))
plot(x=NULL, y=NULL, ylim=c(0, 50), 
     xlim=c(.2, 1), xlab="Pr(make freethrow)", 
     ylab="Posterior density", 
)

labx <- rep(NA, length(id))
laby <- rep(NA, length(id))
for (i in 1:length(id)){
  id[i] <- which(ftd$Player == players[i])
  vals <- plogis(post$alpha[, id[i]])
  den <- density(vals)
  lines(den)
  labx[i] <- median(vals)
  laby[i] <- max(den$y)
}

textplot(labx, laby, players, cex=.7, new=FALSE)




# calculate posterior modes 
p_mode <- rep(NA, N)
for (i in 1:N){
  p_mode[i] <- mlv(post$alpha[, i], method="mfv")$M
}
p_mode <- plogis(p_mode) # put on probability scale

# update previous plot
plot(ftd$FTA, ftd$percent,
     xlab="Free throw attempts", 
     ylab="Proportion of free throws made", 
     col=color.scale(p_mode, 
                     c(0,.9),
                     1,1,color.spec="hsv"), 
     pch=19)
lseq <- seq(min(p_mode), max(p_mode), 
            length.out = 7)
legend("bottomright", pch=rep(19, 4), 
       col=color.scale(lseq, 
                       c(0,.9),
                       1,1,color.spec="hsv"), 
       legend=paste("Posterior mode =", round(lseq, digits=2)), 
       bty="n")



# make a mean-mean shrinkage plot
plot(y=c(p_mode, ftd$percent), x=rep(0:1, each=N), 
     xaxt="n", 
     ylab="P(make freethrow)", 
     xlab="",
     xlim=c(-.1, 1.1), 
     col=alpha("black", .2))
title("Shrinkage plot")
axis(3, at=c(0, 1), labels=c("Random effects", "Fixed effects"), padj=1)
segments(y0=p_mode, x0=rep(0, N), 
         y1=ftd$percent, x1=rep(1, N), 
         col=alpha("black", .2))
# notice how the shooters that made all or no free throws 
# are strongly shrunk (because they always made few attempts)
# in other words, they never got a chance to prove themselves
# as exceptionally good or exceptionally bad



# who were the top freethrow shooters?
players <- ftd$Player[p_mode > quantile(p_mode, 1-4/N)]
id <- rep(NA, length(players))
par(mfrow=c(1, 1))
plot(x=NULL, y=NULL, ylim=c(0, 50), 
     xlim=c(.0, 1), xlab="Pr(make freethrow)", 
     ylab="Posterior density", 
)

labx <- rep(NA, length(id))
laby <- rep(NA, length(id))

for (i in 1:length(id)){
  id[i] <- which(ftd$Player == players[i])
  vals <- plogis(post$alpha[, id[i]])
  den <- density(vals)
  lines(den)
  labx[i] <- median(vals)
  laby[i] <- max(den$y)
}

textplot(labx, laby, players, cex=.7, new=FALSE)


# who was the worst?
players <- ftd$Player[p_mode < quantile(p_mode, 4/N)]
id <- rep(NA, length(players))

labx <- rep(NA, length(id))
laby <- rep(NA, length(id))

for (i in 1:length(id)){
  id[i] <- which(ftd$Player == players[i])
  vals <- plogis(post$alpha[, id[i]])
  den <- density(vals)
  lines(den)
  labx[i] <- median(vals)
  laby[i] <- max(den$y)
  #  text(x=median(vals), y=max(den$y), players[i], 
  #       pos=4, srt=90, cex=.7)
}
textplot(labx, laby, players, cex=.7, new=FALSE)



# generate posterior for the best player
# a derived parameter, therefore we can get the full 
# posterior distribution with our MCMC samples
# initialize an N vector of zeros
p_best <- rep(0, N)

# sum up the number of times each player has max. pr(make freethrow)
for (i in 1:nsims){
  b <- which.max(post$alpha[i, ])
  p_best[b] <- p_best[b] + 1
}

p_best <- p_best / nsims
# easy peasy

# rearrange the data by posterior probability of being the 
# best freethrow shooter
ftd$p_best <- p_best
ftd <- ftd[with(ftd, order(-p_best)), ]
head(ftd)




# make a plot for the posterior probability distribution 
# of the best free throw shooter
par(mai=c(1.3, 2.5, 1, 1))
barplot(ftd$p_best[10:1], 
        names.arg=ftd$Player[10:1], 
        horiz=TRUE, 
        main="Best free throw shooter", 
        las=1, 
        xlab="Posterior probability")
par(op)




# update previous plot
plot(ftd$FTA, ftd$percent,
     xlab="Free throw attempts", 
     ylab="Proportion of free throws made", 
     col=color.scale(p_mode, 
                     c(0,.9),
                     1,1,color.spec="hsv"), 
     pch=19)
legend("bottomright", pch=rep(19, 4), 
       col=color.scale(lseq, 
                       c(0,.9),
                       1,1,color.spec="hsv"), 
       legend=paste("Posterior mode =", round(lseq, digits=2)), 
       bty="n")
points(ftd$FTA[1], ftd$percent[1], lwd=5)
text(x=ftd$FTA[1], y=ftd$percent[1], 
     labels=ftd$Player[1], 
     pos=4)
