# fun facts about hierarchical models
# max joseph december 2014
# simple random intercept model
library(scales)
library(lme4)
library(ggplot2)
op <- par(no.readonly = TRUE)

# ----------------------- #
# demonstrating shrinkage #
# ------------------------#
# we have the following random intercept model
# Y[i, j] ~ Normal(alpha[j], sigma_y)
# alpha[j] ~ Normal(mu_alpha, sigma_alpha)
# with wildly varying per-group sample sizes

# set parameters for simulation
K <- 20 # number of groups
mu_alpha <- 0 # population mean
sigma_alpha <- 1 # among group sd
sigma_Y <- 2 # residual sd
e_nmin <- .01 # exp(minimum group sample size)
e_nmax <- 3.5 # exp(maximum group sample size)
n_j <- floor(exp(seq(e_nmin, e_nmax, length.out=K))) 
id <- rep(1:K, times=n_j)
N <- length(id)

# view distribution of sample sizes
hist(n_j, right=FALSE, breaks=1:max(n_j))

# simulate vals
alpha_j <- rnorm(K, mu_alpha, sigma_alpha)
Y <- rnorm(N, alpha_j[id], sigma_Y)

## Fit a complete pooling / grand mean model
mod1 <- lm(Y ~ 1)

# view results
plot(Y~id, ylab="observation", xlab="group")
points(alpha_j, col=alpha("blue", .5), cex=2, pch=19)
abline(mod1, col=alpha("red", .6), 
       lty=3, lwd=3)
legend("topleft", 
       pch=c(19, 1, NA), 
       lty=c(0, 0, 3), 
       pt.cex=c(2, 1, NA), 
       lwd=c(NA, NA, 3),
       col=c(alpha("blue", .5), "black", "red"), 
       legend=c("True group means", 
                "Observations", 
                "Estimated grand mean"), 
       bty="n")

# total mean dominated by groups with high n
# true group means shown as blue dots
# total pooling ==> underfitting


## Fit a no pooling / independent means model
mod2 <- lm(Y ~ 1 + factor(id))
alpha_est1 <- coef(mod2)
alpha_est1[-1] <- alpha_est1[1] + alpha_est1[-1]

plot(Y~id, ylab="observation", xlab="group")
points(alpha_j, col=alpha("blue", .5), cex=2, pch=19)
points(x=1:K, alpha_est1, col=alpha("green", .5), 
       pch=19, cex=2)
legend("topleft", 
       pch=c(19, 19, 1),  
       pt.cex=c(2, 2, 1), 
       col=c(alpha("blue", .5), 
             alpha("green", .5), 
             "black"), 
       legend=c("True group means",
                "Estimated group means",
                "Observations"), 
       bty="n"
       )
# notice independent means ==> overfitting



## Fit partial pooling/hierarchical model
# i.e. random intercept model
fid <- factor(id)
mod3 <- lmer(Y ~ 1 + (1|fid))

plot(Y~id, ylab="observation", xlab="group")
points(alpha_j, col=alpha("blue", .5), cex=2, pch=19)
alpha_est2 <- unlist(ranef(mod3)) + fixef(mod3)
points(x=1:K, alpha_est2, col=alpha("red", .5), 
       cex=2, pch=19)
legend("topleft", 
       pch=c(19, 19, 1),  
       pt.cex=c(2, 2, 1), 
       col=c(alpha("blue", .5), 
             alpha("red", .5), 
             "black"), 
       legend=c("True group means",
                "Estimated group means",
                "Observations"), 
       bty="n"
)
# go back and forth between this plot and the previous to see shrinkage




## Shrinkage plots 
# Plot 1
color_scheme <- function(n_j){
  res <- ifelse(n_j==1, "red", 
                ifelse(n_j==2, "blue", "grey"))
  return(res)
} 
size_scheme <- function(n_j){
  res <- log(n_j)+1
  return(res)
} 
# Shrinkage plot 1
plot(y=c(alpha_est2, alpha_est1), x=rep(0:1, each=K), 
     col=color_scheme(n_j), 
     cex=size_scheme(n_j), 
     xaxt="n", 
     ylab="Estimated group means", 
     xlab="",
     ylim=range(c(alpha_est1)), 
     xlim=c(-.1, 1.1))
title(expression(paste("Shrinkage of ", hat(alpha[k]))))
axis(3, at=c(0, 1), labels=c("Random effects", "Fixed effects"), padj=1)
segments(y0=alpha_est2, x0=rep(0, K), y1=alpha_est1, x1=rep(1, K), 
         col=color_scheme(n_j))
abline(h=mu_alpha, lty=2, lwd=2)
# red points show groups with one observation, blue = 2

# end plot 1

# Plot 2
# Deviations from true values (bias) for both methods
dev1 <- coef(mod1) - alpha_j
dev2 <- alpha_est1 - alpha_j
dev3 <- alpha_est2 - alpha_j

plot(dev2, dev3, cex=size_scheme(n_j), 
     xlab=expression(paste("Fixed effect bias: ", bar(alpha[k]) - alpha[k])), 
     ylab=expression(paste("Random effect bias: ", bar(alpha[k]) - alpha[k])), 
     col=color_scheme(n_j), 
     xlim=range(c(dev1, dev2)), 
     ylim=range(c(dev1, dev2)))
title(expression(paste("Bias comparisons for ", bar(alpha[k]))))
abline(h=0, lty=2)
abline(v=0, lty=2)
abline(0, 1, lty=3)
ndots <- 3
legseq <- c(1, 2, max(n_j))
lab <- rep(NA, ndots)
for (i in 1:ndots){
  lab[i] <- paste("n = ", legseq[i], sep="")
}
legend("topleft", pch=rep(1, ndots), 
       pt.cex=size_scheme(legseq), 
       legend = lab, 
       bty="n", 
       inset=.02,
       y.intersp=1.6, 
       x.intersp = 1.5, 
       col=color_scheme(legseq))
segments(x0=dev2, y0=dev2, x1=dev2, y1=dev3, 
         col=color_scheme(n_j), 
         lty=3)
# end plot 2



# Plot 3
# Dotplots showing reduced error for hierarchical model
df <- data.frame(dev=c(dev2, dev3),
                 model=rep(c("Fixed", "Random"), each=K))
ggplot(df, aes(x=dev, fill=model)) + 
  geom_dotplot(data=subset(df, model=="Fixed"), 
                           aes(dev, fill="Fixed"), 
               alpha=.8) +
  geom_dotplot(data=subset(df, model=="Random"), 
                           aes(dev, fill="Random"),
               stackdir = "down", 
               alpha=.8) + 
  scale_fill_hue("Model") + 
  theme_bw() + 
  xlab("Deviation") + 
  geom_vline(xintercept=0, linetype="dashed")
# end plot 3

  

# Plot 4
# Displaying shrinkage as a function of within group sample-size
# aka omega Gelman and Pardoe 2005
lambda <- (alpha_est2 - fixef(mod3)) / (alpha_est1 - fixef(mod3))

par(mai=c(1.2, 1.2, 1, 3), 
    xpd=TRUE)

plot(n_j, 1-lambda, 
     ylab=expression(1-lambda[j]), 
     xlab=expression(n[j]), 
     ylim=c(0, 1), type='b', 
     main=expression(
       paste(1-lambda[j], " : degree of shrinkage, where ",
                           hat(alpha[j]) == lambda[j]*bar(y[j.]) + 
                                 (1 - lambda[j])*mu[alpha], 
                            sep="")
       )
)
text(x=max(n_j)/2, y=1, labels="Complete shrinkage")
text(x=max(n_j)/2, y=0, labels="No shrinkage")

# explore shrinkage for various ratios of sigma_a / (sigma_a + sigma_y)
sigma_a_vec <- seq(.01, .99, length.out=5)
denom <- 1
sigma_y_vec <- denom - sigma_a_vec
# sigma_a + sigma_y = 1
# p = sigma_a 
leg <- expression()
colors <- NULL

for (i in 1:length(sigma_a_vec)){
  sigma_alpha <- sigma_a_vec[i]
  sigma_Y <- sigma_y_vec[i]
  # simulate vals
  alpha_j <- rnorm(K, mu_alpha, sigma_alpha)
  Y <- rnorm(N, alpha_j[id], sigma_Y)
  fid <- factor(id)
  mod3S <- lmer(Y ~ 1 + (1|fid))
  mod2S <- lm(Y ~ 1 + fid)
  alpha_est1S <- coef(mod2S)
  alpha_est1S[-1] <- alpha_est1S[1] + alpha_est1S[-1]
  alpha_est2S <- unlist(ranef(mod3S)) + fixef(mod3S)
  lambdaS <- (alpha_est2S - fixef(mod3S)) / 
    (alpha_est1S - fixef(mod3S))
  colors <- c(colors, 1+i)
  lines(n_j, 1-lambdaS, col=1 + i)
  theta <- sigma_a_vec[i]
  leg <- c(leg, 
           substitute(expression(
             frac(sigma[alpha], (sigma[alpha]+sigma[Y])) == x), 
                      list(x=sigma_a_vec[i]))[[2]])
}

legend(1.05*max(n_j), 1, lty=1, 
       col=colors, 
       legend=leg, 
       bty="n", 
       y.intersp=2)
par(xpd=FALSE)
# end plot 4
par(op)

