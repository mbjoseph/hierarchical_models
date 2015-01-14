# Analyzing the Alot blood parasite data
# set working directory to location of alot.csv
setwd("R_examples/alot")

# load relevant packages
library(ggplot2)
library(lme4)
library(grid) # for arrow() function

# collect (simulate) a dataset
source("alot_sim.R")

# look at structure of dataset
str(d)

# explore the data a bit
ggplot(d, aes(x=body_size, y=log_parasites)) + 
  geom_point(pch=1) + 
  theme_bw() + 
  xlab("Body size (tons)") + 
  ylab("Blood parasite intensity")

ggplot(d, aes(x=site, y=log_parasites)) + 
  geom_boxplot() + 
  theme_bw() + 
  xlab("Site") + 
  ylab("Blood parasite intensity")

# So, it looks like we have among-site variation
# and possibly a relationship w/ size
# let's build a mixed model with: 
# random effect of site and a fixed effect of body size
m1 <- lmer(log_parasites ~ body_size + (1|site), 
           data=d)

# gather up intercepts and slopes for each site
intercepts <- fixef(m1)[1] + unlist(ranef(m1))
slopes <- rep(fixef(m1)[2], length(unique(d$site)))
params <- data.frame(intercepts, slopes, site=unique(d$site))

# visualize fit
ggplot(d, aes(x=body_size, y=log_parasites)) + 
  geom_point(pch=1) + 
  theme_bw() + 
  xlab("Body size (tons)") + 
  ylab("Blood parasite intensity") + 
  geom_abline(aes(intercept = intercepts, 
              slope = slopes), 
              alpha=.5, 
              data=params) + 
  ggtitle("Random intercept, fixed slope model")

# other plots that are worth checking
plot(m1) # residuals vs. fitted

# What if the relationship with size varies among sites?
ggplot(d, aes(y=log_parasites, x=body_size)) + 
  geom_point(pch=1) + 
  facet_wrap(~site) + 
  theme_bw() + 
  xlab("Body size (tons)") + 
  ylab("Blood parasite intensity")

# How well does our previous model fit the data, plotted like this?
ggplot(d, aes(y=log_parasites, x=body_size)) + 
  geom_point(pch=1) + 
  facet_wrap(~site) + 
  theme_bw() + 
  xlab("Body size (tons)") + 
  ylab("Blood parasite intensity") + 
  geom_abline(aes(intercept = intercepts, 
                slope = slopes), 
            alpha=.5, 
            data=params)

# Uh oh, maybe we should allow for among-site variation in the slopes
# let's allow the slopes AND intercpets to vary for each site
m2 <- lmer(log_parasites ~ body_size + (1 + body_size|site), 
           data=d)
m2

# How did that do?
intercepts2 <- fixef(m2)[1] + unlist(ranef(m2)$site["(Intercept)"])
slopes2 <- fixef(m2)[2] + unlist(ranef(m2)$site["body_size"])
params2 <- data.frame(intercepts = intercepts2, 
                      slopes = slopes2, 
                      site=unique(d$site))

# does model fit the observed data better?
ggplot(d, aes(y=log_parasites, x=body_size)) + 
  geom_point(pch=1) + 
  facet_wrap(~site) + 
  theme_bw() + 
  xlab("Body size (tons)") + 
  ylab("Blood parasite intensity") + 
  geom_abline(aes(intercept = intercepts, 
                  slope = slopes), 
              alpha=.5, 
              data=params2)

# any more alarming residual patterns? 
plot(m2)

# looks a bit more reasonable, but are we overfitting?
# could use an information theoretic approach to compare models
AIC(m1, m2)





# looks like the random slope, random intercept model prevails!
# which makes sense, as this is the true model

# visualize fit slightly differently
p1 <- ggplot(d, aes(x=body_size, y=log_parasites)) + 
  geom_abline(intercept = fixef(m2)[1], 
              slope = fixef(m2)[2], col="red", 
              size=2, 
              alpha=.8) + 
  geom_point(pch=1) + 
  theme_classic() + 
  xlab("Body size (tons)") + 
  ylab("Blood parasite intensity") + 
  geom_abline(aes(intercept = intercepts, 
                  slope = slopes), 
              alpha=.5, 
              data=params2) + 
  ggtitle("Random intercept, random slope model")
p1


# Let's 1) compare the fit of the unshrunk vs. shrunk estimates
# then  2) visualize shrinkage in two dimensions
# first, obtain unshrunk estimates
# we want a model that estimates intercepts and slopes for each site independently
# i.e. ANCOVA
m3 <- lm(log_parasites ~ body_size * site, data=d)

# shennanigans to create a data frame of site-specific slopes and intercepts
m3_intercepts <- coef(m3)[!grepl("body_size", names(coef(m3)))]
m3_intercepts <- m3_intercepts +  m3_intercepts[1]
m3_intercepts[1] <- m3_intercepts[1] * .5
m3_slopes <- coef(m3)[grepl("body_size", names(coef(m3)))]
m3_slopes <- m3_slopes +  m3_slopes[1]
m3_slopes[1] <- m3_slopes[1] * .5

# bundle those parameters into a data.frame
params3 <- data.frame(intercepts = m3_intercepts, 
                      slopes = m3_slopes, 
                      site=unique(d$site))

# bundle shrunk and unshrunk estimates together
shrink2d <- data.frame(intF = m3_intercepts, 
                       intR = intercepts2, 
                       slopeF = m3_slopes, 
                       slopeR = slopes2, 
                       n_j = n_j, 
                       site = unique(d$site))

# compare fit of fixed (ANCOVA) vs. random (lmer) estimates
# notice amount of shrinkage for sites with less data
# red line gives independent estimates
# blue line gives shrunken estimates
# blue dashed line gives overall estimated mean
ggplot(d, aes(y=log_parasites, x=body_size)) + 
  facet_wrap(~site) + 
  theme_bw() + 
  xlab("Body size (tons)") + 
  ylab("Blood parasite intensity") + 
  geom_abline(aes(intercept = intR, 
                  slope = slopeR), 
              data=shrink2d, 
              col="blue") + 
  geom_abline(aes(intercept = intF, 
                  slope = slopeF),  
              data=shrink2d, 
              col="red") + 
  geom_abline(intercept = fixef(m2)[1], 
              slope = fixef(m2)[2], 
              col="blue", 
              linetype="dashed") +
  ggtitle("Fixed (red) vs. random (blue) effects fit") + 
  geom_point(pch=1) 
  


# Visualize shrinkage in two dimensions...
# generate a probability density surface of the joint MVNormal distribution
# of intercepts & slopes estimated in our model
library(mvtnorm)
lo <- 200 # length.out of x and y grid
xvals <- seq(from = min(shrink2d$intF), 
             to = max(shrink2d$intF), 
             length.out = lo)
yvals <- seq(from = min(shrink2d$slopeF), 
             to = max(shrink2d$slopeF), 
             length.out = lo)

# now expand the range of values to ensure a large plotting surface
xvals <- 1.5 * xvals
yvals <- 1.5 * yvals

# compute bivariate normal density for random effects, 
# using the ML estimates we generated with lme4
z <- matrix(0, lo, lo)
for (i in 1:lo) {
  x = xvals
  y = yvals[i]
  z[,i] = dmvnorm(cbind(x,y), 
                  mean = fixef(m2), # bivariate mean = fixed effects vector
                  sigma = VarCorr(m2)$site) # estimated covariance matrix
}

# great, but ggplot2 works best with data.frames, so we need to convert
# z from a matrix to a data frame
require(reshape2)
mv_ranef <- melt(z)

# rename columns
names(mv_ranef) <- c("x", "y", "z")

# by default, melt() returned columns of indices rather than original values
# but, we can use this to our advantage: those are the indices of our 
# original x and y values, so the values are easy to recover:
mv_ranef$x <- xvals[mv_ranef$x] 
mv_ranef$y <- yvals[mv_ranef$y]


# visualize the joint shrinkage of slopes and intercepts 
p2 <- ggplot(shrink2d) + 
  geom_raster(aes(x=x, y=y, fill=z), data=mv_ranef) + 
  geom_segment(aes(x=intF, y=slopeF, xend=intR, yend=slopeR, 
                   size=n_j), 
               arrow = arrow(length = unit(0.3, "cm")), 
               alpha=.7) + 
  xlab("Estimated intercepts") + 
  ylab("Estimated slopes") + 
  ggtitle("Slope and intercept shrinkage plot") +  
  scale_fill_gradient(low="white", high="red") + 
  theme_classic() + 
  theme(legend.position="none")
p2 

# If we don't like the bivariate plot, 
# we can make two independent shrinkage plots.
# But, this can be confusing when intercepts & slopes highly correlated
# because estimates can appear to be shrunken AWAY from grand mean.
# If you really want to do this, and show univariate shrinkage 
# consider rescaling/centering continuous covariates 
# to reduce correlation b/t intercept & slope estimates

# univariate intercept shrinkage plot
p3 <- ggplot(shrink2d) + 
  geom_vline(xintercept = fixef(m2)[1], col="red", 
             size=3, alpha=.8) + 
  geom_segment(aes(x=intF, xend=intR, y="Fixed", yend="Random", size=n_j), 
               arrow = arrow(length = unit(0.3, "cm")), 
               alpha=.5) + 
  theme_bw() + 
  ggtitle("Intercept shrinkage plot") + 
  ylab("Effect type")
p3

# univariate slope shrinkage
p4 <- ggplot(shrink2d) + 
  geom_vline(xintercept = fixef(m2)[2], col="red", 
             size=3, alpha=.8) + 
  geom_segment(aes(x=slopeF, xend=slopeR, y="Fixed", yend="Random", size=n_j), 
               arrow = arrow(length = unit(0.3, "cm")), 
               alpha=.5)+ 
  theme_bw() + 
  ggtitle("Slope shrinkage plot") + 
  ylab("Effect type")
p4

# putting it all together:
library(gridExtra)
grid.arrange(p1, p2, ncol=2)

