# Analyzing the Alot blood parasite data

# set working directory to location of alot.csv
setwd("R_examples/alot")

# simulate a dataset
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
# let's build a mixed model with a random effect of site and a fixed effect of body size
library(lme4)
m1 <- lmer(log_parasites ~ body_size + (1|site), 
           data=d)

# gather up intercepts
intercepts <- fixef(m1)[1] + unlist(ranef(m1))
slopes <- rep(fixef(m1)[2], length(unique(d$site)))

# let's plot those results
params <- data.frame(intercepts, slopes, site=unique(d$site))

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




# But what if the relationship with size varies among sites?
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
# let's assume that the slopes for each site are normally distributed too:
# beta_j ~ Normal(mu_beta_j, sigma_beta_j)
m2 <- lmer(log_parasites ~ body_size + (1 + body_size|site), 
           data=d)

# How did that do?
intercepts2 <- fixef(m2)[1] + unlist(ranef(m2)$site["(Intercept)"])
slopes2 <- fixef(m2)[2] + unlist(ranef(m2)$site["body_size"])
params2 <- data.frame(intercepts = intercepts2, 
                      slopes = slopes2, 
                      site=unique(d$site))

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
              data=params2)

# looks a bit more reasonable, but are we overfitting?
# could use an information theoretic approach to compare models
AIC(mod1, mod2)



# looks like the random slope, random intercept model prevails!
# which makes sense, as this is the true model
ggplot(d, aes(x=body_size, y=log_parasites)) + 
  geom_abline(intercept = fixef(m2)[1], 
              slope = fixef(m2)[2], col="red", 
              size=2, 
              alpha=.8) + 
  geom_point(pch=1) + 
  theme_bw() + 
  xlab("Body size (tons)") + 
  ylab("Blood parasite intensity") + 
  geom_abline(aes(intercept = intercepts, 
                  slope = slopes), 
              alpha=.5, 
              data=params2) + 
  ggtitle("Random intercept, random slope model")



# Let's visualize shrinkage in two dimensions
# first, obtain unshrunk estimates
m3 <- lm(log_parasites ~ body_size * site, data=d)

# shennanigans to create a data frame of site-specific slopes and intercepts
m3_intercepts <- coef(m3)[!grepl("body_size", names(coef(m3)))]
m3_intercepts <- m3_intercepts +  m3_intercepts[1]
m3_intercepts[1] <- m3_intercepts[1] * .5
m3_slopes <- coef(m3)[grepl("body_size", names(coef(m3)))]
m3_slopes <- m3_slopes +  m3_slopes[1]
m3_slopes[1] <- m3_slopes[1] * .5

params3 <- data.frame(intercepts = m3_intercepts, 
                      slopes = m3_slopes, 
                      site=unique(d$site))

# bundle shrunk and unshrunk estimates together
shrink2d <- data.frame(intF = m3_intercepts, 
                       intR = intercepts2, 
                       slopeF = m3_slopes, 
                       slopeR = slopes2)

# make a plot
library(grid) # for arrow() function
ggplot(shrink2d) + 
  geom_segment(aes(x=intF, y=slopeF, xend=intR, yend=slopeR), 
               arrow = arrow(length = unit(0.3, "cm"))) + 
  theme_bw() + 
  xlab("Estimated intercepts") + 
  ylab("Estimated slopes") + 
  ggtitle("Slope and intercept shrinkage plot") 

# since we know the true values, let's add those and see whether shrinkage helped
true_d <- data.frame(true_intercept=site_eff + grand_mean, 
                     true_slope = beta_size)
ggplot(shrink2d) + 
  geom_segment(aes(x=intF, y=slopeF, xend=intR, yend=slopeR), 
               arrow = arrow(length = unit(0.3, "cm"))) + 
  theme_bw() + 
  xlab("Estimated intercept") + 
  ylab("Estimated slope") + 
  ggtitle("Slope and intercept shrinkage plot") + 
  geom_point(aes(x=true_intercept, y=true_slope), 
             color="blue", size=4, alpha=.5, 
             data=true_d)


