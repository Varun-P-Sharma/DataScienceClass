# Simulations
library(rstan)
library(rstanarm)
library(tidyverse)
options(mc.cores = parallel::detectCores())

## read in and clean up data
ponddata <- read.csv("diss.data.2017.csv")
ponddata <- subset(ponddata, visit > 1) # take out first visit
ponddata$visit <- ponddata$visit-1
ponddata <- ponddata[, c(2:6, 17,18)] # predictor variables



## SIMULATION 1: RANDOM EFFECT OF SITES ONLY ---------
nsites <- length(levels(ponddata$SiteCode)) # number of levels of the random effect (10)
site.means.gamma <- NA
site.means.gamma <- rnorm(n = nsites, mean = 2, sd = .2) # 10 site-level means (log(mean parasite load))

site.means.df <- data.frame(SiteCode = levels(ponddata$SiteCode), ExpMean = site.means.gamma)
# merge this with the existing predictors
ponddata2 <- merge(ponddata, site.means.df, by = "SiteCode")

# for each row of the dataset, draw a random parasite count from the expected site level means
ponddata2$Echinostoma <- rnbinom(n = nrow(ponddata2), mu = exp(ponddata2$ExpMean), size = 1)
# using the negative binomial
# not really sure what size means here but it seems that smaller values make it more aggregated


# fit a model
stan.fit.sim1 <- stan_glmer(Echinostoma~ (1|SiteCode), data = ponddata2,  family =neg_binomial_2(link="log"))
summary(stan.fit.sim1)
rand.eff.sim1 <- coefficients(stan.fit.sim1)$SiteCode
rand.eff.sim1
site.means.gamma

# plot the estimates vs. real 
plot(rand.eff.sim1[,1]~site.means.gamma, xlab = "Actual Random Effects", ylab = "Estimated Random Effects")
abline(0,1)
# they look ok...what if we do this a few more times and see how variable the estimates are?
site.means.gamma <- rnorm(n = nsites, mean = 2, sd = .2) # 10 site-level means (log(mean parasite load))
site.means.df <- data.frame(SiteCode = levels(ponddata$SiteCode), ExpMean = site.means.gamma)
ponddata2 <- merge(ponddata, site.means.df, by = "SiteCode")
ponddata2$Echinostoma <- rnbinom(n = nrow(ponddata2), mu = exp(ponddata2$ExpMean), size = 1)
stan.fit.sim1 <- stan_glmer(Echinostoma~ (1|SiteCode), data = ponddata2,  family =neg_binomial_2(link="log"))
rand.eff.sim1 <- coefficients(stan.fit.sim1)$SiteCode
plot(rand.eff.sim1[,1]~site.means.gamma, xlab = "Actual Random Effects", ylab = "Estimated Random Effects")
summary(stan.fit.sim1)
abline(0,1) # this one was really bad! (I realize each time we'll get much different estimates)
# it's hard to repeat this varying things like mean, sd because the model takes so long to fit! oh well.

# let's try this with  smaller variation between sites
site.means.gamma <- rnorm(n = nsites, mean = 2, sd = .05) # 10 site-level means (log(mean parasite load))
site.means.df <- data.frame(SiteCode = levels(ponddata$SiteCode), ExpMean = site.means.gamma)
ponddata2 <- merge(ponddata, site.means.df, by = "SiteCode")
ponddata2$Echinostoma <- rnbinom(n = nrow(ponddata2), mu = exp(ponddata2$ExpMean), size = 1)
stan.fit.sim1 <- stan_glmer(Echinostoma~ (1|SiteCode), data = ponddata2,  family =neg_binomial_2(link="log"))
rand.eff.sim1 <- coefficients(stan.fit.sim1)$SiteCode
plot(rand.eff.sim1[,1]~site.means.gamma, xlab = "Actual Random Effects", ylab = "Estimated Random Effects")
summary(stan.fit.sim1)
abline(0,1) 

# try with a poisson instead of negative binomial
site.means.gamma <- rnorm(n = nsites, mean = 2, sd = .5) # 10 site-level means (log(mean parasite load))
site.means.df <- data.frame(SiteCode = levels(ponddata$SiteCode), ExpMean = site.means.gamma)
ponddata2 <- merge(ponddata, site.means.df, by = "SiteCode")
ponddata2$Echinostoma <- rpois(n = nrow(ponddata2), lambda = exp(ponddata2$ExpMean))
stan.fit.sim1 <- stan_glmer(Echinostoma ~ (1|SiteCode), data = ponddata2,  family =poisson(link="log"))
rand.eff.sim1 <- coefficients(stan.fit.sim1)$SiteCode
plot(rand.eff.sim1[,1]~site.means.gamma, xlab = "Actual Random Effects", ylab = "Estimated Random Effects")
abline(0,1) 
summary(stan.fit.sim1, digits = 3)

# I think the dispersion parameter is leading to a lot of noise because the Poisson fit is better (makes sense--there is an extra parameter to estimate with the negative binomial)

# The SD estimate seems biased low (0.2)
# The mean is right (2)


## SIMULATION 2: RANDOM EFFECT OF SITES AND SAMPLE ---------------
# the visit effects are drawn randomly from the mean for that site
site.means.gamma <- rnorm(n = nsites, mean = 3, sd = .5) # 10 site-level means (log(mean parasite load))
visit.site.means <- data.frame(SiteCode = NA, visit = 1:5, visitmean = NA)
visit.site.means <- rep(list(visit.site.means), 10)
for(i in 1:nsites){
  visit.site.means[[i]][,1]<- levels(ponddata$SiteCode)[i]
  visit.site.means[[i]][,3] <- rnorm(n = 5, mean = site.means.gamma[i], sd = 0.2)

} # visit specific random effects

site.means.df <- data.frame(SiteCode = levels(ponddata$SiteCode), ExpMean = site.means.gamma)

visit.site.means <- do.call(rbind, visit.site.means)


# merge these into the dataframe
ponddata2 <- merge(ponddata, site.means.df, by = "SiteCode")
ponddata2 <- merge(ponddata2, visit.site.means, by = c("SiteCode", "visit") )

# simulate the expected number of parasites from the visit mean
ponddata2$Echinostoma <- rnbinom(n = nrow(ponddata2), mu = exp(ponddata2$visitmean), size = 2)

# fit the model
ponddata2$SiteCodevisit <- paste(ponddata2$SiteCode, "_", ponddata2$visit, sep = "")
stan.fit.sim2 <- stan_glmer(Echinostoma ~ (1|SiteCode) + (1|SiteCodevisit), data = ponddata2,  family =neg_binomial_2(link="log"))

# examine fit
rand.eff.sim2.sc <- coefficients(stan.fit.sim2)$SiteCode
rand.eff.sim2.sv <- coefficients(stan.fit.sim2)$SiteCodevisit

plot(rand.eff.sim2.sc[,1]~site.means.gamma, xlab = "Actual Random Effects", ylab = "Estimated Random Effects")
abline(0,1)  # looks good!

visit.site.means$SiteCodevisit <- paste(visit.site.means$SiteCode,"_", visit.site.means$visit, sep = "")
rand.eff.sim2.sv$SiteCodevisit <- rownames(rand.eff.sim2.sv)
rand.eff.sim2.sv <- merge(rand.eff.sim2.sv, visit.site.means, by = "SiteCodevisit")
plot(visitmean~`(Intercept)`, rand.eff.sim2.sv, xlab = "Actual Random Effects", ylab = "Estimated Random Effects")
abline(0,1)  # hm. not great.

