# Gelman and Hill Ch 12 
rm(list=ls())
library(lme4)
library(arm)
## Section 12.5 -----
data(radon)
rad <- radon
head(rad)
M2 <- lmer(log_radon ~ floor + log_uranium + (1|county), data = rad)
display(M2)
coef(M2)
fixef(M2)
ranef(M2)
nrow(coef(M2)$county) # 85 different intercepts 
# the coefficients for floor and uranium are the same for all counties

# overall model fit
# 1.49 - 0.64*floor + 0.69*uranium
# for county 85:
rad$log_uranium[85] 
# Ranef[85] = -0.04
# y = 1.456 - 0.64*floor + 0.69*uranium 
# y = 1.456 - 0.64*floor + 0.69* 0.2716137
# y = 1.64 - 0.64*floor
fixef(M2)
