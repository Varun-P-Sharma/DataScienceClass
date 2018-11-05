# ants dataset
library(lme4)
ants <- read.csv("data/ants.csv")
head(ants)

# Model equations
# Richness ~ Habitat * Latitude + Elevation + (1|Site)
# Elevation is a site-level covariate
# Latitude is a site-level covariate
# Richness is the response variable
# Habitat is a observation-level covariate

# Site is a grouping level
# Habitat is not a grouping level since we want to treat it as a fixed effect

# Richness[i] ~ Poisson(mu[i])

# log(mu[i]) ~ alpha[i,j] + beta_1*latitude + beta_2*habitat + beta_12 (latitude*habitat) + beta_4*elevation

# alpha[j] ~ normal(mu_alpha, sigma2_alpha)



ant.lmer <- glmer(richness ~ habitat*scale(latitude) + scale(elevation) + (1|site)-1, data = ants, family = poisson)
summary(ant.lmer)



# Intercept is the log(richness) at the average latitude for bog sites at sea level
# Habitat forest is the increase in richness for forests at sea level and mean latitude
# For BOG sites, each SD increase in latitude results in a 0.23 decline in log(richness)
# For all sites, each SD increase in elevation lowers log(richness) by -0.19
# In forests, the slope of latitude on richness is steeper, but not significantly so


