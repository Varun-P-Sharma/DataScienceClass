# Simulations

nsites <- 10
site.means.gamma <- NA
beta.t.sites <- NA
visits <- 1:5
nvisits <- 5
parasite.sim.data <- expand.grid("Sites" = LETTERS[1:10], "Visit" = 1:5,
                                 "Species" = c("PSRE", "TATO"), "SVL" = seq(-1,1,.2))
parasite.sim.data$collection <- paste(parasite.sim.data$Sites, parasite.sim.data$Visit)

# First, simulate a site-level mean log(parasite load) at time zero (random intercept term)

site.means.gamma <- rnorm(n = nsites, mean = 0.5, sd = .2)

# Simulate a beta_t (effect of time for that site)

beta.t.sites <- rnorm(n = nsites, mean = 0.2, sd = .05)

# Calculate a predicted parasite load at time point t for a given site (a sample mean)
# I'm going to loop through the sites and do this, because I don't know how else to do it.
site.visits.exp <- list()

for(i in 1:nsites){
  site.visits.exp[[i]] <- site.means.gamma[i] + beta.t.sites[i]*visits
}

# OK now we have 50 samples and their means calculated (5 from each of 10 sites)

# Simulate the sample visit mean randomly from that predicted value (this will be 50 different alphas)
# collapse all the site visits.exp (mu's) into a vector

site.visits.exp <- unlist(site.visits.exp)
alpha.sites <- rnorm(n = nsites*nvisits, mean = site.visits.exp, sd = .2)

# Now, simulate how much each individual varies from that sample visit based on species and snout vent length

# each individual's covariates are in the dataframe
head(parasite.sim.data)

