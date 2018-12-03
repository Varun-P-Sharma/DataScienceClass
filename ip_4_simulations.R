# Simulations

nsites <- 10
site.means.gamma <- NA
beta.t.sites <- NA
visits <- 1:5

parasite.sim.data <- expand.grid("Sites" = LETTERS[1:10], "Visit" = 1:5,
                                 "Species" = c("PSRE", "TATO"), "SVL" = seq(-1,1,.2))
parasite.sim.data$collection <- paste(parasite.sim.data$Sites, parasite.sim.data$Visit)

# First, simulate a site-level mean log(parasite load) at time zero (random intercept term)

site.means.gamma <- rnorm(n = nsites, mean = 0.5, sd = .2)

# Simulate a beta_t (effect of time for that site)

beta.t.sites <- rnorm(n = nsites, mean = 0.2, sd = .05)

# Calculate a predicted parasite load at time point t for a given site (a sample mean)
# Integrate this into the data-frame?
# loop through the sites and do this?

exp.samp.mean <- site.means.gamma + beta.t.sites*visits

# Simulate the sample visit mean randomly from that predicted value


# Now, simulate how much each individual varies from that sample visit based on species and snout vent length

