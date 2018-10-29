#' ---
#' output: github_document
#' ---

# Data Analysis # 

## Data description==========
# Data are parasite community data within amphibian larvae, analyzed over time.
# Experimental design: visit 10 sites (ponds) 5 times each over the summer (2017). At each visit, # collect 10 individuals of each species (newt: TATO and chorus frog:PSRE) and dissect.
# Each row is an individual amphibian larvae with columns being individual level data as well as counts of various parasite species
# For this analysis, I will model the abundance of one parasite species (Echinostoma). Response variable is number of parasites within an individual (Poisson distributed). Explanatory variable is visit date (fixed), site (random), individual relative size (fixed), and species (fixed).


## Libraries=================
library(ggplot2)
library(dplyr)
## Exploratory analyses========
# read in data
dis <- read.csv("diss.data.2017.csv")
str(dis)
dis$X = NULL # get rid of CSV's X column
colnames(dis)

## Adding new columns ==========
# make a new column that is scaled Snout-Vent-Length
# At each visit and for each species, take the average SVL.
# Scale each individual relative to that average
# Smaller than average individuals have a negative SVL and larger than average have a positive

# first, let's take the avg SVL at each site-visit combination
# This is basically a "cohort mean"
meanSVLSiteSpec <- dis %>% group_by (CollectionCode, SpeciesCode) %>% summarise(meanSVL = mean(SVL), sdSVL = sd(SVL))
# For each individual, standardize their size relative to the cohort mean
# merge these data
dis <- left_join(dis, meanSVLSiteSpec, by = c("CollectionCode", "SpeciesCode"))
# some sites have only 1 individual, for those, the scaled SVL will be NA
dis <- dis %>% mutate(siteScaledSVL = (SVL-meanSVL)/sdSVL)
dis$meanSVL =NULL
dis$sdSVL = NULL

# now let's try it without a site-specific standardization. Just standardize for VISIT
# meaning, some sites will have smaller-than-average larvae
meanSVLVisitSpec <- dis %>% group_by (visit, SpeciesCode) %>% summarise(meanVSVL = mean(SVL), sdVSVL = sd(SVL))
dis <- left_join(dis, meanSVLVisitSpec, by = c("visit", "SpeciesCode"))
dis <- dis %>% mutate(visitScaledSVL = (SVL-meanVSVL)/sdVSVL)
dis$meanVSVL =NULL
dis$sdVSVL = NULL
## Data visualization =========

# relationship between Echinostoma parasite count and visit
# question: do individual's parasite burdens increase over time?
ggplot(data = dis) +
  geom_jitter(aes(x=visit, y = Echinostoma, color = SpeciesCode))
# data are Poisson distributed so try a log transformation for visualization
ggplot(data = dis) +
  geom_jitter(aes(x=visit, y = log(Echinostoma+.01), color = SpeciesCode))
# there are a lot of zeroes and a lot of noise!

# let's try separating out by site to reduce some of this noise
ggplot(data = dis) +
  geom_jitter(aes(x=visit, y = log(Echinostoma+.01), color = SpeciesCode))+
  facet_wrap(facets = ~SiteCode)
# it looks like newts (TATO) don't seem to get Echinostoma at some of the sites (weird!)
# In some sites, it seems like Echinostoma count goes up with time
# Might this be a case where we need random slopes and intercepts?

# question: do individual's relative size (are they late bloomers or early bloomers) impact their parasite counts?
ggplot(data = dis)+
  geom_jitter(aes(x = siteScaledSVL, y = log(Echinostoma + .1), color = SpeciesCode))+
  facet_wrap(facets= ~SiteCode)
# I don't see much of a relationship here

# do sites with delayed growth have more parasites?
# negative values indicate sites with smaller than average individuals
ggplot(data = dis)+
  geom_jitter(aes(x = visitScaledSVL, y = log(Echinostoma + .1), color = SpeciesCode))
# nah not really

# do individuals with delayed growth have more parasites?
# negative values indicate sites with smaller than average individuals
ggplot(data = dis)+
  geom_jitter(aes(x = siteScaledSVL, y = log(Echinostoma + .1), color = SpeciesCode))
# nah not really

# check distribution of important variables
ggplot(data = dis)+
  geom_histogram(aes(visitScaledSVL, group = SpeciesCode, color = SpeciesCode), alpha = .2)+
  geom_vline(xintercept = 0, col = "red")+
  facet_wrap(facets = ~SiteCode)

ggplot(data = dis)+
  geom_density(aes(log(Echinostoma+.1), color = SpeciesCode)) +
  facet_wrap(facets = ~SiteCode)

ggplot(data = dis)+
  geom_histogram(aes(Echinostoma, color = SpeciesCode), alpha =.4) +
  facet_wrap(facets = ~SiteCode)


# my response variable is overdispersed (lots of zeroes!)
# this is normal for parasite data


