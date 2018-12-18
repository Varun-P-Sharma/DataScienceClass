Simulation of parasite data for model checks
================

Read in and clean up data
-------------------------

``` r
ponddata <- read.csv("diss.data.2017.csv")
ponddata <- subset(ponddata, visit > 1) # take out first visit # fix this??
ponddata$visit <- ponddata$visit-1
ponddata <- ponddata[, c(2:6, 17,18)] # get just predictor variables
ponddata$visitScaledSVL <- rnorm(nrow(ponddata), 0, 2)# just make up this predictor 
```

SIMULATION 1: Site-level random effect
--------------------------------------

This model predicts parasite richness using only a random effect for site. Generate random effects for each of 10 sites from a normal distribution

``` r
nsites <- length(levels(ponddata$SiteCode)) # number of levels of the random effect (10)
site.means.gamma <- NA
set.seed(12398) # makes sure the random draws are repeatable (?)

site.means.gamma <- rnorm(n = nsites, mean = 0, sd = .2) # 10 site-level means (log(mean parasite load))

# make a dataframe that we can merge with the predictor data frame
site.means.df <- data.frame(SiteCode = levels(ponddata$SiteCode), ExpMean = site.means.gamma)
# merge this with the existing predictors
ponddata2 <- merge(ponddata, site.means.df, by = "SiteCode")
```

Generate an observed parasite count from the expected site level mean. This is done using a negative binomial distribution.

``` r
# for each row of the dataset, draw a random parasite richness count
# from the expected site level means
ponddata2$tot.para <- rpois(n = nrow(ponddata2), lambda = exp(ponddata2$ExpMean))
```

Fit a model; does it recapture the random effects?

``` r
stan.fit.sim1 <- stan_glmer(tot.para~ (1|SiteCode), data = ponddata2,  family ="poisson")
summary(stan.fit.sim1)
```

    ## 
    ## Model Info:
    ## 
    ##  function:     stan_glmer
    ##  family:       poisson [log]
    ##  formula:      tot.para ~ (1 | SiteCode)
    ##  algorithm:    sampling
    ##  priors:       see help('prior_summary')
    ##  sample:       4000 (posterior sample size)
    ##  observations: 959
    ##  groups:       SiteCode (10)
    ## 
    ## Estimates:
    ##                                           mean    sd      2.5%    25%  
    ## (Intercept)                                -0.1     0.1    -0.2    -0.1
    ## b[(Intercept) SiteCode:PRNTH1]             -0.1     0.1    -0.3    -0.2
    ## b[(Intercept) SiteCode:PRNTH4]             -0.1     0.1    -0.3    -0.2
    ## b[(Intercept) SiteCode:PRNTHIDK]           -0.1     0.1    -0.3    -0.2
    ## b[(Intercept) SiteCode:PRNTHMIT]           -0.2     0.1    -0.4    -0.3
    ## b[(Intercept) SiteCode:PRNTHOWL]            0.0     0.1    -0.2    -0.1
    ## b[(Intercept) SiteCode:PRPND009]            0.1     0.1    -0.1     0.1
    ## b[(Intercept) SiteCode:PRPND010]            0.2     0.1     0.0     0.1
    ## b[(Intercept) SiteCode:PRPND014]           -0.1     0.1    -0.3    -0.2
    ## b[(Intercept) SiteCode:PRPND015]            0.2     0.1     0.0     0.1
    ## b[(Intercept) SiteCode:TGIF]                0.0     0.1    -0.2     0.0
    ## Sigma[SiteCode:(Intercept),(Intercept)]     0.0     0.0     0.0     0.0
    ## mean_PPD                                    0.9     0.0     0.8     0.9
    ## log-posterior                           -1235.0     3.5 -1242.9 -1237.1
    ##                                           50%     75%     97.5%
    ## (Intercept)                                -0.1     0.0     0.1
    ## b[(Intercept) SiteCode:PRNTH1]             -0.1     0.0     0.1
    ## b[(Intercept) SiteCode:PRNTH4]             -0.1     0.0     0.1
    ## b[(Intercept) SiteCode:PRNTHIDK]           -0.1     0.0     0.1
    ## b[(Intercept) SiteCode:PRNTHMIT]           -0.2    -0.1     0.0
    ## b[(Intercept) SiteCode:PRNTHOWL]            0.0     0.0     0.2
    ## b[(Intercept) SiteCode:PRPND009]            0.1     0.2     0.4
    ## b[(Intercept) SiteCode:PRPND010]            0.2     0.3     0.4
    ## b[(Intercept) SiteCode:PRPND014]           -0.1     0.0     0.1
    ## b[(Intercept) SiteCode:PRPND015]            0.2     0.3     0.5
    ## b[(Intercept) SiteCode:TGIF]                0.0     0.1     0.2
    ## Sigma[SiteCode:(Intercept),(Intercept)]     0.0     0.1     0.1
    ## mean_PPD                                    0.9     1.0     1.0
    ## log-posterior                           -1234.6 -1232.4 -1229.3
    ## 
    ## Diagnostics:
    ##                                         mcse Rhat n_eff
    ## (Intercept)                             0.0  1.0  1138 
    ## b[(Intercept) SiteCode:PRNTH1]          0.0  1.0  1891 
    ## b[(Intercept) SiteCode:PRNTH4]          0.0  1.0  1979 
    ## b[(Intercept) SiteCode:PRNTHIDK]        0.0  1.0  2101 
    ## b[(Intercept) SiteCode:PRNTHMIT]        0.0  1.0  1941 
    ## b[(Intercept) SiteCode:PRNTHOWL]        0.0  1.0  2418 
    ## b[(Intercept) SiteCode:PRPND009]        0.0  1.0  1974 
    ## b[(Intercept) SiteCode:PRPND010]        0.0  1.0  2075 
    ## b[(Intercept) SiteCode:PRPND014]        0.0  1.0  2033 
    ## b[(Intercept) SiteCode:PRPND015]        0.0  1.0  2322 
    ## b[(Intercept) SiteCode:TGIF]            0.0  1.0  2000 
    ## Sigma[SiteCode:(Intercept),(Intercept)] 0.0  1.0   916 
    ## mean_PPD                                0.0  1.0  4391 
    ## log-posterior                           0.1  1.0   940 
    ## 
    ## For each parameter, mcse is Monte Carlo standard error, n_eff is a crude measure of effective sample size, and Rhat is the potential scale reduction factor on split chains (at convergence Rhat=1).

``` r
rand.eff.sim1 <- coefficients(stan.fit.sim1)$SiteCode
rand.eff.sim1
```

    ##          (Intercept)
    ## PRNTH1   -0.17005349
    ## PRNTH4   -0.15676673
    ## PRNTHIDK -0.16955944
    ## PRNTHMIT -0.26442440
    ## PRNTHOWL -0.09043619
    ## PRPND009  0.05950080
    ## PRPND010  0.13702361
    ## PRPND014 -0.16679127
    ## PRPND015  0.13060910
    ## TGIF     -0.03831745

``` r
site.means.gamma
```

    ##  [1] -0.245521446 -0.240448924 -0.096852973 -0.101195460 -0.143450908
    ##  [6]  0.095252545  0.129607614 -0.127518331  0.259110865 -0.005629135

Plot the estimates vs. real

``` r
{plot(rand.eff.sim1[,1]~site.means.gamma, xlab = "Actual Random Effects", ylab = "Estimated Random Effects", xlim = c(-.5,.5), ylim = c(-0.5,.5))
abline(0,1)}
```

![](ip_5_simulations_files/figure-markdown_github/unnamed-chunk-5-1.png)

These look OK, now try again with diff set of random intercepts:

``` r
set.seed(231)
site.means.gamma <- rnorm(n = nsites, mean = 0, sd = .2) # 10 site-level means (log(mean parasite richness))
site.means.df <- data.frame(SiteCode = levels(ponddata$SiteCode), ExpMean = site.means.gamma)
ponddata2 <- merge(ponddata, site.means.df, by = "SiteCode")
ponddata2$tot.para <- rpois(n = nrow(ponddata2), lambda = exp(ponddata2$ExpMean))
stan.fit.sim1 <- stan_glmer(tot.para~ (1|SiteCode), data = ponddata2,  family ="poisson")
rand.eff.sim1 <- coefficients(stan.fit.sim1)$SiteCode
{plot(rand.eff.sim1[,1]~site.means.gamma, xlab = "Actual Random Effects", ylab = "Estimated Random Effects", xlim = c(-.5,.5), ylim = c(-.5, .5))
abline(0,1)} 
```

![](ip_5_simulations_files/figure-markdown_github/unnamed-chunk-6-1.png)

It's hard to repeat this varying things like mean, sd because the model takes so long to fit! oh well. It also seems like the model underestimates variation in random effects--they are shrunk on the y axis...is this a partial pooling thing? A prior thing?

Let's try this with smaller variation between sites

``` r
set.seed(1491)
site.means.gamma <- rnorm(n = nsites, mean = 0, sd = .05) # 10 site-level means (log(mean parasite load))
site.means.df <- data.frame(SiteCode = levels(ponddata$SiteCode), ExpMean = site.means.gamma)
ponddata2 <- merge(ponddata, site.means.df, by = "SiteCode")
ponddata2$tot.para <- rpois(n = nrow(ponddata2), lambda = exp(ponddata2$ExpMean))
stan.fit.sim1 <- stan_glmer(tot.para~ (1|SiteCode), data = ponddata2,  family ="poisson")
rand.eff.sim1 <- coefficients(stan.fit.sim1)$SiteCode
{plot(rand.eff.sim1[,1]~site.means.gamma, xlab = "Actual Random Effects", ylab = "Estimated Random Effects", xlim = c(-.5,.5), ylim = c(-.5,.5))
abline(0,1)}
```

![](ip_5_simulations_files/figure-markdown_github/unnamed-chunk-7-1.png)

SIMULATION 2: Random effects of site and sample
-----------------------------------------------

In this simulation we have a nested random effect for the collection visit (5 per site). The visit effects are drawn randomly from the mean for that site

``` r
set.seed(523)
site.means.gamma <- rnorm(n = nsites, mean = 0, sd = .5) # 10 site-level means (log(mean parasite load))
visit.site.means <- data.frame(SiteCode = NA, visit = 1:5, visitmean = NA)
visit.site.means <- rep(list(visit.site.means), 10)
for(i in 1:nsites){
  visit.site.means[[i]][,1]<- levels(ponddata$SiteCode)[i]
  visit.site.means[[i]][,3] <- rnorm(n = 5, mean = site.means.gamma[i], sd = 0.2)

} # visit specific random effects (the site level re will change each time I run this code)
# since I didn't figure out the set seed thing within a loop
# don't want the visit effects to be the same per site

# get these in df form to combine with predictors
site.means.df <- data.frame(SiteCode = levels(ponddata$SiteCode), ExpMean = site.means.gamma)
visit.site.means <- do.call(rbind, visit.site.means)

# merge these into the dataframe
ponddata2 <- merge(ponddata, site.means.df, by = "SiteCode")
ponddata2 <- merge(ponddata2, visit.site.means, by = c("SiteCode", "visit") )
```

Simulate the expected number of parasites from the visit mean

``` r
ponddata2$tot.para <- rpois(n = nrow(ponddata2), lambda = exp(ponddata2$visitmean))
```

Fit the model

``` r
ponddata2$SiteCodevisit <- paste(ponddata2$SiteCode, "_", ponddata2$visit, sep = "")
stan.fit.sim2 <- stan_glmer(tot.para ~ (1|SiteCode) + (1|SiteCodevisit), data = ponddata2,  family = "poisson")
```

Examine fit

``` r
## site level
rand.eff.sim2.sc <- coefficients(stan.fit.sim2)$SiteCode
rand.eff.sim2.sv <- coefficients(stan.fit.sim2)$SiteCodevisit

{plot(rand.eff.sim2.sc[,1]~site.means.gamma, xlab = "Actual Random Effects", ylab = "Estimated Random Effects", main = "Site Level Random Effects", xlim = c(-1.5,1.5), ylim = c(-1.5,1.5))
abline(0,1)}  # looks good!
```

![](ip_5_simulations_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
## visit level
visit.site.means$SiteCodevisit <- paste(visit.site.means$SiteCode,"_", visit.site.means$visit, sep = "")
rand.eff.sim2.sv$SiteCodevisit <- rownames(rand.eff.sim2.sv)
rand.eff.sim2.sv <- merge(rand.eff.sim2.sv, visit.site.means, by = "SiteCodevisit")
{plot(visitmean~`(Intercept)`, rand.eff.sim2.sv, xlab = "Actual Random Effects", ylab = "Estimated Random Effects", xlim = c(-1,1), ylim = c(-1,1))
abline(0,1)}  #looks less great
```

![](ip_5_simulations_files/figure-markdown_github/unnamed-chunk-11-2.png)

These are pretty big random effects and the deviation is worrisome...

SIMULATION 3: Random effects of site and sample, and fixed effects of species and visit
---------------------------------------------------------------------------------------

richness ~ visitscaledSVL + visit + visit^2 + speciesCode + speciesCode\*visit Set parameters

``` r
nsites <- length(levels(ponddata$SiteCode)) # number of levels of the random effect (10)
site.means.gamma <- NA
set.seed(122)
site.means.gamma <- rnorm(n = nsites, mean = 0, sd = .5) # 10 site-level means (log(mean parasite load))
visit.site.means <- data.frame(SiteCode = NA, visit = 1:5, visitmean = NA)
visit.site.means <- rep(list(visit.site.means), 10)

## set fixed parameters
visit.effect <- 0.85 # fixed effect for visit
visit.effect.sq <- -0.1
species.effect <- -0.4 # fixed effect for species
visit.species.effect <- -1 # fixed effect for species
svl.effect <- 0.3
Effects <- data.frame(FixedEffect = c("Visit", "Species", "Visit^2", "SVL", "Visit*Species"), 
             TrueValue =  c(visit.effect, species.effect, visit.effect.sq, svl.effect, visit.species.effect))

for(i in 1:nsites){
  visit.site.means[[i]][,1]<- levels(ponddata$SiteCode)[i]
  for(j in 1:5){
    Exp.mean <- site.means.gamma[i] + j*visit.effect + j^2*visit.effect.sq
    visit.site.means[[i]][j,3] <- rnorm(n=1, mean = Exp.mean, sd = .1)
  }
} # visit specific random effects drawn from expected mean. Putting the visit effect here. 


# combine into a df with predictors
site.means.df <- data.frame(SiteCode = levels(ponddata$SiteCode), ExpMean = site.means.gamma)
visit.site.means <- do.call(rbind, visit.site.means)
# merge these into the dataframe
ponddata2 <- merge(ponddata, site.means.df, by = "SiteCode")
ponddata2 <- merge(ponddata2, visit.site.means, by = c("SiteCode", "visit") )
```

Simulate the expected number of parasites from the sample mean. The sample mean will become the intercept and I will add the fixed effects.

``` r
# add fixed effect for visit and species (have to treat as numeric to multiply by species slope)
ponddata2$SpeciesCode <- as.numeric(ponddata2$SpeciesCode)-1

ponddata2$ParasiteRichExp <-  ponddata2$visitmean +
                              ponddata2$visitScaledSVL*svl.effect + 
                              ponddata2$SpeciesCode*species.effect +   
                              ponddata2$SpeciesCode*ponddata2$visit*visit.species.effect
```

Generate observed parasite counts (observations) from the expected means, again, using some kind of overdispersion parameter

``` r
ponddata2$ParasiteRichObs<- rpois(n = nrow(ponddata2), lambda = exp(ponddata2$ParasiteRichExp))
```

Fit the model

``` r
ponddata2$SiteCodevisit <- paste(ponddata2$SiteCode, "_", ponddata2$visit, sep = "")
stan.fit.sim3 <- stan_glmer(ParasiteRichObs ~ visit*SpeciesCode + I(visit^2) + visitScaledSVL +  (1|SiteCode) + (1|SiteCodevisit), data = ponddata2,  family = "poisson")
```

Examine fit

``` r
summary(stan.fit.sim3, digits = 4)
```

    ## 
    ## Model Info:
    ## 
    ##  function:     stan_glmer
    ##  family:       poisson [log]
    ##  formula:      ParasiteRichObs ~ visit * SpeciesCode + I(visit^2) + visitScaledSVL + 
    ##     (1 | SiteCode) + (1 | SiteCodevisit)
    ##  algorithm:    sampling
    ##  priors:       see help('prior_summary')
    ##  sample:       4000 (posterior sample size)
    ##  observations: 959
    ##  groups:       SiteCodevisit (48), SiteCode (10)
    ## 
    ## Estimates:
    ##                                                mean       sd      
    ## (Intercept)                                      0.2161     0.2301
    ## visit                                            0.6894     0.0952
    ## SpeciesCode                                     -0.4452     0.2064
    ## I(visit^2)                                      -0.0753     0.0153
    ## visitScaledSVL                                   0.2974     0.0082
    ## visit:SpeciesCode                               -1.0075     0.0915
    ## b[(Intercept) SiteCodevisit:PRNTH1_1]            0.0107     0.0928
    ## b[(Intercept) SiteCodevisit:PRNTH1_2]            0.0462     0.0804
    ## b[(Intercept) SiteCodevisit:PRNTH1_3]            0.1410     0.0805
    ## b[(Intercept) SiteCodevisit:PRNTH1_4]           -0.0922     0.0752
    ## b[(Intercept) SiteCodevisit:PRNTH1_5]           -0.0872     0.0845
    ## b[(Intercept) SiteCodevisit:PRNTH4_1]            0.0284     0.0957
    ## b[(Intercept) SiteCodevisit:PRNTH4_2]            0.0536     0.0920
    ## b[(Intercept) SiteCodevisit:PRNTH4_3]            0.0086     0.0885
    ## b[(Intercept) SiteCodevisit:PRNTH4_4]           -0.0339     0.0817
    ## b[(Intercept) SiteCodevisit:PRNTH4_5]           -0.0752     0.0929
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_1]         -0.0530     0.0973
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_2]         -0.0357     0.0895
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_3]          0.0429     0.0899
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_4]          0.0211     0.0864
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_5]          0.0309     0.1075
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_1]         -0.0433     0.0952
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_2]         -0.0573     0.0910
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_3]          0.0034     0.0893
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_4]          0.0390     0.0774
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_5]          0.0630     0.0874
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_1]          0.0351     0.1017
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_2]         -0.0069     0.1008
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_3]         -0.0980     0.1099
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_4]          0.0234     0.1065
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_5]         -0.0011     0.1096
    ## b[(Intercept) SiteCodevisit:PRPND009_1]          0.0619     0.0888
    ## b[(Intercept) SiteCodevisit:PRPND009_2]          0.0193     0.0794
    ## b[(Intercept) SiteCodevisit:PRPND009_3]          0.0572     0.0741
    ## b[(Intercept) SiteCodevisit:PRPND009_4]         -0.0260     0.0734
    ## b[(Intercept) SiteCodevisit:PRPND009_5]         -0.0774     0.0841
    ## b[(Intercept) SiteCodevisit:PRPND010_1]         -0.0360     0.0956
    ## b[(Intercept) SiteCodevisit:PRPND010_2]         -0.0170     0.0886
    ## b[(Intercept) SiteCodevisit:PRPND010_3]          0.0079     0.0892
    ## b[(Intercept) SiteCodevisit:PRPND010_4]         -0.0231     0.0853
    ## b[(Intercept) SiteCodevisit:PRPND010_5]          0.0735     0.0885
    ## b[(Intercept) SiteCodevisit:PRPND014_1]          0.0163     0.0912
    ## b[(Intercept) SiteCodevisit:PRPND014_2]          0.0047     0.0916
    ## b[(Intercept) SiteCodevisit:PRPND014_3]         -0.0806     0.0950
    ## b[(Intercept) SiteCodevisit:PRPND014_4]         -0.0744     0.0870
    ## b[(Intercept) SiteCodevisit:PRPND014_5]          0.1484     0.0966
    ## b[(Intercept) SiteCodevisit:PRPND015_1]          0.0037     0.0987
    ## b[(Intercept) SiteCodevisit:PRPND015_2]         -0.0670     0.1016
    ## b[(Intercept) SiteCodevisit:PRPND015_3]          0.0382     0.0968
    ## b[(Intercept) SiteCodevisit:TGIF_1]             -0.0393     0.0980
    ## b[(Intercept) SiteCodevisit:TGIF_2]              0.0426     0.0953
    ## b[(Intercept) SiteCodevisit:TGIF_3]              0.0063     0.0846
    ## b[(Intercept) SiteCodevisit:TGIF_4]              0.0053     0.0852
    ## b[(Intercept) SiteCodevisit:TGIF_5]             -0.0180     0.0910
    ## b[(Intercept) SiteCode:PRNTH1]                   0.5272     0.1996
    ## b[(Intercept) SiteCode:PRNTH4]                  -0.3063     0.2022
    ## b[(Intercept) SiteCode:PRNTHIDK]                 0.1100     0.2049
    ## b[(Intercept) SiteCode:PRNTHMIT]                 0.2551     0.1998
    ## b[(Intercept) SiteCode:PRNTHOWL]                -0.9943     0.2207
    ## b[(Intercept) SiteCode:PRPND009]                 0.7439     0.1979
    ## b[(Intercept) SiteCode:PRPND010]                 0.0987     0.2041
    ## b[(Intercept) SiteCode:PRPND014]                 0.1903     0.2048
    ## b[(Intercept) SiteCode:PRPND015]                -0.5813     0.2214
    ## b[(Intercept) SiteCode:TGIF]                    -0.0417     0.2028
    ## Sigma[SiteCodevisit:(Intercept),(Intercept)]     0.0120     0.0068
    ## Sigma[SiteCode:(Intercept),(Intercept)]          0.3544     0.2271
    ## mean_PPD                                         3.7547     0.0883
    ## log-posterior                                -1437.9403     7.3901
    ##                                                2.5%       25%     
    ## (Intercept)                                     -0.2243     0.0643
    ## visit                                            0.4952     0.6286
    ## SpeciesCode                                     -0.8577    -0.5819
    ## I(visit^2)                                      -0.1047    -0.0853
    ## visitScaledSVL                                   0.2812     0.2919
    ## visit:SpeciesCode                               -1.1923    -1.0682
    ## b[(Intercept) SiteCodevisit:PRNTH1_1]           -0.1730    -0.0494
    ## b[(Intercept) SiteCodevisit:PRNTH1_2]           -0.1002    -0.0064
    ## b[(Intercept) SiteCodevisit:PRNTH1_3]           -0.0033     0.0848
    ## b[(Intercept) SiteCodevisit:PRNTH1_4]           -0.2529    -0.1409
    ## b[(Intercept) SiteCodevisit:PRNTH1_5]           -0.2652    -0.1415
    ## b[(Intercept) SiteCodevisit:PRNTH4_1]           -0.1643    -0.0322
    ## b[(Intercept) SiteCodevisit:PRNTH4_2]           -0.1184    -0.0080
    ## b[(Intercept) SiteCodevisit:PRNTH4_3]           -0.1742    -0.0466
    ## b[(Intercept) SiteCodevisit:PRNTH4_4]           -0.2030    -0.0850
    ## b[(Intercept) SiteCodevisit:PRNTH4_5]           -0.2728    -0.1327
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_1]         -0.2664    -0.1116
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_2]         -0.2206    -0.0904
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_3]         -0.1319    -0.0144
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_4]         -0.1502    -0.0343
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_5]         -0.1828    -0.0383
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_1]         -0.2357    -0.1043
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_2]         -0.2494    -0.1169
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_3]         -0.1740    -0.0527
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_4]         -0.1103    -0.0114
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_5]         -0.1013     0.0047
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_1]         -0.1583    -0.0296
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_2]         -0.2067    -0.0706
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_3]         -0.3359    -0.1650
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_4]         -0.1876    -0.0425
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_5]         -0.2216    -0.0687
    ## b[(Intercept) SiteCodevisit:PRPND009_1]         -0.1023     0.0010
    ## b[(Intercept) SiteCodevisit:PRPND009_2]         -0.1343    -0.0327
    ## b[(Intercept) SiteCodevisit:PRPND009_3]         -0.0802     0.0065
    ## b[(Intercept) SiteCodevisit:PRPND009_4]         -0.1716    -0.0723
    ## b[(Intercept) SiteCodevisit:PRPND009_5]         -0.2563    -0.1303
    ## b[(Intercept) SiteCodevisit:PRPND010_1]         -0.2388    -0.0952
    ## b[(Intercept) SiteCodevisit:PRPND010_2]         -0.1981    -0.0695
    ## b[(Intercept) SiteCodevisit:PRPND010_3]         -0.1646    -0.0486
    ## b[(Intercept) SiteCodevisit:PRPND010_4]         -0.1933    -0.0791
    ## b[(Intercept) SiteCodevisit:PRPND010_5]         -0.0884     0.0133
    ## b[(Intercept) SiteCodevisit:PRPND014_1]         -0.1642    -0.0444
    ## b[(Intercept) SiteCodevisit:PRPND014_2]         -0.1837    -0.0524
    ## b[(Intercept) SiteCodevisit:PRPND014_3]         -0.2863    -0.1407
    ## b[(Intercept) SiteCodevisit:PRPND014_4]         -0.2582    -0.1301
    ## b[(Intercept) SiteCodevisit:PRPND014_5]         -0.0256     0.0832
    ## b[(Intercept) SiteCodevisit:PRPND015_1]         -0.2000    -0.0584
    ## b[(Intercept) SiteCodevisit:PRPND015_2]         -0.2828    -0.1290
    ## b[(Intercept) SiteCodevisit:PRPND015_3]         -0.1478    -0.0244
    ## b[(Intercept) SiteCodevisit:TGIF_1]             -0.2449    -0.0982
    ## b[(Intercept) SiteCodevisit:TGIF_2]             -0.1396    -0.0171
    ## b[(Intercept) SiteCodevisit:TGIF_3]             -0.1653    -0.0492
    ## b[(Intercept) SiteCodevisit:TGIF_4]             -0.1628    -0.0495
    ## b[(Intercept) SiteCodevisit:TGIF_5]             -0.2014    -0.0733
    ## b[(Intercept) SiteCode:PRNTH1]                   0.1327     0.3987
    ## b[(Intercept) SiteCode:PRNTH4]                  -0.7171    -0.4358
    ## b[(Intercept) SiteCode:PRNTHIDK]                -0.3006    -0.0185
    ## b[(Intercept) SiteCode:PRNTHMIT]                -0.1464     0.1281
    ## b[(Intercept) SiteCode:PRNTHOWL]                -1.4445    -1.1342
    ## b[(Intercept) SiteCode:PRPND009]                 0.3536     0.6186
    ## b[(Intercept) SiteCode:PRPND010]                -0.3004    -0.0302
    ## b[(Intercept) SiteCode:PRPND014]                -0.2210     0.0591
    ## b[(Intercept) SiteCode:PRPND015]                -1.0327    -0.7256
    ## b[(Intercept) SiteCode:TGIF]                    -0.4565    -0.1741
    ## Sigma[SiteCodevisit:(Intercept),(Intercept)]     0.0023     0.0073
    ## Sigma[SiteCode:(Intercept),(Intercept)]          0.1249     0.2135
    ## mean_PPD                                         3.5829     3.6945
    ## log-posterior                                -1453.6469 -1442.5849
    ##                                                50%        75%     
    ## (Intercept)                                      0.2159     0.3624
    ## visit                                            0.6912     0.7517
    ## SpeciesCode                                     -0.4427    -0.3073
    ## I(visit^2)                                      -0.0756    -0.0656
    ## visitScaledSVL                                   0.2975     0.3030
    ## visit:SpeciesCode                               -1.0063    -0.9445
    ## b[(Intercept) SiteCodevisit:PRNTH1_1]            0.0092     0.0688
    ## b[(Intercept) SiteCodevisit:PRNTH1_2]            0.0426     0.0987
    ## b[(Intercept) SiteCodevisit:PRNTH1_3]            0.1372     0.1916
    ## b[(Intercept) SiteCodevisit:PRNTH1_4]           -0.0901    -0.0417
    ## b[(Intercept) SiteCodevisit:PRNTH1_5]           -0.0827    -0.0288
    ## b[(Intercept) SiteCodevisit:PRNTH4_1]            0.0262     0.0868
    ## b[(Intercept) SiteCodevisit:PRNTH4_2]            0.0499     0.1110
    ## b[(Intercept) SiteCodevisit:PRNTH4_3]            0.0084     0.0653
    ## b[(Intercept) SiteCodevisit:PRNTH4_4]           -0.0298     0.0186
    ## b[(Intercept) SiteCodevisit:PRNTH4_5]           -0.0686    -0.0112
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_1]         -0.0481     0.0109
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_2]         -0.0316     0.0228
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_3]          0.0405     0.0994
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_4]          0.0200     0.0743
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_5]          0.0283     0.0976
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_1]         -0.0385     0.0195
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_2]         -0.0513     0.0027
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_3]          0.0029     0.0600
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_4]          0.0349     0.0888
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_5]          0.0598     0.1188
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_1]          0.0307     0.0989
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_2]         -0.0038     0.0560
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_3]         -0.0873    -0.0202
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_4]          0.0209     0.0871
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_5]         -0.0007     0.0670
    ## b[(Intercept) SiteCodevisit:PRPND009_1]          0.0560     0.1163
    ## b[(Intercept) SiteCodevisit:PRPND009_2]          0.0189     0.0703
    ## b[(Intercept) SiteCodevisit:PRPND009_3]          0.0546     0.1051
    ## b[(Intercept) SiteCodevisit:PRPND009_4]         -0.0249     0.0205
    ## b[(Intercept) SiteCodevisit:PRPND009_5]         -0.0715    -0.0209
    ## b[(Intercept) SiteCodevisit:PRPND010_1]         -0.0327     0.0259
    ## b[(Intercept) SiteCodevisit:PRPND010_2]         -0.0150     0.0374
    ## b[(Intercept) SiteCodevisit:PRPND010_3]          0.0061     0.0637
    ## b[(Intercept) SiteCodevisit:PRPND010_4]         -0.0207     0.0309
    ## b[(Intercept) SiteCodevisit:PRPND010_5]          0.0708     0.1274
    ## b[(Intercept) SiteCodevisit:PRPND014_1]          0.0162     0.0751
    ## b[(Intercept) SiteCodevisit:PRPND014_2]          0.0030     0.0649
    ## b[(Intercept) SiteCodevisit:PRPND014_3]         -0.0727    -0.0185
    ## b[(Intercept) SiteCodevisit:PRPND014_4]         -0.0698    -0.0157
    ## b[(Intercept) SiteCodevisit:PRPND014_5]          0.1410     0.2080
    ## b[(Intercept) SiteCodevisit:PRPND015_1]          0.0051     0.0639
    ## b[(Intercept) SiteCodevisit:PRPND015_2]         -0.0629     0.0017
    ## b[(Intercept) SiteCodevisit:PRPND015_3]          0.0346     0.0997
    ## b[(Intercept) SiteCodevisit:TGIF_1]             -0.0345     0.0253
    ## b[(Intercept) SiteCodevisit:TGIF_2]              0.0398     0.1005
    ## b[(Intercept) SiteCodevisit:TGIF_3]              0.0072     0.0615
    ## b[(Intercept) SiteCodevisit:TGIF_4]              0.0037     0.0604
    ## b[(Intercept) SiteCodevisit:TGIF_5]             -0.0161     0.0397
    ## b[(Intercept) SiteCode:PRNTH1]                   0.5286     0.6625
    ## b[(Intercept) SiteCode:PRNTH4]                  -0.3019    -0.1719
    ## b[(Intercept) SiteCode:PRNTHIDK]                 0.1138     0.2420
    ## b[(Intercept) SiteCode:PRNTHMIT]                 0.2564     0.3845
    ## b[(Intercept) SiteCode:PRNTHOWL]                -0.9869    -0.8516
    ## b[(Intercept) SiteCode:PRPND009]                 0.7465     0.8720
    ## b[(Intercept) SiteCode:PRPND010]                 0.1009     0.2295
    ## b[(Intercept) SiteCode:PRPND014]                 0.1934     0.3242
    ## b[(Intercept) SiteCode:PRPND015]                -0.5766    -0.4375
    ## b[(Intercept) SiteCode:TGIF]                    -0.0361     0.0881
    ## Sigma[SiteCodevisit:(Intercept),(Intercept)]     0.0109     0.0154
    ## Sigma[SiteCode:(Intercept),(Intercept)]          0.3005     0.4222
    ## mean_PPD                                         3.7560     3.8154
    ## log-posterior                                -1437.7894 -1432.8075
    ##                                                97.5%   
    ## (Intercept)                                      0.6691
    ## visit                                            0.8711
    ## SpeciesCode                                     -0.0432
    ## I(visit^2)                                      -0.0442
    ## visitScaledSVL                                   0.3134
    ## visit:SpeciesCode                               -0.8318
    ## b[(Intercept) SiteCodevisit:PRNTH1_1]            0.1963
    ## b[(Intercept) SiteCodevisit:PRNTH1_2]            0.2155
    ## b[(Intercept) SiteCodevisit:PRNTH1_3]            0.3108
    ## b[(Intercept) SiteCodevisit:PRNTH1_4]            0.0478
    ## b[(Intercept) SiteCodevisit:PRNTH1_5]            0.0668
    ## b[(Intercept) SiteCodevisit:PRNTH4_1]            0.2223
    ## b[(Intercept) SiteCodevisit:PRNTH4_2]            0.2443
    ## b[(Intercept) SiteCodevisit:PRNTH4_3]            0.1852
    ## b[(Intercept) SiteCodevisit:PRNTH4_4]            0.1193
    ## b[(Intercept) SiteCodevisit:PRNTH4_5]            0.0889
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_1]          0.1304
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_2]          0.1383
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_3]          0.2333
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_4]          0.1964
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_5]          0.2483
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_1]          0.1393
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_2]          0.1123
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_3]          0.1847
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_4]          0.1958
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_5]          0.2431
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_1]          0.2441
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_2]          0.1946
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_3]          0.0889
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_4]          0.2413
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_5]          0.2211
    ## b[(Intercept) SiteCodevisit:PRPND009_1]          0.2517
    ## b[(Intercept) SiteCodevisit:PRPND009_2]          0.1785
    ## b[(Intercept) SiteCodevisit:PRPND009_3]          0.2069
    ## b[(Intercept) SiteCodevisit:PRPND009_4]          0.1178
    ## b[(Intercept) SiteCodevisit:PRPND009_5]          0.0806
    ## b[(Intercept) SiteCodevisit:PRPND010_1]          0.1490
    ## b[(Intercept) SiteCodevisit:PRPND010_2]          0.1618
    ## b[(Intercept) SiteCodevisit:PRPND010_3]          0.1926
    ## b[(Intercept) SiteCodevisit:PRPND010_4]          0.1417
    ## b[(Intercept) SiteCodevisit:PRPND010_5]          0.2572
    ## b[(Intercept) SiteCodevisit:PRPND014_1]          0.1952
    ## b[(Intercept) SiteCodevisit:PRPND014_2]          0.1881
    ## b[(Intercept) SiteCodevisit:PRPND014_3]          0.0970
    ## b[(Intercept) SiteCodevisit:PRPND014_4]          0.0860
    ## b[(Intercept) SiteCodevisit:PRPND014_5]          0.3572
    ## b[(Intercept) SiteCodevisit:PRPND015_1]          0.2027
    ## b[(Intercept) SiteCodevisit:PRPND015_2]          0.1166
    ## b[(Intercept) SiteCodevisit:PRPND015_3]          0.2386
    ## b[(Intercept) SiteCodevisit:TGIF_1]              0.1435
    ## b[(Intercept) SiteCodevisit:TGIF_2]              0.2438
    ## b[(Intercept) SiteCodevisit:TGIF_3]              0.1740
    ## b[(Intercept) SiteCodevisit:TGIF_4]              0.1763
    ## b[(Intercept) SiteCodevisit:TGIF_5]              0.1586
    ## b[(Intercept) SiteCode:PRNTH1]                   0.9127
    ## b[(Intercept) SiteCode:PRNTH4]                   0.0728
    ## b[(Intercept) SiteCode:PRNTHIDK]                 0.5103
    ## b[(Intercept) SiteCode:PRNTHMIT]                 0.6435
    ## b[(Intercept) SiteCode:PRNTHOWL]                -0.5520
    ## b[(Intercept) SiteCode:PRPND009]                 1.1361
    ## b[(Intercept) SiteCode:PRPND010]                 0.5043
    ## b[(Intercept) SiteCode:PRPND014]                 0.5891
    ## b[(Intercept) SiteCode:PRPND015]                -0.1622
    ## b[(Intercept) SiteCode:TGIF]                     0.3514
    ## Sigma[SiteCodevisit:(Intercept),(Intercept)]     0.0284
    ## Sigma[SiteCode:(Intercept),(Intercept)]          0.9193
    ## mean_PPD                                         3.9270
    ## log-posterior                                -1424.5811
    ## 
    ## Diagnostics:
    ##                                              mcse   Rhat   n_eff
    ## (Intercept)                                  0.0070 1.0036 1068 
    ## visit                                        0.0021 1.0010 2027 
    ## SpeciesCode                                  0.0036 0.9996 3290 
    ## I(visit^2)                                   0.0003 1.0008 2013 
    ## visitScaledSVL                               0.0001 0.9997 5927 
    ## visit:SpeciesCode                            0.0016 0.9999 3362 
    ## b[(Intercept) SiteCodevisit:PRNTH1_1]        0.0013 0.9995 5379 
    ## b[(Intercept) SiteCodevisit:PRNTH1_2]        0.0011 0.9998 5100 
    ## b[(Intercept) SiteCodevisit:PRNTH1_3]        0.0015 1.0004 3043 
    ## b[(Intercept) SiteCodevisit:PRNTH1_4]        0.0012 1.0003 3796 
    ## b[(Intercept) SiteCodevisit:PRNTH1_5]        0.0014 0.9993 3828 
    ## b[(Intercept) SiteCodevisit:PRNTH4_1]        0.0012 0.9997 6910 
    ## b[(Intercept) SiteCodevisit:PRNTH4_2]        0.0012 0.9996 5578 
    ## b[(Intercept) SiteCodevisit:PRNTH4_3]        0.0012 0.9996 5288 
    ## b[(Intercept) SiteCodevisit:PRNTH4_4]        0.0011 1.0002 5097 
    ## b[(Intercept) SiteCodevisit:PRNTH4_5]        0.0014 1.0001 4268 
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_1]      0.0014 0.9993 4555 
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_2]      0.0012 0.9997 5802 
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_3]      0.0013 1.0000 5050 
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_4]      0.0011 1.0003 5940 
    ## b[(Intercept) SiteCodevisit:PRNTHIDK_5]      0.0013 0.9994 6794 
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_1]      0.0013 1.0005 5005 
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_2]      0.0012 1.0002 5765 
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_3]      0.0013 0.9999 4695 
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_4]      0.0011 1.0004 4587 
    ## b[(Intercept) SiteCodevisit:PRNTHMIT_5]      0.0012 0.9996 5327 
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_1]      0.0014 0.9993 5580 
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_2]      0.0013 1.0002 5861 
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_3]      0.0018 1.0014 3723 
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_4]      0.0014 0.9992 5421 
    ## b[(Intercept) SiteCodevisit:PRNTHOWL_5]      0.0014 0.9993 6589 
    ## b[(Intercept) SiteCodevisit:PRPND009_1]      0.0013 0.9995 5004 
    ## b[(Intercept) SiteCodevisit:PRPND009_2]      0.0011 0.9993 4805 
    ## b[(Intercept) SiteCodevisit:PRPND009_3]      0.0012 0.9993 3969 
    ## b[(Intercept) SiteCodevisit:PRPND009_4]      0.0011 0.9999 4678 
    ## b[(Intercept) SiteCodevisit:PRPND009_5]      0.0014 0.9999 3872 
    ## b[(Intercept) SiteCodevisit:PRPND010_1]      0.0012 0.9999 6320 
    ## b[(Intercept) SiteCodevisit:PRPND010_2]      0.0012 1.0001 5390 
    ## b[(Intercept) SiteCodevisit:PRPND010_3]      0.0012 1.0001 5171 
    ## b[(Intercept) SiteCodevisit:PRPND010_4]      0.0011 0.9999 5823 
    ## b[(Intercept) SiteCodevisit:PRPND010_5]      0.0013 1.0012 4570 
    ## b[(Intercept) SiteCodevisit:PRPND014_1]      0.0013 0.9996 5156 
    ## b[(Intercept) SiteCodevisit:PRPND014_2]      0.0012 0.9996 5503 
    ## b[(Intercept) SiteCodevisit:PRPND014_3]      0.0014 0.9995 4815 
    ## b[(Intercept) SiteCodevisit:PRPND014_4]      0.0012 0.9995 5045 
    ## b[(Intercept) SiteCodevisit:PRPND014_5]      0.0015 0.9994 3973 
    ## b[(Intercept) SiteCodevisit:PRPND015_1]      0.0013 0.9997 5593 
    ## b[(Intercept) SiteCodevisit:PRPND015_2]      0.0015 1.0017 4883 
    ## b[(Intercept) SiteCodevisit:PRPND015_3]      0.0013 0.9998 5253 
    ## b[(Intercept) SiteCodevisit:TGIF_1]          0.0015 0.9997 4417 
    ## b[(Intercept) SiteCodevisit:TGIF_2]          0.0013 1.0000 5018 
    ## b[(Intercept) SiteCodevisit:TGIF_3]          0.0011 0.9999 5812 
    ## b[(Intercept) SiteCodevisit:TGIF_4]          0.0011 0.9997 5738 
    ## b[(Intercept) SiteCodevisit:TGIF_5]          0.0012 0.9997 5477 
    ## b[(Intercept) SiteCode:PRNTH1]               0.0065 1.0043  936 
    ## b[(Intercept) SiteCode:PRNTH4]               0.0070 1.0041  828 
    ## b[(Intercept) SiteCode:PRNTHIDK]             0.0066 1.0050  962 
    ## b[(Intercept) SiteCode:PRNTHMIT]             0.0066 1.0055  919 
    ## b[(Intercept) SiteCode:PRNTHOWL]             0.0067 1.0033 1076 
    ## b[(Intercept) SiteCode:PRPND009]             0.0065 1.0047  915 
    ## b[(Intercept) SiteCode:PRPND010]             0.0068 1.0057  911 
    ## b[(Intercept) SiteCode:PRPND014]             0.0066 1.0044  972 
    ## b[(Intercept) SiteCode:PRPND015]             0.0069 1.0036 1044 
    ## b[(Intercept) SiteCode:TGIF]                 0.0069 1.0043  852 
    ## Sigma[SiteCodevisit:(Intercept),(Intercept)] 0.0002 1.0014 1495 
    ## Sigma[SiteCode:(Intercept),(Intercept)]      0.0057 1.0003 1587 
    ## mean_PPD                                     0.0014 1.0003 3894 
    ## log-posterior                                0.2656 1.0041  774 
    ## 
    ## For each parameter, mcse is Monte Carlo standard error, n_eff is a crude measure of effective sample size, and Rhat is the potential scale reduction factor on split chains (at convergence Rhat=1).

``` r
Effects$EstimatedValue <- as.vector(stan.fit.sim3$coefficients[2:6])
Effects$EstimatedValueSE <- stan.fit.sim3$ses[2:6]

rand.eff.sim3.sc <- coefficients(stan.fit.sim3)$SiteCode
rand.eff.sim3.sv <- coefficients(stan.fit.sim3)$SiteCodevisit

## site level RE
{plot(rand.eff.sim3.sc[,1]~site.means.gamma, xlab = "Actual Random Effects", ylab = "Estimated Random Effects", main = "Site level random effects")
abline(0,1)}  # looks pretty good! 
```

![](ip_5_simulations_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
ggplot(Effects)+
  geom_hline(yintercept = 0, linetype = "dashed", col = "black")+
  geom_point(aes(x=FixedEffect, y = EstimatedValue, colour = "EstimatedValue"), shape = 16, size = 2)+
  geom_errorbar(aes(x=FixedEffect,ymin = EstimatedValue - 2*EstimatedValueSE, ymax = EstimatedValue + 2*EstimatedValueSE, colour = "EstimatedValue"), width = .1)+ 
  geom_point(aes(x=FixedEffect, y = TrueValue, colour = "TrueValue"), shape = 95, size = 10)+
  scale_colour_manual(name = "", values = c(EstimatedValue="dodgerblue", TrueValue = "red"), labels = c("Estimated Value", "True Value"))+
  xlab("Parameter")+
  ylab("Coefficient Value")+
  theme(legend.position = "bottom")
```

![](ip_5_simulations_files/figure-markdown_github/unnamed-chunk-16-2.png)

Overall impressions
-------------------

The model is pretty good at estimating fixed effects and the site-level random effects. I'm not really sure how to plot the site-visit random effects since they are a combination of fixed predictors and random effects. However, I think this output confirms that the model is doing a pretty decent job at estimation. I haven't played around with standard deviations much to see how this impacts estimation, since the model takes so long to fit.
