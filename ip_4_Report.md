The data
--------

SUMMARY OF DATASET: I quantified parasite communities across 10
different pond sites in the East Bay of California; each site was
visited 4-6 times within the 2017 summer. At each visit I collected
10-12 individuals from 2 amphibian species. Individuals were measured
and parasite infection was quantified.

GROUPING VARIABLES: \* Site (10 ponds) - this is a random variable. \*
Population (2 species at each site) - this is a random variable

PREDICTOR VARIABLES:

-   Species (2 species) - I think this is a better designation for
    species

-   Body size (snout-vent-length) - A continuous variable (fixed effect)
    at the individual level

-   Developmental stage - A continuous variable at the individual
    level...problematic since development is measured differently in
    newts vs. frogs

-   Sex - A factor variable at the individual level

-   Visit - A continuous variable (though could be treated as a factor
    if the relationship si non linear)

RESPONSE VARIABLE:

-   The number of parasites found within an individual. For this
    analysis, just count number of Echinostoma parasites. Can be modeled
    as Poisson distributed with a negative binomial to account for
    aggregation. Infection status (1 or 0) could also be modeled as a
    binary response variable using logistic regression.

![Experimental Design](%22ip_fig%20design.jpg%22)

Questions
---------

1.  Does the impact of species and body size change over the course of
    the summer (interact with visit)?
2.  How much of the variation in overall parasite load is explained by
    visit-level, species-level, site-level, or individual-level
    variation?

Display the structure of the data
---------------------------------

    dis <- read.csv("diss.data.2017.csv")
    # colnames(dis)
    str(dis)

    ## 'data.frame':    1049 obs. of  35 variables:
    ##  $ X                          : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ HostCode                   : Factor w/ 1049 levels "PRNTH1_20170328_PSRE_001",..: 113 872 340 755 1 230 231 866 870 346 ...
    ##  $ Date                       : int  20170513 20170328 20170607 20170327 20170328 20170513 20170513 20170328 20170328 20170607 ...
    ##  $ SiteCode                   : Factor w/ 10 levels "PRNTH1","PRNTH4",..: 2 9 4 8 1 3 3 9 9 4 ...
    ##  $ SpeciesCode                : Factor w/ 2 levels "PSRE","TATO": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ CollectionCode             : Factor w/ 53 levels "PRNTH1_20170328",..: 7 45 18 39 1 12 12 45 45 18 ...
    ##  $ Lifestage                  : Factor w/ 2 levels "Larva","Metamorph": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Dissector                  : Factor w/ 5 levels "AO","CM","DC",..: 2 3 2 3 3 2 2 3 3 2 ...
    ##  $ DissectionCondition        : Factor w/ 3 levels "Dead on Arrival",..: 3 3 3 1 3 3 3 3 3 2 ...
    ##  $ GosnerStage                : int  40 26 26 26 26 29 26 26 26 26 ...
    ##  $ TarichaLarvaeStage         : Factor w/ 6 levels "","2T","3T","4T",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ SVL                        : num  4.58 5.13 5.29 5.41 5.42 5.45 5.6 5.74 5.94 5.95 ...
    ##  $ TailLength                 : num  27.44 6.44 5.76 6.78 7.15 ...
    ##  $ TotalLength                : num  32 11.6 11.1 12.2 12.6 ...
    ##  $ Malformed                  : Factor w/ 2 levels "N","Y": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Sex                        : Factor w/ 3 levels "Female","Male",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ collDate                   : Factor w/ 13 levels "2017-03-27","2017-03-28",..: 3 2 7 1 2 3 3 2 2 7 ...
    ##  $ visit                      : int  2 1 3 1 1 2 2 1 1 3 ...
    ##  $ SecYr                      : logi  NA NA NA NA NA NA ...
    ##  $ tot.para                   : int  2 1 2 0 0 4 0 1 1 2 ...
    ##  $ BDinf                      : int  1 0 0 0 0 0 0 1 1 0 ...
    ##  $ aveZE                      : num  2.75 0 0 0 0 ...
    ##  $ Alaria                     : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Cephalogonimus             : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Echinostoma                : int  0 0 0 0 0 2 0 0 0 1 ...
    ##  $ Gorgoderid_Metacercaria    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Gyrinicola_batrachiensis   : int  0 0 0 0 0 3 0 0 0 0 ...
    ##  $ Manodistomum_syntomentera  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Megalobatrachonema_moraveci: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Nematode                   : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Oxyurid                    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Ribeiroia_ondatrae         : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Nyctotherus                : int  1 1 0 0 0 1 0 0 1 0 ...
    ##  $ Opalina                    : int  1 0 1 0 0 1 0 1 0 0 ...
    ##  $ Tritrichomonas             : int  0 0 1 0 0 0 0 0 0 1 ...

    dis$Pop <- paste(dis$SiteCode, dis$SpeciesCode, sep = "_") # visits are nested by the

Model formulation
-----------------

For now, I'll focus on question 1 with the response variable being
`Echinostoma` (number of Echinostoma parasites found within that
individual)

### Formulation 1

This is the model formulation for a random intercept model where `visit`
is treated as a fixed effect (perhaps using julian date which is
continuous)

*y*<sub>*i*</sub> ~ NB(*μ*<sub>*j*\[*i*\]</sub>, *p*)

*μ*<sub>*j*\[*i*\]</sub> is the rate (mean) and *p* is the
overdispersion parameter (since we can't let mean equal variance with
overdispersed data).

I'm not sure if this equation is used, or whether *p* is just modeled
like any other parameter with maximum likelihood: Var(*y*<sub>*i*</sub>)
~ *r*<sub>*i*</sub> + *r*<sub>*i*</sub><sup>2</sup>/*p* (Allows variance
to increase with mean)

log(*μ*<sub>*j*\[*i*\]</sub>) = *α*<sub>*i*\[*j*\]</sub>
+*β*<sub>1</sub>SpeciesCode + *β*<sub>2</sub>visit +
$\\beta\_3}$(SpeciesCode x visit) + *β*<sub>4</sub>SVL +
*β*<sub>5</sub>(SVL x Visit)

*α*<sub>*i*\[*j*\]</sub> ~ Normal(*μ*<sub>*α*</sub>,
*σ*<sub>*α*</sub><sup>2</sup>) This is between pond variance. J's are
ponds (10 total)

### Formulation 2

This is the model formulation for a random slope + random intercept
model where visit is treated as a fixed effect

*y*<sub>*i*</sub> ~ NB(*μ*<sub>*i*</sub>, *p*)

log(*μ*<sub>*i*\[*j*\]</sub>) = *α*<sub>*i*\[*j*\]</sub>
+*β*<sub>1</sub>SpeciesCode + *β*<sub>2</sub>(SpeciesCode x Visit) +
*β*<sub>3</sub>SVL + *β*<sub>4</sub>(SVL x Visit) +
*β*<sub>*i*\[*j*\]</sub>visit

*α*<sub>*i*\[*j*\]</sub> ~ Normal(*μ*<sub>*α*</sub>,
*σ*<sub>*α*</sub><sup>2</sup>)

*β*<sub>*i*\[*j*\]</sub> ~ Normal(*μ*<sub>*β*</sub>, ???)

Now, I'm not sure how these sigmas work since the random slopes and
intercepts are correlated...

### Formulation 3

Instead of doing NB, can I have an observation-level random effect which
accounts for dispersion?

*y*<sub>*i**j*</sub> ~ NB(*μ*<sub>*i*</sub>, *p*)

log(*μ*<sub>*i*\[*j*\]</sub>) = *α*<sub>*i*</sub>
+*β*<sub>1</sub>SpeciesCode + *β*<sub>2</sub>(SpeciesCode x Visit) +
*β*<sub>3</sub>SVL + *β*<sub>4</sub>(SVL x Visit) +
*β*<sub>*i*\[*j*\]</sub>visit

Individual level observations are modeled as a random effect, coming
with some distribution centered around the mean for that
individual...???

*α*<sub>*i*</sub> ~ Normal(*α*<sub>*i*\[*j*\]</sub>,
*σ*<sub>*α*</sub><sup>2</sup>) What's the sigma here?

*α*<sub>*i*\[*j*\]</sub> ~ Normal(*μ*<sub>*j*</sub>,
*σ*<sub>*α*</sub><sup>2</sup>) Each pond level alpha comes from the mean
over all the ponds

*β*<sub>*i*\[*j*\]</sub> ~ Normal(*μ*<sub>*β*</sub>, ???)

I confused myself...

### Formulation 4

I'm trying to incorporate Brett's feedback about using visit as another
nesting (although I'm still really confused on nesting vs. crossing.
Since every site has the same 6 visits (and they're done within a day of
each other), the visits don't seem unique to each site?)

It makes sense to me to include both as random effects, but I am
wondering if the effect of pond differs by visit. I think ponds get a
little more similar over time.

I think the model formula I'm looking for would be something like:
Echinostoma ~ (visit|SpeciesCode) + (visit|SVL) + (1|visit:SiteCode)

But does this let me ask questions like: does the effect of species
lessen over time? Ultimately that's what I'm interested in: how the
impact of predictors change over the summer (I think that stochastic
factors dominate early on).

Also, can random effects interact with each other? Am I overthinking
this completely???

Stan formulation
----------------

In stan\_glm, the model could be written as:

    stan.fit <- stan_glmer(Echinostoma ~ visit*SpeciesCode + visit*SVL + (1|SiteCode) + (1|Pop) +
                             (1|HostCode), data = dis,  family =poisson(link="log"))
    summary(stan.fit)
    launch_shinystan(stan.fit)
    stan.samp <- sample(stan.fit)
    save(stan.samp, file = "stan.fit.1.RData")

Next steps: incorporate Brett's suggestions. Treat visit as a random
effect (there are two levels of grouping), but perhaps have some other
metric of visit (Julian date) as a fixed effect allowing it to interact
with other variables to affect parasite count. Developmental stages
could perhaps be included with an interaction with species, which would
allow TATO to have some levels and PSRE to have other levels. e.g. I
need to put them in the same column. Instead of using stan's negative
binomial (which seems a bit problematic according to googling) I can
include individual as a random effect.

-   Will autocorrelation of fixed and random effects become an issue?
