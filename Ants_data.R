library(ggplot2)
ants <- read.csv("ants.csv", as.is = TRUE)
head(ants)
ants
class(ants$site)
unique(ants$site)
ants$habitat <- factor(ants$habitat)
ants$site <- factor(ants$site)
# analysis plan
# plot the data

# think about mathematical model/transforming
plot(log(richness)~latitude, ants, col = as.factor(habitat), pch = 16)

# yi ~ dpois(lambdai)
# mu = exp(log(mu))
# log(lambda) = b0 + b1*forest + b2*latitude + b3*forest*latitude
# data are paired

# model: nu <- B0 + B1 * latitude + B2 * forest + B3*forest*latitude


### MAXIMUM LIKELIHOOD MODEL FIT------------
# fit the model
predictMat <- model.matrix(~habitat*latitude, data = ants)

poisReg <- function(params, X, y){ # use the vector math here
  k <- ncol(X) # number of predictors
  b <- params[1:k] # a vector of parameters up to the number of predictors
  g <- X%*%b # the linear model to predict the link var
  lambda <- exp(g)
  ll <- sum(dpois(y, lambda, log = TRUE))
  return(-ll)
}
ncol(predictMat)

fitant <- optim(c(1,1,1,1), poisReg, method = "BFGS", X = predictMat, y = ants$richness, hessian = T)

# explore model output
params <- fitant$par
names(params) <- c(colnames(predictMat))
cov_mat <- solve(fitant$hessian)
library(mvtnorm)
nsim <- 10000

forest.data <- data.frame(
  int = rep(1, 100),
  habitat = rep(1,100),
  latitude = seq(min(ants$latitude), max(ants$latitude), len=100)
)
forest.data$for.lat <- forest.data$habitat*forest.data$latitude

bog.data <- data.frame(
  int = rep(1,100),
  habitat = rep(0,100),
  latitude = seq(min(ants$latitude), max(ants$latitude), len=100),
  bog.lat = rep(0,100)
)
bog.data$Pred.Bog <- as.matrix(bog.data) %*% params
forest.data$Pred.For <- as.matrix(forest.data) %*% params
forest.data$Pred.For.rich <- exp(Pred.For)
bog.data$Pred.Bog.rich <- exp(Pred.Bog)

## plot the output 
plot(Pred.For.rich~seq(min(ants$latitude), max(ants$latitude), len =100), type = "l", ylim = c(0,14), xlim = c(40, 46), xlab = "latitude", ylab = "ant richness", col = "forestgreen", lwd = 2) 
points(Pred.Bog.rich~seq(min(ants$latitude), max(ants$latitude), len =100), 
       type = "l", col = "brown", lwd = 2)
points(richness~latitude, subset(ants, habitat == "forest"), col = "forestgreen", pch =16)
points(richness~latitude, subset(ants, habitat == "bog"), col = "brown", pch =16)

## try plotting with tidyverse
mycolors <- c("brown", "forestgreen")
ggplot()+
  geom_point(data = ants, mapping = aes(x=latitude, y = richness, colour = habitat))+
  geom_line(data = bog.data,colour="brown", aes(x= latitude ,y=Pred.Bog.rich))+
  geom_line(data = bog.data, colour = "forestgreen", aes(x=latitude, y = Pred.For.rich))+
  scale_color_manual(values = mycolors)

### Using the glm (frequentist) approach---------
ant.glm <- glm(richness~latitude*habitat, data=ants, family = "poisson")
summary(ant.glm)
confint(ant.glm) #confidence intervals for parameters
plot(ant.glm,1:6,ask=FALSE) #Diagnostic plots
cov2cor(vcov(ant.glm)) #Correlation matrix for parameters
logLik(ant.glm)  #The log likelihood

## get confidence intervals for plotting
newd <- data.frame(latitude = rep(seq(min(ants$latitude), max(ants$latitude),
                                length.out=100),2), 
                   habitat = factor(rep(c("bog","forest"),each=100)))
# For GLMs, there is no ' interval = "confidence" ' option, so we have to
# construct intervals from the standard errors. This is approximate. We use 
# 2 * s.e. here. More accurate intervals are obtained by parametric bootstrap.
# We will cover bootstrap later.
preds <- predict(ant.glm,newdata=newd,se.fit=TRUE)
mlp <- preds$fit         #mean of the linear predictor
selp <- preds$se.fit     #se of the linear predictor
cillp <- mlp - 2 * selp  #lower of 95% CI for linear predictor
ciulp <- mlp + 2 * selp  #upper
cilp <- exp(cillp)       #lower of 95% CI for response scale
ciup <- exp(ciulp)       #upper
mp <- exp(mlp)           #mean of response scale

preds <- cbind(newd,preds,cilp,ciup,mp)
preds

# in base plot
palette(mycolors)
plot(ants$latitude,ants$richness,pch=16, col=as.factor(ants$habitat), ylab= "richness", xlab = "habitat")

cat <- c("bog","forest")
for ( i in 1:2 ) {
  subd <- subset(preds,habitat==cat[i])
  lines(subd$latitude,subd$mp,lty=1, col = mycolors[i])
  lines(subd$latitude,subd$cilp,lty=2,col=mycolors[i])
  lines(subd$latitude,subd$ciup,lty=2,col=mycolors[i])
}

ggplot() +
  geom_ribbon(mapping=aes(x=latitude,ymin=cilp,ymax=ciup,fill=habitat),
              alpha=0.2,data=preds) +
  geom_point(mapping=aes(x=latitude,y=richness,col=habitat),data=ants) +
  geom_line(mapping=aes(x=latitude,y=mp,col=habitat),data=preds)

## Brett's code for plotting

# 
# 3 layers: a polygon geom for intervals
#           a line geom for estimated means
#           a point geom for the data
# Compared to ggplot, it is clear that we have to do extra subsetting and
# styling work to achieve the same result but base plot is very flexible if
# somewhat hacky and ad hoc. Since no packages were used, this plot will be
# highly reproducible over time.

# Bog & Forest colors
bfc <- c("#d95f02","#1b9e77")

# Coordinate system with scales (ticks and lines), and grey background
par(mar=c(5, 4, 2, 2) + 0.1)
plot(richness~latitude,ylim=c(0,18),type="n",axes=FALSE,ann=FALSE,data=ants)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col="grey92", border=NA) #grey background
axis(1,at=42:45,col=NA,col.ticks="black")
axis(2,col=NA,col.ticks="black",las=2)
abline(v=axTicks(1),col="white",lty=1)
abline(h=axTicks(2),col="white",lty=1)
# A polygon geom layer for intervals
h <- c("bog","forest")
for ( i in 1:2 ) {
  subd <- subset(preds,habitat==h[i])
  x <- subd$latitude
  ymin <- subd$cil
  ymax <- subd$ciu
  polygon(x=c(x,rev(x)),y=c(ymin,rev(ymax)),col=adjustcolor(bfc[i],alpha.f=0.2),border=NA)
}
# A line geom layer for means
for ( i in 1:2 ) {
  subd <- subset(preds,habitat==h[i])
  lines(mp~latitude,lty=1,col=bfc[i],data=subd)
}
# A points geom layer for the data
points(richness~latitude,pch=20,col=bfc[habitat],data=ants)
# Annotations
mtext("Latitude (degrees north)",1,line=2.5)

mtext(expression("Ant species richness"),2,line=2.5)
text(42.9,3.3,"Bog",col=bfc[1])
text(43.85,9.5,"Forest",col=bfc[2])
# that's a good looking plot right there!

# Model matrix demo
# A subset of the data used to demonstrate the model matrix (see ppt)
# Points can be indicated on graph with black outlines
antsub <- c(2,11,15,21,28,34,38,44)
antdemo <- ants[antsub,c(2,3,5)]
points(antdemo$latitude,antdemo$richness)
antdemo
mm <- model.matrix(ant.glm)
mm
mm[antsub,] #The subset for antdemo

## Using Bayesian approach--------------
library(rstan)
library(rstanarm)
theme_set(theme_grey())
fit_b <- stan_glm(richness ~ habitat + latitude + habitat:latitude,
                  family= poisson(link = "log"), data = ants)

samples <- extract(fit$stanfit)
class(samples)
str(samples)
names(samples)
hist(samples$beta[,1]) #e.g. histogram of \beta_1

# Convenience functions that estimate things from the samples
methods(class="stanreg") 
?coef.stanreg #see esp. rstanarm::stanreg-methods here
summary(fit_b,digits=4) #Estimates and standard errors, etc
launch_shinystan(fit_b) #Diagnostic plots
posterior_interval(fit_b,prob=0.95) #central posterior intervals, nb default=0.9
vcov(fit_b,correlation=TRUE) #Correlation matrix
# sample(fit_b)
# Plotting regression intervals (credible intervals and prediction intervals)
# Use the samples, as you learned from McElreath

library(tidyverse)
# the posterior
fits <- fit_b %>% 
  as_data_frame %>%
  rename(intercept = `(Intercept)`) 
# sample from the posterior
# for each sample, get the predicted line and plot it
hdpi.1 <- posterior_interval(fit_b, prob = 0.89)

newd.f <- data.frame(
  intercept = rep(1, 100),
  habitat = rep(1,100),
  latitude = seq(min(ants$latitude), max(ants$latitude), len=100)
)
newd.f$for.lat <- newd.f$habitat*newd.f$latitude

newd.b <- data.frame(
  intercept = rep(1, 100),
  habitat = rep(0,100),
  latitude = seq(min(ants$latitude), max(ants$latitude), len=100)
)
newd.b$b.lat <- newd.b$habitat*newd.b$latitude
params.b <- fit_b$coefficients %>% data.frame()
newd.b$best <- exp(as.matrix(newd.b[1:4]) %*% as.matrix(params.b))
newd.f$best <- exp(as.matrix(newd.f[1:4]) %*% as.matrix(params.b))

mus.f <- matrix(NA, nrow=100, ncol = 1000)
mus.b <- matrix(NA, nrow = 100, ncol = 1000)
lats = seq(min(ants$latitude), max(ants$latitude), len=100)
hdpi.mat.b <- matrix(NA, nrow = 100, ncol = 2)
hdpi.mat.f <- matrix(NA, nrow = 100, ncol = 2)

sample.post <- sample_n(fits, size = 1000) %>% data.frame()
for (i in 1:1000){
  mus.f[,i] <- exp(sample.post[i,1]+sample.post[i,2]+(sample.post[i,3]+sample.post[i,4])*lats)
  mus.b[,i] <- exp(sample.post[i,1]+sample.post[i,3]*lats)
}
library(rethinking)
for(i in 1:100){
  hdpi.mat.b[i,] <- HPDI(mus.b[i,])
  hdpi.mat.f[i,] <- HPDI(mus.f[i,])
}

plot(richness~latitude, data = ants, pch = 16, col = as.factor(habitat), ylim = c(0,21), type = "n")
for(i in 1:1000){
  params <- sample_n(fits,1) %>% data.frame()
  newd.f$Pred.r <- exp(as.matrix(newd.f[1:4]) %*% t(params))
  newd.b$Pred.r <- exp(as.matrix(newd.b[1:4]) %*% t(params))
  lines(y = newd.f$Pred.r, x = newd.f$latitude, lwd = .2, col = alpha("forestgreen",.1))
  lines(y = newd.b$Pred.r, x = newd.b$latitude, lwd = .2, col = alpha("brown",.1))}

lines(y = hdpi.mat.b[,1], x=lats, lwd =2, lty =2, col = "saddlebrown")
lines(y = hdpi.mat.b[,2], x=lats, lwd =2, lty =2, col = "saddlebrown")
lines(y = newd.b$best, x=lats, lwd =2,  col = "saddlebrown")
lines(y = hdpi.mat.f[,1], x=lats, lwd =2, lty =2, col = "darkgreen")
lines(y = hdpi.mat.f[,2], x=lats, lwd =2, lty =2, col = "darkgreen")
lines(y = newd.f$best, x=lats, lwd =2, col = "darkgreen")
points(richness~latitude, pch=21, bg = as.factor(habitat), col = "black", ants)


# try with ggplot
hdpi.f <- hdpi.mat.f %>% data.frame() %>% mutate(lats = lats)
hdpi.b <- hdpi.mat.b %>% data.frame() %>% mutate(lats = lats)

ggplot()+
  geom_ribbon(mapping=aes(x=lats,ymin=X1,ymax=X2),
              alpha=0.2,fill = "forestgreen", data=hdpi.f)+
  geom_ribbon(mapping=aes(x=lats,ymin=X1,ymax=X2),
              alpha=0.2,fill = "brown", data=hdpi.b)+
  geom_line(data = newd.f, mapping = aes(x=latitude, y = best), color="forestgreen")+
  geom_line(data = newd.b, mapping = aes(x=latitude, y = best), color="brown")+
  geom_point(data = ants, aes(x=latitude, y = richness, col = habitat))+
  scale_color_manual(values =mycolors)+
  guides(color=guide_legend(title="Habitat Type"))+
  xlab("Latitude") + ylab("Species richness")

