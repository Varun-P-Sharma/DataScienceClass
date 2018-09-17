# Chapter 4 McElreath

### R setup for Chapter 4 ------
library(rethinking)
# set up a posterior sampling algorithm (instead of map)
sampost <- function(flist, data, n = 10000) {
  quadapprox <- map(flist, data)
  posterior_sample <- extract.samples(quadapprox, n)
  return(posterior_sample)
}
# but this uses map? 

### Example 1: Height ----------
data(Howell1)
d <- Howell1
head(d)
d2 <- d[ d$age >= 18 , ]
# plot the prior for mu
curve(dnorm(x, 178, 20), from = 100, to = 250)
# plot the prior for sigma
curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )
# get a list of possible values of mu
mu.list<-seq(from=140, to=160, length.out=800)
# get a list of possible values of sigma
sigma.list<-seq(from=4, to=9, length.out=800)
# get a table of every possible combination of mu and sigma
post<-expand.grid(mu=mu.list, sigma=sigma.list)
# generate the posterior likelihood 
post$LL<-sapply(1:nrow(post), function(i) sum(dnorm(
  d2$height, 
  mean=post$mu[i],
  sd=post$sigma[i],
  log=TRUE))) #getting the log likelihood for each data point, for each possible combo of mu and sigmas
# dnorm gives the probability of X given the mean and sd
# this function sums the probability of each value of height, given the mean and sigma from the list
# what is difference about log likelihood and probability? 
# multiply the prior and the likelihood (priors are on a log scale (really?) so we can add them)
post$prod<-post$LL + dnorm(post$mu, 178, 20, TRUE)+ dunif(post$sigma, 0, 50, TRUE) 
post$prob<-exp(post$prod-max(post$prod)) #scale the log proucts to teh maximum...not sure why
contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post$mu, post$sigma, post$prob)
#sample the posterior
sample.rows<-sample(1:nrow(post), size=1e4, replace=TRUE, prob=post$prob)
sample.mu<-post$mu[sample.rows]
sample.sigma<-post$sigma[sample.rows]
plot(sample.mu, sample.sigma, cex=.5, pch=16, col=col.alpha(rangi2,0.1))
#describe the posterior
dens(sample.mu)
dens(sample.sigma)
HPDI(sample.mu)
HPDI(sample.sigma)

### Example 1: Height with Sampost Function------
flist <- alist(
  height ~ dnorm(mu, sigma), 
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)
m4.1 <- sampost(flist, data = d2)
precis(m4.1)
hist(m4.1$mu, breaks = 100, freq = FALSE, col = "lightblue")
hist(m4.1$sigma, breaks =100, freq = FALSE, col = "lightpink")

# let's try a more informative prior on mu
m4.2 <- sampost(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 0.1),
    sigma ~ dunif(0, 50)
  ), data = d2
)
precis(m4.2)
hist(m4.2$mu)


### Example 2: A linear regression model-----
m4.3 <- sampost(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
    a ~ dnorm(156, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0,50)
  ),
  data = d2
)
precis(m4.3); round(cor(m4.3), 2)

# centering variables to remove correlations among parameters
# I'm not really sure why this works...
d2$weight.c <- d2$weight - mean(d2$weight)
m4.4 <- sampost(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight.c,
    a ~ dnorm(156, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0,50)
  ),
  data = d2
)
precis(m4.4); round(cor(m4.4),2)
# plotting the model fit
plot(height~weight, data = d2)
abline(a = mean(m4.3[,"a"]), b = mean(m4.3[, "b"]))
plot(height~weight.c, data = d2)
# adding uncertainty

plot(height~weight.c, data = d2)
for(i in 1:100){
  abline(a = m4.4$a[i], b = m4.4$b[i], col = col.alpha("black", 0.3))
}

# plotting density around the best fit line
w <- seq (from = 25, to = 70, by = 1) # potential weights over which we want to plot best fit line
n <- length(w)
hdpi_m <- matrix(NA, nrow = n, ncol = 2) # matrix to store hdpi values
colnames(hdpi_m) <- c("low89", "high89")
for (i in 1:n){
  mu <- m4.3$a + m4.3$b*w[i] # the posterior sample of mu at weight i (for all posterior draws of a and b)
  hdpi_m[i, ] <- HPDI(mu, prob = 0.89)
}
hdpi_m
plot(height~weight, d2, col = "blue")
abline(a = mean(m4.3[, "a"]), b = mean(m4.3[,"b"]))
lines(w, hdpi_m[, "low89"], col = "grey")
lines(w, hdpi_m[, "high89"], col = "grey")

# prediction intervals 
w <- seq(from=25,to=70,by=1)
n <- length(w)
pred_hpdi_m <- matrix(NA,nrow=n,ncol=2)
colnames(pred_hpdi_m) <- c("low89","high89")
for(i in 1:n){
  mu <- m4.3$a + m4.3$b*w[i] #the posterior sample of mu at weight w 
  newdat <- rnorm(n = length(mu), mu , sd = m4.3$sigma) # pull an observation from each value of sigma
  pred_hpdi_m[i,] <- HPDI(newdat,prob=0.89) # hpdi of the sample 
  }
pred_hpdi_m

plot(height ~ weight,data=d2,col="blue")
abline(a=mean(m4.3[,"a"]),b=mean(m4.3[,"b"]))
lines(w,hdpi_m[,"low89"],col="grey")
lines(w,hdpi_m[,"high89"],col="grey")
lines(w,pred_hpdi_m[,"low89"],col="red")
lines(w,pred_hpdi_m[,"high89"],col="red")

# why are they even but the ci lines aren't? 