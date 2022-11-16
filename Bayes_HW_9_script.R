#####3Homework 9 for bayesian class
##Alexandra Reich

#set things up
summary(mtcars) #the dataset
names(mtcars)


#problem 1
#let's center some variables
#1a
mtcars$cwt <- mtcars$wt - mean(mtcars$wt)
mtcars$cqsec <- mtcars$qsec - mean(mtcars$qsec)
mod1 <- lm(mpg ~ cwt + cqsec, data = mtcars)
mod2 <- lm( mpg ~ cwt * cqsec, data = mtcars )
summary(mod1)
summary(mod2)

#1b
AIC(mod1, mod2)

##############3
##Problem 2 #hmmm. see pg 201
#2a
#use JAGS to fit Model 2
library(rjags)

#use JAGS to fit Model 1
num.betas <- 3
beta.prior.mean <- rep(0.0, num.betas) # p. 217 code
beta.prior.precision <- 1/20
sigma.min <- 0
sigma.max <- 10^5
my.data <- list(
  y=mtcars$mpg, cwt = mtcars$cwt, 
  cqsec = mtcars$cqsec,
  N=nrow(mtcars),
  beta.prior.mean = beta.prior.mean,
  beta.prior.precision = beta.prior.precision,
  sigma.min = sigma.min, sigma.max=sigma.max
)

my.inits = list(beta=rep(0.0, num.betas), sigma=1.0)#,
 # list(beta=rep(0.0, num.betas), sigma=1.0),
  #list(beta=rep(0.0, num.betas), sigma=1.0)
#)

my.fname <- "mod_statement_hw9_1"
my.jags.mod1 <- jags.model(
  file=my.fname, data=my.data, inits= my.inits,
  n.adapt=1000, quiet=FALSE, n.chains=1
)

