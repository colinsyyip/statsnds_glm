pkgs <- rownames(installed.packages())
if(!"faraway" %in% pkgs) install.packages("faraway")
if(!"MASS" %in% pkgs) install.packages("MASS")
if(!"leaps" %in% pkgs) install.packages("leaps")
if(!"MuMIn" %in% pkgs) install.packages("MuMIn")
if(!"ISLR" %in% pkgs) install.packages("ISLR")
if(!"corrplot" %in% pkgs) install.packages("corrplot")
if(!"caret" %in% pkgs) install.packages("caret")
if(!"stats" %in% pkgs) install.packages("stats")

library(faraway)
library(MASS)
library(leaps)
library(MuMIn)
library(ISLR)
library(corrplot)
library(caret)
library(stats)

y <- c(2,0,3,1,3,2,4,1,3,2)
mean(y)
# We can also define the negative log-likelihood as a function of lambda

NogLogLik_pois <- function(lambda) {
  -log(lambda)*sum(y) + length(y)*lambda
}

#This command mimimizes the function NegLogLik_pois 
# i.e., maximizes the log-likelihood, with a given starting point
# e.g., here we use 1 as a starting value
nlm(NogLogLik_pois, p = 1, hessian=TRUE)

alpha <-0.05
lambda.true <-2 # Select the value for true parameter.
n <- 10 # Sample size
nSim <- 1000 #number of simulated samples
Z <- qnorm(1-alpha/2)  #Critical value

cover <- 0
for(i in 1:nSim)
{
  Y <- rpois(n,lambda=lambda.true)
  l.hat  <- mean(Y)
  lowCI <- l.hat - Z*sqrt(l.hat/n)
  upCI <- l.hat + Z*sqrt(l.hat/n)
  if(lambda.true>lowCI & lambda.true<upCI) cover <- cover+1
}

cover/nSim

n <- 10
lambda_hat <- sum(y)/n
lambda_0 <- 2
Z_w <- (lambda_hat-lambda_0)/sqrt(0.21)
(p_value_W <- 2*pnorm(Z_w, lower.tail = FALSE))

G2 <- 2 *(sum(y)*(log(lambda_hat) - log(lambda_0)) - n*(lambda_hat-lambda_0) )
(p_value_LR <- pchisq(G2, df=1, lower.tail =  FALSE))

I <- n / lambda_0
S <- sum(y)/ lambda_0 - n
S_0 <- S/sqrt(I)

(p_value_S <- 2*pnorm(S_0, lower.tail =  FALSE))

n <- 365
pi_0 <- 30/365
pi_hat <- 35/365
AsVar <- pi_hat *(1-pi_hat)/n

Z_W <- (pi_hat - pi_0)/sqrt(AsVar)

(p_value <- pnorm(Z_W, lower.tail = FALSE))

x1 <- c(108, 23, 210, 14)
x2 <- c(87, 35, 120, 23)
n <-length(x1)

#MLE estimators
(l1hat <- n/sum(x1))
(l2hat <- n/sum(x2))

#Asymptotic variance
(as_var <- l1hat^2 / n + l2hat^2 /n)

#Test-statistic
(Z_w <- (l1hat - l2hat)/sqrt(as_var))

#P-value for a two sided test
(p_value <- 2*pnorm(Z_w, lower.tail = TRUE))