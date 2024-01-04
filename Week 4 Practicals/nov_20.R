## Set working directory to convenience
setwd("/Users/colinyip/Documents/Masters School Work/GLM/Week 4 Practicals")

## Packages (install and invoke)
pkgs <- rownames(installed.packages())
if(!"faraway" %in% pkgs) install.packages("faraway")
if(!"car" %in% pkgs) install.packages("car")

library(faraway)
library(MASS)
library(car)

y <- Duncan$prestige
X <- cbind(1,Duncan$income,Duncan$education)
#Least squares coefficients:
Xinv=solve(t(X)%*%X)
b <- Xinv %*% t(X) %*% y

#Least squares estimates using lm:
lse <- lm(prestige ~ income + education, data=Duncan)
summary(lse)
coef(lse)

# Income has a p-value of <0.05 so for an alpha = 0.05, income is significant wrt prestige
X1star <- X[,2]-mean(X[,2])
X2star <- X[,3]-mean(X[,3])
Xstar <- cbind(X1star, X2star)
ystar <- y-mean(y)
(b1 <- (solve(t(Xstar) %*% Xstar)) %*% t(Xstar) %*% ystar)

lse_new <- lm(ystar ~ X1star + X2star)
summary(lse_new)
coef(lse_new)

# Ex. 4
(summary(lse)$sigma)^2

e <- y - X %*% b
ete <- t(e) %*% e
n <- nrow(X)
k <- 2
(S2e <- as.numeric(ete/(n-(k+1)))) 

(Vb <- S2e * solve(t(X)%*% X) )
# Variances are given matrix, standard error is root of the matrix (sqrt((Vb <- S2e * solve(t(X)%*% X) )))
sqrt((Vb <- S2e * solve(t(X)%*% X) ))

estar <- ystar - Xstar %*% b1
(S2estar <- as.numeric((t(estar) %*% estar)/(n-(k+1)))) # at this moment this is a 1x1 matrix

(Vb1 <- S2estar * solve(t(Xstar)%*% Xstar) )

nRep <- 100 # number of replicates
betas <- rep(0, nRep)
var_res <- rep(0, nRep)

for (i in seq(1,nRep, by = 1))
{
  
  set.seed(i)
  x <- runif(100) #Simulated predictor
  y <- 2 + 5* x  + rnorm(100) # generated response, with true sigma^2 = 1
  
  modSim <- lm(y ~ x) #Regression model
  
  betas[i] <- coef(modSim)[2] # mean of each run
  var_res[i] <- (summary(modSim)$sigma)^2
  
}

# Histogram
hist(betas)
mean(betas) # estimate for slope over iterations

mean(var_res) # mean of residual variances

# Ex. 6
cars_data <- mtcars[,c("mpg","disp","hp","wt")]
head(cars_data)

model_cars <- lm(mpg~disp+hp+wt, data = cars_data)

# Show the model.
summary(model_cars)

coef(model_cars)

# (Intercept)          disp            hp            wt 
# 37.1055052690 -0.0009370091 -0.0311565508 -3.8008905826 

(X <- model.matrix(model_cars))
y <- as.vector(cars_data$mpg)

beta <- solve(t(X)%*%X)%*%(t(X)%*%y)

e <- y - X %*% beta
ete <- t(e) %*% e
n <- nrow(X)
k <- 2
(S2e <- as.numeric(ete/(n-(k+1))))

(Vb <- S2e * solve(t(X)%*% X) )