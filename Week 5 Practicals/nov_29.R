## Packages (install and invoke)
pkgs <- rownames(installed.packages())
if(!"EnvStats" %in% pkgs) install.packages("EnvStats")
if(!"MASS" %in% pkgs) install.packages("MASS")
if(!"car" %in% pkgs) install.packages("car")
if(!"faraway" %in% pkgs) install.packages("faraway")
if(!"splines" %in% pkgs) install.packages("splines")

library(EnvStats)
library(car)
library(faraway)
library(splines)
library(MASS)

set.seed(12345)
n <- 20
x <- runif(n)
eps1 <- rnorm(n)
eps2 <- rnorm(n, sd=sqrt(2*x))
eps3 <- rchisq(n,2)-2
y1 <- 5 + 8*x + eps1
y2 <- 5 + 8*x + eps2
y3 <- 5 + 8*x + eps3
lmo1 <- lm(y1 ~ x); std.res1 <- rstandard(lmo1)
lmo2 <- lm(y2 ~ x); std.res2 <- rstandard(lmo2)
lmo3 <- lm(y3 ~ x); std.res3 <- rstandard(lmo3)
q1<-car::qqPlot(std.res1); q2<-car::qqPlot(std.res2); q3<-car::qqPlot(std.res3);
plot(std.res1 ~ fitted(lmo1)); plot(std.res2 ~ fitted(lmo2)); plot(std.res3 ~ fitted(lmo3))

set.seed(6789)
n <- 100
x <- runif(n)
eps1 <- rnorm(n)
eps2 <- rnorm(n, sd=sqrt(2*x))
eps3 <- rchisq(n,2)-2
y1 <- 5 + 8*x + eps1
y2 <- 5 + 8*x + eps2
y3 <- 5 + 8*x + eps3
lmo1 <- lm(y1 ~ x); std.res1 <- rstandard(lmo1)
lmo2 <- lm(y2 ~ x); std.res2 <- rstandard(lmo2)
lmo3 <- lm(y3 ~ x); std.res3 <- rstandard(lmo3)
q1<-car::qqPlot(std.res1); q2<-car::qqPlot(std.res2); q3<-car::qqPlot(std.res3);
plot(std.res1 ~ fitted(lmo1)); plot(std.res2 ~ fitted(lmo2)); plot(std.res3 ~ fitted(lmo3))

X1 <- runif(100)
X2 <- runif(100)
epsilon <- rnorm(100)
Y <- X1 + X2 + 0.1*epsilon
lmo.a <- lm(Y ~ X1 + X2)
crPlot(lmo.a,X1) # function from car library
crPlot(lmo.a,X2)

X1 <- runif(100)
delta <- rnorm(100)
epsilon <- rnorm(100)
X2 <- X1 + 0.1*delta
Y <- X1 + X2 + 0.1*epsilon
lmo.b <- lm(Y ~ X1 + X2)
crPlot(lmo.b,X1)
crPlot(lmo.b,X2)

X1 <- runif(100)
X2 <- runif(100)
epsilon <- rnorm(100)
Y <- 2*(X1-0.5)^2 + X2 + 0.1*epsilon
lmo.c <- lm(Y ~ X1 + X2)
crPlot(lmo.c,X1)
crPlot(lmo.c,X2)

data(savings)
head(savings)
g <- lm(sr ~ pop15+pop75+dpi+ddpi,savings)
summary(g)
bc <- MASS::boxcox(g,plotit=T)
#We can narrow the range of lambda, to make it more visible
bc <- MASS::boxcox(g,plotit=T,lambda=seq(0.5,1.5,by=0.1))
# Exact lambda
lambda <- bc$x[which.max(bc$y)]
lambda

data(gala)
g <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent,gala)
summary(g)
summary(g)$r.squared

car::qqPlot(gala$Species)
bc <- MASS::boxcox(g,plotit=T)
bc <- MASS::boxcox(g,lambda=seq(0.0,1.0,by=0.05),plotit=T)
lambda <- bc$x[which.max(bc$y)]
lambda

bc_Species <- (gala$Species^lambda - 1) / lambda
#car::qqPlot(bc_Species)
car::qqPlot(bc_Species)

head(aatemp)
olm <- lm(temp ~ year, aatemp)
summary(olm)
plot(temp ~ year, data=aatemp)
abline(olm)

plm.10 <- lm(temp ~ poly(year, 10), data=aatemp)
summary(plm.10)

plot(temp ~ year, data=aatemp)
lines(aatemp$year, fitted(plm.5))
d2020 <- data.frame(year=2020)
predict(plm.5, d2020)