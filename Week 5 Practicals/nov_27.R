## Set working directory to convenience
setwd("/Users/colinyip/Documents/Masters School Work/GLM/Week 5 Practicals")

## Packages (install and invoke)
pkgs <- rownames(installed.packages())
if(!"bcgam" %in% pkgs) install.packages("bcgam")
if(!"car" %in% pkgs) install.packages("car")
if(!"faraway" %in% pkgs) install.packages

library(bcgam)
library(car)
library(faraway)

df <- 4
Z <- rnorm(1000)
chisq <- rchisq(1000, df)
y <- Z / sqrt(chisq/df)
hist(y, prob=TRUE, breaks=30, main="t-distribution with 4 df")
x <- seq(min(y), max(y), by=0.01)
d <- dt(x,df)
lines(d ~ x, type="l", col="red")

df <- 25
Z <- rnorm(1000)
chisq <- rchisq(1000, df)
y <- Z / sqrt(chisq/df)
hist(y, prob=TRUE, breaks=30, main="t-distribution with 25 df")
x <- seq(min(y), max(y), by=0.01)
d <- dt(x,df)
lines(d ~ x, type="l", col="red")

df1 <- 4; df2 <- 25
chisq1 <- rchisq(1000, df1)
chisq2 <- rchisq(1000, df2)
y <- (chisq1/df1) / (chisq2/df2)
hist(y, prob=TRUE, breaks=30, main="F-distribution with 4 and 25 df")
x <- seq(min(y), max(y), by=0.01)
d <- df(x,df1, df2)
lines(d ~ x, type="l", col="red")
mean(y)
# E(F) = df_2/(df_2-2) = 25/23

data(savings)
head(savings)

SavingsReg <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data=savings)
summary(SavingsReg)
# y_sr = 28.566 - 0.461 * pop15 - 1.691 * pop75 - 0.0003 * dpi + 0.410 * ddpi

# H_0: B_1 = ... = B_n = 0
(TSS <- sum((savings$sr-mean(savings$sr))^2)) #Total SS
(RSS <- sum(SavingsReg$residuals^2)) # Residual SS 
(RegSS <- TSS - RSS) #Regression SS
# F  = RegSS/k / RSS / (n - (k + 1))
(F <- ((RegSS)/4)/(RSS/45)) # Test statistic
pf(F,4,45, lower.tail = FALSE)
# Since p value of F stat is < 0.05, we reject H_0, and F tends to values larger than dist. given F(4, 45)
newX <- data.frame(pop15 = 30, pop75=2.5, dpi = 2000, ddpi = 4)
#Confidence interval
pr1 <- predict(SavingsReg, newdata = newX, interval = "confidence",se.fit=TRUE)
pr1$fit
#Prediction interval
pr2 <- predict(SavingsReg, newdata = newX, interval = "prediction",se.fit=TRUE)
pr2$fit

#Manual calculation of the predicted value
coef(SavingsReg)[1] + coef(SavingsReg)[2]*30 + 
  coef(SavingsReg)[3]*2.5 +  coef(SavingsReg)[4]*2000 +
  coef(SavingsReg)[5]*4 

#New x_star
newX <- as.vector(c(1, 30, 2.5, 2000, 4))

#Manual calculations
X_sr <- model.matrix(SavingsReg)
#Standard error of the average response
(se_mu <- summary(SavingsReg)$sigma * sqrt(t(newX)%*%solve(t(X_sr)%*%X_sr)%*%(newX)))
pr1$se.fit

c(pr1$fit[1]  - qt(0.025, pr1$df, lower.tail = FALSE)*se_mu, 
  pr1$fit[1] + qt(0.025, pr1$df, lower.tail = FALSE)*se_mu)

se_ind <- sqrt(pr1$se.fit^2 + pr1$residual.scale^2)

c(pr1$fit[1]  - qt(0.025, pr1$df, lower.tail = FALSE)*se_ind, 
  pr1$fit[1] + qt(0.025, pr1$df, lower.tail = FALSE)*se_ind)

data.frame(se_mu, se_ind)

SavingsReg0 <- lm(sr ~ pop15 + dpi , data=savings)
summary(SavingsReg0)

(RSS <- sum(SavingsReg$residuals^2)) # Residual SS of the full model
(RSS0 <- sum(SavingsReg0$residuals^2)) # Residual SS of the reduced model
(F <- ((RSS0 - RSS)/2)/(RSS/45)) # Test statistic
pf(F,2,45, lower.tail = FALSE)

SavingsReg01 <- lm(sr ~ I(pop15+pop75)+dpi+ddpi,savings)
summary(SavingsReg01)
anova(SavingsReg, SavingsReg01)

SatReg <-lm(total ~ expend + ratio + salary, data=sat)
summary(SatReg)
2*pt(-1.878, 42, lower.tail = TRUE)

SatReg0 <-lm(total ~ expend + ratio, data=sat)
summary(SatReg0)
anova(SatReg, SatReg0)

summary(SatReg)

Upp <- coefficients(summary(SatReg))[,1] + qt(0.975,46 ) * coefficients(summary(SatReg))[,2]
Low <- coefficients(summary(SatReg))[,1] -  qt(0.975,46 ) * coefficients(summary(SatReg))[,2]
cbind(Low, Upp)
confint(SatReg)

SatReg.wtakers <-lm(total ~ expend + ratio + salary + takers, data=sat)
anova(SatReg, SatReg.wtakers)

#F-statistics
(fstat <- (deviance(SatReg)-deviance(SatReg.wtakers))/(deviance(SatReg.wtakers)/df.residual(SatReg.wtakers)))
#p-value
pf(fstat, 1, df.residual(SatReg.wtakers), lower.tail=FALSE)
#t-statistics
(tstat<-coef(summary(SatReg.wtakers))[5,3])
#p-value
2*pt(tstat,  df.residual(SatReg.wtakers), lower.tail=TRUE)