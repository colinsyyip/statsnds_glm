# weight gain y of chickens with amount of lysine x ingested
y <- c(14.7, 17.8, 19.6, 18.4, 20.5, 21.1, 17.2, 18.7, 20.2, 16.0, 17.8, 19.4)
x <- c(0.09, 0.14, 0.18, 0.15, 0.16, 0.23, 0.11, 0.19, 0.23, 0.13, 0.17, 0.21)

# plot the data
plot(x, y)

# Q1 - Positive correlation, as x increases so does y
# Guesstimate like a 0.8, positive

cor(x, y)

# Q2 - Fairly close 

# Q3 
weightgain <- lm( y ~ x )

summary(weightgain)

# Q4 

anova(weightgain)
sst <- sum((y - mean(y)) ^ 2)

# Q10
cor(x, y)
summary(weightgain)

manual_r2 <- 28.358 / (28.358 + 10.692)

### Ex. 2 
Norway_NO2 <- read.csv("Norway_NO2.csv")

# Matrix of scatterplots
pairs(Norway_NO2[,1:4])
pairs(Norway_NO2[,c(1,5,6,7,8)])

# Perform a simple linear regression
lm1 <-lm(y ~ x3, data = Norway_NO2)
summary(lm1)
anova(lm1)

# multiple linear regression model with all 7 predictors
lmmulti <-lm(y ~ x1 + x2 + x3 + x4+ x5 + x6 + x7, data = Norway_NO2)
summary(lmmulti)
anova(lmmulti)

# Predict the concentration of NO2 at a wind speed of ... 
par(mfrow = c(1,1))
plot (y ~ x3, data=Norway_NO2)

# newdata <- data.frame(x3= c(......))
newdata <- data.frame(x1 = mean(Norway_NO2$x1), x2 = mean(Norway_NO2$x2), 
                      x3 = c(2,4,6,8),
                      x4 = mean(Norway_NO2$x4), x5 = mean(Norway_NO2$x5),
                      x6 = mean(Norway_NO2$x6), x7 = mean(Norway_NO2$x7))
predict(lmmulti,newdata , se.fit = TRUE, interval = "confidence")

## Residuals and fitted values
resi <- residuals(lmmulti)
pred <- fitted(lmmulti)

## Allowing two side-by-side plots
par(mfrow = c(1,2))

## Scatterplot of residual vs predicted values
plot(resi ~ pred)
abline(h = 0)
title ("Homoscedasticity check")

## Normal Q-Q plot
qqnorm(resi)
qqline(resi)
