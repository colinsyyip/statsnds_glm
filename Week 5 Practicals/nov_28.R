pkgs <- rownames(installed.packages())
if(!"EnvStats" %in% pkgs) install.packages("EnvStats")
if(!"car" %in% pkgs) install.packages("car")
if(!"faraway" %in% pkgs) install.packages("faraway")

library(EnvStats)
library(car)
library(faraway)

lime <- read.table("lime.prn", header=T)
head(lime)
limeAL <- lime[lime$lime =="AL",]
#Linear regression model
mr <- lm(ph ~ rate, data=limeAL)
summary(mr)
#Anova table for the simple linear regression model
anova(mr)
plot(ph ~ rate, data=limeAL)
abline(mr)
#Now check the ANOVA model
ma <- lm(ph ~ as.factor(rate), data=limeAL)
#Anova table for the one-way Anova  model
anova(ma)
#Compare two models.
#Compare the outputs of the individual models
anova(mr,ma)

#Pure error variance estimate
(sigmaPE <- deviance(ma)/ma$df.residual)
(sdPE <- sqrt(sigmaPE))

x <- c(125,100,200,75,150,175,75,175,125,200,100)
y <- c(160,112,124,28,152,156,42,124,150,104,136)
RegModel <- lm(y~x)
summary(RegModel)
plot(y ~ x)
abline(RegModel)

#Now check the ANOVA model
AnModel <- lm(y ~ as.factor(x))
#Compare two models.
anova(RegModel,AnModel)
anovaPE(RegModel)

(sigmaPE <- deviance(AnModel)/ma$df.residual)
(sdPE <- sqrt(sigmaPE))

g <- lm(total ~ expend + salary + ratio + takers, data=sat)
summary(g)
X <- model.matrix(g)
H <- X%*%solve(t(X)%*%X)%*%t(X)
levs <- diag(H)
#Easier way to get the hat values
levs <- lm.influence(g)$hat

k <- dim(X)[2]-1 # Number of predictors
n <- dim(sat)[1]
(avLev <- (k+1)/n)
mean(levs)
cutoff1 <- 2*mean(levs)
cutoff2 <- 3*mean(levs)

levs[levs>cutoff1]
levs[levs>cutoff2]
stRes <- rstudent(g) 
cutoff_res <- 2
stRes[abs(stRes) > cutoff_res]
plot(rstudent(g) ~ g$fitted)
abline(h=2)
abline(h=-2)
# Bonferoni outlier test
outlierTest(g)
# plot Cook's distances and check the largest ones. 
cdist <- cooks.distance(g)
cutoff_cook <- 4/(n-k-1)
plot(cdist) # index plot
abline(h=cutoff_cook)
cdist[cdist > cutoff_cook]

#Obtain the standardized residuals
standard_res <- rstandard(g)
# Another way of calculating the Cook's distance
cdist1 <- (standard_res^2 / (k+1)) *(levs / (1-levs))
cbind(cdist, cdist1)
sum((cdist - cdist1)^2)
satNew <- sat[! (rownames(sat) %in% c(names(cdist[cdist > cutoff_cook]))),]
gNew <- lm(total ~ expend + salary + ratio + takers, data=satNew)
summary(gNew)
DFFITS_val <- stRes* sqrt(levs / (1-levs))

plot(DFFITS_val~levs, xlab = "Hat values", ylab = "Dffits")
cutoff_dfit <- 2*sqrt(k/n)
DFFITS_val[abs(DFFITS_val)>cutoff_dfit]
abline(h=cutoff_dfit)

covratio_i <- 1/((1-levs)*((n-k-2+stRes^2)/(n-k-1))^(k+1))
#covratio_i <- covratio(g)
plot(covratio_i, xlab = "Hat values", ylab = "Covratio")
abline(h=1)
cutoff_covr <- 3*(k+1)/n
DFFITS_val[abs(covratio_i-1)>cutoff_covr]

m1 <- lm(prestige ~ income + education, data=Duncan)
summary(m1)

lev <- lm.influence(m1)$hat

mean(lev)
3/45  # 3 parameters in model, 45 observations
threshlev <- 2*mean(lev) 
lev[lev>threshlev]
