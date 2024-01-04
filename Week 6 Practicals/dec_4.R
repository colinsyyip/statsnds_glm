pkgs <- rownames(installed.packages())
if(!"faraway" %in% pkgs) install.packages("faraway")
if(!"MASS" %in% pkgs) install.packages("MASS")
if(!"leaps" %in% pkgs) install.packages("leaps")
if(!"MuMIn" %in% pkgs) install.packages("MuMIn")
if(!"ISLR" %in% pkgs) install.packages("ISLR")
if(!"corrplot" %in% pkgs) install.packages("corrplot")
if(!"caret" %in% pkgs) install.packages("caret")

library(faraway)
library(MASS)
library(leaps)
library(MuMIn)
library(ISLR)
library(corrplot)
library(caret)

cars_data <- mtcars[,c("mpg", "cyl", "disp", "hp", "wt")]
head(cars_data)

#Create a linear regression model
cars_model <- lm(mpg~., data = cars_data)
summary(cars_model)
corrplot(cor(cars_data), method = "number")

vif(cars_model)
cars_model_disp <- lm(disp ~ cyl + hp + wt, data = cars_data)
R2_disp <- summary(cars_model_disp)$r.squared 
(VIF_disp <- 1/(1-R2_disp))

cars_model2 <- update(cars_model, . ~.-disp)
vif(cars_model2)

summary(cars_model2)

anova(cars_model, cars_model2)

data(prostate)
mo.p <- lm(lpsa ~ ., prostate)
summary(mo.p)

# Gleason least significant with P-value 0.775; remove it and refit the model
mo.p.1 <- update(mo.p, .~. -gleason)
summary(mo.p.1)

# lcp least significant with P-value 0.251; remove and refit the model
mo.p.2 <- update(mo.p.1, .~. -lcp)
summary(mo.p.2)

# pgg45 least significant with P-value 0.25; remove
mo.p.3 <- update(mo.p.2, .~. -pgg45)
summary(mo.p.3)

# age least significant with P-value 0.27; remove
mo.p.4 <- update(mo.p.3, .~. -age)
summary(mo.p.4)

# lbph least significant with P-value 0.11; remove
mo.p.5 <- update(mo.p.4, .~. -lbph)
summary(mo.p.5)

moma.p <- model.matrix(mo.p)[,-1]
adju <- leaps(moma.p, prostate$lpsa, method="adjr2")
maxadjr(adju, 10)

mallows <-leaps(moma.p, prostate$lpsa)
Cpplot(mallows)

data(prostate)
options(na.action="na.fail") # needed for dredge() function
lmo <- lm(lpsa ~ ., prostate)
mmi <- dredge(lmo)
nrow(mmi)
head(mmi, 15)

mmi <- dredge(lmo, rank = "AIC")
head(mmi, 15)

mmi <- dredge(lmo, rank = "BIC")
head(mmi, 15)

#Data generation 
YX <- matrix(NA, 500, 101)
set.seed(10824)
for (i in 1:101) YX[,i] <- rnorm(500)
X <- YX[,2:101]
Y <- YX[,1]
mydata <- data.frame(Y, X)
#Regressing Y against all the predictors
lm.sim <- lm(Y ~ . , mydata)
summary(lm.sim)

Tvals <- abs(coef(summary(lm.sim))[2:101,3])
selected.vars <- order(Tvals, decreasing=TRUE)[1:3] #select the three variables with highest absolute t-value
selected.vars

lm.sim.sel <- lm(Y ~ X[,selected.vars]) # Linear regression with only three variables
summary(lm.sim.sel)

library(leaps)
b <- regsubsets(Y~., data=mydata, nbest=1, nvmax=3, intercept=TRUE, method=c("exhaustive"), really.big=TRUE)
rs <- summary(b)
rs$cp # Cp values for best models with 1, 2, and 3 regressors
rs$adjr2 # Adjr2 values for best models with 1, 2, and 3 regressors
(sel_reg <- names(rs$which[3,])[rs$which[3,]]) # Names of regressors of best fitting 3-regressor model
sel_reg <- (substring(sel_reg[-1],2,3))
sel_reg <- as.numeric(sel_reg)
lm.sim.sel1 <- lm(Y ~ X[,sel_reg])
summary(lm.sim.sel1)

b2 <- regsubsets(Y ~ ., data=mydata, nbest=1, nvmax=100, intercept=TRUE, method=c("forward"), really.big=TRUE)
rs2 <- summary(b2)
plot(2:101,rs2$cp,xlab="No of parameters",ylab="Cp statistic")
abline(a=0, b=1)

(1:length(rs2$cp))[rs2$cp==min(rs2$cp)]

newX <- matrix(NA, 500, 100)
for (i in 1:100) newX[,i] <- rnorm(500)
newY <- rnorm(500)
lm.sim.val <- lm(newY ~ newX[,selected.vars])
summary(lm.sim.val)

hitters <- na.omit(ISLR::Hitters)
dim(hitters)
lm_hitters <- lm(Salary ~ ., data = hitters)
summary(lm_hitters)

#All possible subsets
best_subset_hitters <- regsubsets(Salary ~ ., hitters, nvmax = 19)
summary(best_subset_hitters)

set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(hitters), replace = T, prob = c(0.6,0.4))
train <- hitters[sample, ]
test <- hitters[!sample, ]

# perform best subset selection
best_subset <- regsubsets(Salary ~ ., train, nvmax = 19)
results <- summary(best_subset)
results
adj_R2 <- results$adjr2
Cp <- results$cp
BIC <- results$bic
predictors <- c(1:19)
plot(adj_R2 ~ predictors, type="l", lty = 2, lwd = 2)
plot(Cp ~ predictors, type="l", lty = 2, lwd = 2)
plot(BIC ~ predictors, type="l", lty = 2, lwd = 2)

which.max(results$adjr2)
which.min(results$bic)
which.min(results$cp)

# Selected 4-variables
(selected_variables <- colnames(results$outmat)[results$outmat[4,]=="*"])

lm_training_hitters <- lm(Salary ~ Runs + CAtBat + CHits + PutOuts, data = train)
summary(lm_training_hitters)

# predicting the target variable
predictions_hitters <- predict(lm_training_hitters, test)

plot(predictions_hitters ~ test$Salary, 
     xlab="Testing values", ylab = "Predicted values",
     lwd= 3)
abline(a=0, b=1, col="red")

# computing model performance metrics
(R2 <- R2(predictions_hitters, test$Salary))
(RMSE <- RMSE(predictions_hitters, test$Salary))