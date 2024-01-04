# Get overview of all packages on your computer
pkgs <- rownames(installed.packages())

#if our package is missing, install it
if(!"ggplot2" %in% pkgs) install.packages("ggplot2")
if(!"car" %in% pkgs) install.packages("car")
if(!"doBy" %in% pkgs) install.packages("doBy")
if(!"multcomp" %in% pkgs) install.packages("multcomp")

# Read data into an object named teaching_data
# Check whether Program needs to be converted to a factor
teaching_data <- read.table(file = "Teaching material wur.txt", header=TRUE)
teaching_data$Program <- as.factor(teaching_data$Program)

# Visualise the data
# Create plot of score vs age
# and fit least squares regression per Program
library(ggplot2)

# define the plot, use geom_point to add the dots, 
# use stats_smooth to add regression lines without standard errors 
ggplot(data = teaching_data, aes(y=score,x=age,color=Program)) + 
  geom_point()+ 
  stat_smooth(method="lm",se=FALSE) 

### Q1: Extend the line beyond the given data

# Create the ANCOVA model; note that lm (as always) decides
# on whether a variable or a covariate on whether it is coded as a factor
# or as a numerical variable

teaching.lm <- lm(score ~ Program +age, data=teaching_data)
summary(teaching.lm)

library(car) #for type 2 sum of squares
Anova(teaching.lm)

### Q2: Animal: 104.2740 - 1.8305x, Food: 65.2828 - 1.8305x, Plant: 97.3779 - 1.8305x
### Q3: 1. H_0: 

#check out the average ages per group and the overall mean age
library(doBy)
summaryBy(age ~ Program, data = teaching_data, FUN=mean)
mean(teaching_data$age)

## Post-hoc testing
library(emmeans)

emmeans(teaching.lm, specs = "Program", by = "age")

pairs(emmeans(teaching.lm, specs = "Program", by = "age"), adjust="none")

# Read data into an object named rats_data
rats_data <- read.table(file = "RoastedPeanuts.txt", header = TRUE)
str(rats_data)

# createa plot with least-squares fitted lines per sex 
library(ggplot2)
ggplot(rats_data, aes(x = fraction, y = weight_gain, color = sex )) +
  geom_point(aes(size = 1.5)) +                # make the dots a little larger
  geom_smooth(method = "lm", se=FALSE)

# linear model
rats.lm1 <- lm(formula = weight_gain ~ fraction * sex, data = rats_data)
summary(rats.lm1)

library(car) #for type 2 sum of squares
Anova(rats.lm1)

# reduced model
rats_reduced.lm <- lm(weight_gain ~ fraction, data = rats_data)
summary(rats_reduced.lm)
Anova(rats_reduced.lm)

# compare the models with an F-test
anova(rats_reduced.lm, rats.lm1)
