# Get overview of all packages on your computer
pkgs <- rownames(installed.packages())

#if our package is missing, install it
if(!"emmeans" %in% pkgs) install.packages("emmeans")

#if our package is missing, install it
if(!"multcomp" %in% pkgs) install.packages("multcomp")

# taste of three types of tomato: round, beef, or cherry
taste <- c(25.44462, 28.09772, 46.46490, 36.95795, 24.83477, 28.46805,
           48.14579, 31.77559, 53.42217, 70.86571, 57.07008, 38.08433)

#create a qualitative variable indicating tomato type
tomtype <- as.factor(c(rep("round",4), rep("beef",4), rep("cherry",4)))

# one-way ANOVA model
Tastemodel <- lm(taste ~ tomtype)
anova(Tastemodel)

# Q1: SST: 2321.7, SSR: 1187.5, SSE: 1134.2
# Q2: df for SST is n obserations - 1, df for SSR is number of categories - 1, df for SSE is df for (df of SST) - (df of SSR)
# Q3: Given a P-value on the F test of < 0.05, at a significance level of 0.05, we can reject the null, and there is a significant diff.

# this is what happens when you do not introduce
# type of tomato as a factor
# type of tomato: 1 = round, 2 = beef, 3 = cherry tomato)
type <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3 ,3)
WrongModel <- lm(taste ~ type)
summary(WrongModel)

# Q4: incorrect as it is trying to evaluate the type as a numeric, as opposed to a categorical, and so it is trying to identify some incremental even step difference between the categories

# more output with the one-way anova model
summary(Tastemodel)

library(multcomp)

# All three pairwise comparisons with Tukeys method
Tukey <- glht(Tastemodel, linfct=mcp(tomtype="Tukey"))
summary(Tukey)

tomtype2 <- relevel(tomtype, ref="cherry")
Tastemodel2 <- lm(taste ~ tomtype2)
summary(Tastemodel)
summary(Tastemodel2)

# response is yield of fruit y, 
# comparing four pesticides or three varieties f fruit trees
# eight trees of each variety randomly assigned to the four pesticides
# two trees per variety, per pesticide
y <- c(49, 39, 55, 41, 66, 68, 50, 55, 67, 58, 85, 92, 43, 38, 53, 42, 69, 62, 53, 48, 85, 73, 85, 99)
pesticide <- factor(c(1,  1,  1,  1,  1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4))
variety <- factor(c(1, 1, 2, 2, 3, 3, 1, 1, 2, 2, 3, 3, 1, 1, 2, 2, 3, 3, 1, 1, 2, 2, 3, 3))

# printing the data
print(cbind(y, variety, pesticide))

#profile plot
interaction.plot(pesticide, variety, y)
interaction.plot(variety, pesticide, y)

# a two-way analysis of variance with interaction
twowayint <- lm(y ~ variety + pesticide + variety:pesticide)
anova(twowayint)
summary(twowayint)

library(emmeans)
#comparing pesticides averaged over varieties
marginal <- emmeans(twowayint, ~ pesticide)
# Fisher's LSD method
pairs(marginal, adjust="None")

# Tukey's method
pairs(marginal, adjust="Tukey")
