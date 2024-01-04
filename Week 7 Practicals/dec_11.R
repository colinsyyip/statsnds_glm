setwd("/Users/colinyip/Documents/Masters School Work/GLM/Week 7 Practicals")

ova_df <- read.csv("ova.csv")
ova_df %>% group_by(death, figo) %>% count()

pi1 <- 0.85417
pi0 <- 0.62595
risk_ratio <- pi1 / pi0
odds_ratio <- (pi1 / (1 - pi1))/(pi0 / (1 - pi0))

model.lr1 <- glm(death ~ figo, family = binomial, data = ova_df)
summary(model.lr1)
exp(model.lr1$coefficients['figo'])
beta_1 <- model.lr1$coefficients['figo']
beta_1_se <- 0.3161
alpha <- 0.05
z_alpha_2 <- qnorm(1 - alpha / 2)
conf_interval <- c(beta_1 - z_alpha_2 * beta_1_se, beta_1 + z_alpha_2 * beta_1_se)
odds_ratio_conf_interval <- c(exp(conf_interval[1]), exp(conf_interval[2]))
lrt_conf_interval <- cbind(exp(coefficients(model.lr1)), exp(confint(model.lr1)))

model.lr2 <- glm(death ~ diameter, family = binomial, data = ova_df)
summary(model.lr2)
pred_logit <- model.lr2$coefficients[1] + model.lr2$coefficients[2] * 5
pred_prob <- log(pred_logit)

all_pred_values <- predict(model.lr2, type="response")
all_pred_values_log <- log(all_pred_values)
pred_diam <- data.frame(pred_logit = all_pred_values, diameter = ova_df$diameter)
sorted_pred_diam <- pred_diam[order(pred_diam$diameter, decreasing = F), ]
plot(sorted_pred_diam$diameter, sorted_pred_diam$pred_logit, type = "l")
plot(sorted_pred_diam$diameter, log(sorted_pred_diam$pred_logit), type = "l")

ova_df %>% group_by(death, ascites) %>% count()
model.lr3 <- glm(death ~ factor(ascites), family = binomial, data = ova_df)
summary(model.lr3)

model.lr4 <- glm(death ~ factor(ascites) + karn + figo + diameter, family = binomial, data = ova_df)
summary(model.lr4)
