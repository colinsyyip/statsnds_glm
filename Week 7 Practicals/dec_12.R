bonemarrow_df <- read.csv("bonemarrow.csv")

model.lg1 <- glm(agvhd ~ agerec, family = binomial, data = bonemarrow_df)
pred_lg1 <- predict(model.lg1)
pred_prob_lg1 <- exp(pred_lg1)/(1 + exp(pred_lg1))
plot(bonemarrow_df$agerec, pred_prob_lg1)

logit <- predict(model.lg1, type="response", se.fit = TRUE)
logit.lwb <- logit$fit-1.96*logit$se.fit
logit.upb <- logit$fit+1.96*logit$se.fit
prob.lwb <- exp(logit.lwb) / (1 + exp(logit.lwb))
prob.upb <- exp(logit.upb) / (1 + exp(logit.upb))

ggplot(x = bonemarrow_df$agerec) + 
  geom_line(aes(x = bonemarrow_df$agerec, y = exp(logit$fit)/(1 + exp(logit$fit)))) +
  geom_ribbon(aes(x = bonemarrow_df$agerec, ymin = prob.lwb, ymax = prob.upb), alpha = 0.2)

bonemarrow_df$agerec_sq <- bonemarrow_df$agerec ^ 2
model.lg1_sq <- glm(agvhd ~ agerec_sq, family = binomial, data = bonemarrow_df)
pred_lg1_sq <- predict(model.lg1_sq)
pred_prob_lg1_sq <- exp(pred_lg1_sq)/(1 + exp(pred_lg1_sq))

ggplot(x = bonemarrow_df$agerec) + 
  geom_line(aes(x = bonemarrow_df$agerec, y = pred_prob_lg1_sq), color = "red") + 
  geom_line(aes(x = bonemarrow_df$agerec, y = pred_prob_lg1))

ggplot(x = bonemarrow_df$agedon) + 
  geom_point(aes(x = bonemarrow_df$agedon, y = bonemarrow_df$agerec))

model.lg2 <- glm(agvhd ~ agerec + agedon, family = binomial, data = bonemarrow_df)
summary(model.lg2)

model.lg3 <- glm(agvhd ~ agedon + factor(diag), family = binomial, data = bonemarrow_df)
summary(model.lg3)
pred_lg3 <- predict(model.lg3, type="response")
pred_prob_lg3 <- exp(pred_lg3)/(1 + exp(pred_lg3))

ggplot(x = bonemarrow_df$agedon) + 
  geom_point(aes(x = bonemarrow_df$agedon, y = pred_prob_lg3))

model.lg4 <- glm(agvhd ~ agedon + factor(diag) + sexdon + sexrec, family = binomial, data = bonemarrow_df)
summary(model.lg4)

lr_stat <- -2 * (logLik(model.lg3) - logLik(model.lg4))
df <- df.residual(model.lg3) - df.residual(model.lg4)
p_value <- 1 - pchisq(lr_stat, df)

model.lg5 <- glm(agvhd ~ agedon + factor(diag) + sexdon * sexrec, family = binomial, data = bonemarrow_df)
summary(model.lg5)
bonemarrow_df %>% group_by(sexdon, sexrec) %>% count()
pi_1_0 <- 38/(38 + 45)
pi_0_0 <- 45/(38 + 45)
(pi_1_0/(1 - pi_1_0))/(pi_0_0/(1 - pi_0_0))

pi_0_1 <- 44/(44 + 45)
pi_0_0 <- 45/(44 + 45)
(pi_1_0/(1 - pi_1_0))/(pi_0_0/(1 - pi_0_0))

pi_1_1 <- 39/(39 + 45)
pi_0_0 <- 45/(39 + 45)
(pi_1_1/(1 - pi_1_1))/(pi_0_0/(1 - pi_0_0))

bonemarrow_df$mismatch <- ifelse(bonemarrow_df$sexdon == bonemarrow_df$sexrec, 0, 1)
model.lg6 <- glm(agvhd ~ agedon + factor(diag) + mismatch, family = binomial, data = bonemarrow_df)
summary(model.lg6)

initial_model <- glm(agvhd ~ agedon + agerec + sexdon + sexrec + factor(diag) + mismatch, 
                    family = binomial, data = bonemarrow_df)
stepwise_model <- step(initial_model, direction = "both")

plot(hatvalues(stepwise_model))