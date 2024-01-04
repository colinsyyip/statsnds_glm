pill_df <- read.csv("pill.csv")
bonemarrow_df <- read.csv("bonemarrow.csv")

lin_mod1 <- lm(agedon ~ agerec, data = bonemarrow_df)
summary(lin_mod1)

glm_1 <- glm(agedon ~ agerec, data = bonemarrow_df, family = gaussian)
summary(glm_1)

log_reg1 <- glm(agvhd ~ agedon, data = bonemarrow_df, family = binomial)
summary(log_reg1)
# Link function is logit

log_reg2 <- glm(agvhd ~ agedon, data = bonemarrow_df, 
                family = binomial(link = "identity"))
summary(log_reg2)

pill_gen_2 <- pill_df[pill_df$type_pill == 2, ]
pill_gen_3 <- pill_df[pill_df$type_pill == 3, ]
(n_cases_gen_2 <- sum(pill_gen_2$thrombosis))
(n_cases_gen_3 <- sum(pill_gen_3$thrombosis))
person_months_gen_2 <- pill_gen_2 %>% group_by(month) %>% summarize(cond_disp = sum(users))
person_months_gen_3 <- pill_gen_3 %>% group_by(month) %>% summarize(cond_disp = sum(users))
total_pm_gen_2 <- sum(pill_gen_2$users)
total_pm_gen_3 <- sum(pill_gen_3$users)
cases_bymonth_gen_2 <- pill_gen_2 %>% group_by(month) %>% summarize(cond_disp = mean(thrombosis))
cases_bymonth_gen_3 <- pill_gen_3 %>% group_by(month) %>% summarize(cond_disp = mean(thrombosis))
case_rate_gen_2 <- n_cases_gen_2/total_pm_gen_2
case_rate_gen_3 <- n_cases_gen_3/total_pm_gen_3

glm_2 <- glm(thrombosis ~ as.factor(type_pill) + offset(log(users)), data = pill_df, family = poisson)
summary(glm_2)

glm_3 <- glm(thrombosis ~ as.factor(type_pill) + offset(log(users)) + age + month, data = pill_df, family = quasipoisson)
summary(glm_3)

saturated_chisq <- 1 - pchisq(q=941.77, df = 884)

