data = read.csv("/Users/chiragshahi/Desktop/Project 6/prostate_cancer.csv")
attach(data)
vesinv = as.factor(vesinv)
log_PSA = log(psa)
qqnorm(log_PSA)
qqline(log_PSA)

# plot log_PSA against cancervol
plot(cancervol, log_PSA)
fit_cancervol = lm(log_PSA ~ cancervol)
abline(fit_cancervol)
summary(fit_cancervol)

# plot log_PSA against weight
plot(weight, log_PSA)
fit_weight = lm(log_PSA ~ weight)
abline(fit_weight)
summary(fit_weight)

# plot log_PSA against age
plot(age, log_PSA)
fit_age = lm(log_PSA ~ age)
abline(fit_age)
summary(fit_age)

# plot log_PSA against benpros
plot(benpros, log_PSA)
fit_benpros = lm(log_PSA ~ benpros)
abline(fit_benpros)
summary(fit_benpros)

# plot log_PSA against vesinv
plot(vesinv, log_PSA)
fit_vesinv = lm(log_PSA ~ vesinv)
summary(fit_vesinv)

# plot log_PSA against capspen
plot(capspen, log_PSA)
fit_capspen = lm(log_PSA ~ capspen)
abline(fit_capspen)
summary(fit_capspen)

# plot log_PSA against gleason
plot(gleason, log_PSA)
fit_gleason = lm(log_PSA ~ gleason)
abline(fit_gleason)
summary(fit_gleason)

fit.forward <- step(lm(log_PSA ~ 1, data), scope = list(upper = ~cancervol + weight + age + benpros + vesinv + capspen + gleason), direction = "forward")

fit.backward <- step(lm(log_PSA ~ cancervol + weight + age + benpros + vesinv + capspen + gleason, data), scope = list(lower = ~1), direction = "backward")

summary(fit.forward)

#anova : cancervol + benpros + vesinv - gleason
fit_c_b_v <- lm(log_PSA ~ cancervol + benpros + vesinv, data) 
anova(fit_c_b_v, fit.backward)

#anova : cancervol + benpros = gleason - vesinv 
fit_c_b_g <- lm(log_PSA ~ cancervol + benpros + gleason, data) 
anova(fit_c_b_g, fit.backward)

#anova : cancervol + vesinv + gleason - benpros
fit_c_v_g <- lm(log_PSA ~ cancervol + vesinv + gleason, data)
anova(fit_c_v_g, fit.backward)

#anova : benpros + vesinv + gleason - cancervol
fit_b_v_a <- lm(log_PSA ~ benpros + vesinv + gleason, data) 
anova(fit_b_v_a, fit.backward)

summary(fit.backward)

# model 
plot(fitted(fit.backward),resid(fit.backward)) 
abline(h=0)

qqnorm(resid(fit.backward)) 
qqline(resid(fit.backward))

# predictions
mean_cancervol = mean(data$cancervol)
mean_gleason = mean(data$gleason)
mean_benpros = mean(data$benpros)
freq_vesinv = as.integer(names(sort(table(data$vesinv), decreasing = T, na.last = T)[1])) 

predicted = fit.backward$coefficients["(Intercept)"] +
  + (fit.backward$coefficients["cancervol"] * mean_cancervol) +
  + (fit.backward$coefficients["gleason"] * mean_gleason) +
  + (fit.backward$coefficients["benpros"] * mean_benpros) +
  + (fit.backward$coefficients["vesinv"] * freq_vesinv)

cat("Predicted Outcome: ", predicted)
cat("Actual Predicted Outcome:", exp(predicted))
    
