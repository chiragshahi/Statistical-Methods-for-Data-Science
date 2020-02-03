data <- read.csv("/Users/chiragshahi/Desktop/Project5/bodytemp-heartrate.csv")
data

male_data <- subset(data, data$gender == 1)
male_data
summary(male_data$body_temperature)
female_data <- subset(data, data$gender == 2)
female_data
summary(female_data$body_temperature)
boxplot(male_data$body_temperature, female_data$body_temperature, main = "Boxplot for body temperatures of males and females", ylab = "Temperature", names = c('Male', 'Female'))
qqnorm(male_data$body_temperature, main = "Q-Q plot for males")
qqline(male_data$body_temperature)
qqnorm(female_data$body_temperature, main = "Q-Q plot for females")
qqline(female_data$body_temperature)
t.test(male_data$body_temperature, female_data$body_temperature, alternative = "two.sided", var.equal = FALSE)
____

boxplot(male_data$heart_rate, female_data$heart_rate, main = "Boxplot for heart rates of males and females", ylab = "Mean heart rate", names = c('Male', 'Female'))
summary(male_data$heart_rate)
summary(female_data$heart_rate)
qqnorm(male_data$heart_rate, main = "Q-Q plot for males")
qqline(male_data$heart_rate)
qqnorm(female_data$heart_rate, main = "Q-Q plot for females")
qqline(female_data$heart_rate)
t.test(male_data$heart_rate, female_data$heart_rate, alternative = "two.sided", var.equal = FALSE)
____

cor(data$body_temperature, data$heart_rate)
scatter.smooth(data$body_temperature, data$heart_rate, main = "Scatterplot of body temperature & heart rate for people", xlab = "Body Temperature", ylab = "Heart Rate")
model_data <- lm(body_temperature ~ heart_rate, data = data)
model_data
summary(model_data)

cor(male_data$body_temperature, male_data$heart_rate)
scatter.smooth(male_data$body_temperature, male_data$heart_rate, main = "Scatterplot of body temperature & heart rate for males", xlab = "Body Temperature", ylab = "Heart Rate")
model_male_data <- lm(body_temperature ~ heart_rate, data = male_data)
model_male_data
summary(model_male_data)

cor(female_data$body_temperature, female_data$heart_rate)
scatter.smooth(female_data$body_temperature, female_data$heart_rate, main = "Scatterplot of body temperature & heart rate for females", xlab = "Body Temperature", ylab = "Heart Rate")
model_female_data <- lm(body_temperature ~ heart_rate, data = female_data)
model_female_data
summary(model_female_data)

___________

z_ci <- function(n, lambda) {
  exDist <- rexp(n, lambda)
  lower <- mean(exDist) - qnorm(.975) * sd(exDist) / sqrt(n)
  upper <- mean(exDist) + qnorm(.975) * sd(exDist) / sqrt(n)
  m <- 1/lambda
  if (upper > m && lower < m) 
    return (1)
  else
    return (0)
}

z_prop <- function(n, lambda) {
  data <- replicate(5000, z_ci(n, lambda))
  prop <- data[which(data == 1)]
  return (length(prop) / 5000)
}

z_prop(5, 0.01)

