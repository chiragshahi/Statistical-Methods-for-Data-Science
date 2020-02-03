library(boot)
scores <- read.csv("/Users/chiragshahi/Desktop/Project4/gpa.csv")
scores
plot(scores$act, scores$gpa, xlab = "ACT", ylab = "GPA", main = "Scatterplot of GPA against ACT Scores")   

correlation <- function(data,indices) {		#Function to calculate the correlation between GPA & ACT Scores
  result = cor(scores$act[indices], scores$gpa[indices])
  return(result)
}

correlation_boot <- boot(scores, correlation, R = 1000, sim = "ordinary", stype = "i")
#Bootstrap point estimate values function to obtain the point estimate, bias and standard error of GPA & ACT Scores

correlation_boot

mean(correlation_boot$t) - correlation_boot$t0

sd(correlation_boot$t)

plot(correlation_boot)

boot.ci(correlation_boot)

###########

voltages <- read.csv("/Users/chiragshahi/Desktop/Project4/VOLTAGE.csv")
voltages
loc_0 <- voltages$voltage[voltages$location == 0]
loc_0
loc_1 <- voltages$voltage[voltages$location == 1]
loc_1
hist(loc_0, main = "Histogram for voltages at remote locations", xlab = "Voltages")
hist(loc_1, main = "Histogram for voltages at local locations", xlab = "Voltages")
boxplot(loc_0, main = "Boxplot for voltages at remote locations", xlab = "Voltages")
boxplot(loc_1, main = "Boxplot for voltages at local locations", xlab = "Voltages")
boxplot(loc_0, loc_1, main = "Boxplot for voltages at remote locations vs local locations", xlab = "Remote                                          Local")
t.test(loc_0, loc_1, alternative = "two.sided", conf.level = 0.95, var.equal = FALSE)  

###########

dataset3<-read.csv("/Users/chiragshahi/Desktop/Project4/VAPOR.csv")
# Boxplot of the calculated and experimental values 
boxplot(dataset3$theoretical,dataset3$experimental,
        range=1.5,col='red',ylab='Theoretical and experiment values',
        xlab='Vapor pressure values') 
par(mfrow=c(2,1))
qqnorm(dataset3$theoretical, main="Q-Q plot for the theoretical data")
# QQ plot of the values in theoretical values
qqline(dataset3$theoretical) # line plotting theoritical values
qqnorm(dataset3$experimental, main="Q-Q plot for the experimental data")
# QQ plot of the values in theoretical values
qqline(dataset3$experimental) # line plotting theoritical values
par(mfrow=c(2,1)) 
hist(dataset3$theoretical,xlab = "Values of the vapor pressure",main = "Histogram of the theoritical distribution",col = "pink",border = "red")
# Creates histogram for theoretical values
hist(dataset3$experimental,xlab = "Values of the vapor pressure",main = "Histogram of the Experimental distribution",col = "pink",border = "red")
# Creates histogram for theoretical values
dataset3_mean <- dataset3$experimental- dataset3$theoretical
round(abs(mean(dataset3_mean)),5)
# computing confidence interval
diff1 <- dataset3[,3] - dataset3[,2]
alpha <- 0.05
qqnorm(diff1, main="Q-Q plot for the difference in experimental and theoretical data")
qqline(diff1)
n1 <- length(diff1)
ci_diff1 <- mean(diff1) + c(-1, 1)* qnorm(0.975) * sd(diff1)/sqrt(n1)
mean(diff1) + c(-1, 1)* qt(0.975, n1-1) * sd(diff1)/sqrt(n1)
mean(diff1) + c(-1, 1)* qnorm(0.975) * sd(diff1)/sqrt(n1)
ci_diff1
