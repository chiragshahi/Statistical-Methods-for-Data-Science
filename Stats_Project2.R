roadrace = read.csv("/Users/chiragshahi/Desktop/roadrace.csv")
summary(roadrace)

maine <- table(roadrace$Maine)
barplot(maine, main="Maine")

home <- roadrace[roadrace$Maine == "Maine",]
away <- roadrace[roadrace$Maine == "Away",]

hist_home <- hist(home$Time..minutes., col='blue', border=F, xlab = "Green: Away | Blue: Home", main = "Runners' times (in minutes)")
hist_away <- hist(away$Time..minutes., col='green', border=F, add=T)

summary(home)
summary(away)

summary(home$Time..minutes.)
summary(away$Time..minutes.)

sd(home$Time..minutes.)
sd(away$Time..minutes.)

timeRange_home <- max(home$Time..minutes.) - min(home$Time..minutes.)
timeRange_home

timeRange_away <- max(away$Time..minutes.) - min(away$Time..minutes.)
timeRange_away

IQR(home$Time..minutes.)
IQR(away$Time..minutes.)

boxplot(home$Time..minutes., away$Time..minutes., names=c('Home', 'Away'), horizontal = TRUE, xlab='Minutes', main="Home & Away Runners' times")

male <- roadrace[roadrace$Sex == 'M',]
female <- roadrace[roadrace$Sex == 'F',]
boxplot(male$Age, female$Age, names = c('Male', 'Female'), horizontal = TRUE, xlab = 'Years', main = "Runners' ages")

summary(female$Age)
summary(male$Age)

sd(female$Age)
sd(male$Age)

ageRange_female <- max(female$Age) - min(female$Age)
ageRange_female

ageRange_male <- max(male$Age) - min(male$Age)
ageRange_male

IQR(female$Age)
IQR(male$Age)




motorcycle <- read.csv("/Users/chiragshahi/Desktop/motorcycle.csv")

boxplot(motorcycle$Fatal.Motorcycle.Accidents, horizontal = TRUE, main = 'Accident data: South Carolina (in 2009)', xlab = '# of accidents')

summary(motorcycle$Fatal.Motorcycle.Accidents)


