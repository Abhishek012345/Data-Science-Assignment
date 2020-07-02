Calor_consmd <- read.csv(file.choose())
View(Calor_consmd)
Calor_consmd
range(Calor_consmd$Calories.Consumed)

library(lattice)
summary(Calor_consmd)

hist(Calor_consmd$Calories.Consumed)
hist(Calor_consmd$Weight.gained..grams)

qqnorm(Calor_consmd$Calories.Consumed)
qqline(Calor_consmd$Calories.Consumed)

attach(Calor_consmd)
cor(Weight.gained..grams., Calories.Consumed)

reg <- lm(Calories.Consumed~Weight.gained..grams., data=Calor_consmd)
summary(reg)

pred <- predict(reg,interval="predict")
pred <- as.data.frame(pred)
pred

reg_sqrt <- lm(Calories.Consumed~sqrt(Weight.gained..grams.), data=Calor_consmd)
summary(reg_sqrt)

plot(reg_sqrt)

reg_sqrtFnl <- lm(Calories.Consumed~Weight.gained..grams., data = Calor_consmd[-8,] [-7,])
summary(reg_sqrtFnl)
