Salry_data <- read.csv(file.choose())
View(Salry_data)

hist(Salry_data$Salary)

library(lattice)
qqnorm(Salry_data$YearsExperience)
qqline(Salry_data$YearsExperience)

attach(Salry_data)
cor(YearsExperience,Salary)

reg <- lm(Salary~YearsExperience , data = Salry_data)
summary(reg)

pred <- predict(reg, interval = "predict")
pred <- as.data.frame(pred)
pred

reg_sqrt <- lm(Salary~sqrt(YearsExperience) , data = Salry_data)
summary(reg_sqrt)


reg_cbrt <- lm(Salary~YearsExperience+ I(YearsExperience^2) + I(YearsExperience^3), data = Salry_data)
summary(reg_cbrt)





