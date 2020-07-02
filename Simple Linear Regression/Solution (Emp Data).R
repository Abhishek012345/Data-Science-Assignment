emp_data <- read.csv(file.choose())
View(emp_data)
emp_data
library(lattice)
summary(emp_data)

hist(emp_data$Salary_hike)
hist(emp_data$Churn_out_rate)

qqnorm(emp_data$Churn_out_rate)
qqline(emp_data$Churn_out_rate)

attach(emp_data)
cor(Salary_hike,Churn_out_rate)

reg <- lm(Churn_out_rate~Salary_hike, data = emp_data)
summary(reg)

pred <- predict(reg, interval = "predict")
pred <- as.data.frame(pred)
pred

reg_sqrt <- lm(Churn_out_rate~sqrt(Salary_hike), data = emp_data)
summary(reg_sqrt)

plot(reg_sqrt)

reg_Fnl <- lm(Churn_out_rate~Salary_hike, data = emp_data[-1,][-2,])
summary(reg_sqrt)


