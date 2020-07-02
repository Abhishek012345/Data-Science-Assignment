Delvr_time <- read.csv(file.choose())
View(Delvr_time)
Delvr_time
library(lattice)
summary(Delvr_time)

hist(Delvr_time$Sorting.Time)
hist(Delvr_time$Delivery.Time)

qqnorm(Delvr_time$Delivery.Time)
qqline(Delvr_time$Delivery.Time)

attach(Delvr_time)
cor(Sorting.Time,Delivery.Time)

reg <- lm(Delivery.Time~Sorting.Time, data = Delvr_time)
summary(reg)

pred <- predict(reg, interval = "predict")
pred<- as.data.frame(pred)
pred

reg <- lm(Delivery.Time~sqrt(Sorting.Time), data = Delvr_time)
summary(reg)

plot(reg)

reg_fnl <- lm(Delivery.Time~sqrt(Sorting.Time), data = Delvr_time)
summary(reg_fnl)


