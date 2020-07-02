Com_data <- read.csv(file.choose())
attach(Com_data)

View(Com_data)
qqnorm(price)
qqline(price)
summary(Com_data)
hist(price)

plot(speed, price) 
pairs(Com_data) 

cor(speed, price)
cor(hd, price)
cor(ram, price)
cor(screen, price)
cor(ads, price)
cor(trend, price)
cor(Com_data)


model.Com_data <- lm(price~speed+hd+ram+screen+ads+trend, data = Com_data)
summary(model.Com_data)

model.Com_dataA <- lm(price~speed)
summary(model.Com_dataA)

model.Com_dataB <- lm(price~hd)
summary(model.Com_dataB)

model.Com_dataG <- lm(price~speed+hd)
summary(model.Com_dataG)

library(car)
vif(model.Com_data)

library(MASS)
stepAIC(model.Com_data)

library(car)
plot(model.Com_data)

influenceIndexPlot(model.Com_data)

model.Com_dataFnl <- lm(price~speed+hd+ram+screen+ads+trend, data = Com_data[-1701,] [-1441,])
summary(model.Com_dataFnl)
