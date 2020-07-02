getwd()
Start_up <- read.csv(file.choose())
View(Start_up[-4])
attach(Start_up)

qqnorm(Profit)
qqline(Profit)
summary(Start_up)
hist(R.D.Spend)

plot(R.D.Spend, Profit) 
plot(Administration, Profit)
pairs(Start_up) 
cor(R.D.Spend, Profit)
cor(Administration, Profit)
cor(Marketing.Spend, Profit)
plot(Marketing.Spend, Profit)

model.Start_up <- lm(Profit~R.D.Spend+Administration+Marketing.Spend)
summary(model.Start_up)

model.Start_upV <- lm(Profit~R.D.Spend)
summary(model.Start_upV)

model.Start_upW <- lm(Profit~Administration)
summary(model.Start_upW)

model.Start_upY <- lm(Profit~Administration+R.D.Spend)
summary(model.Start_upY)


library(car)
vif(model.Start_up) 

library(MASS)
stepAIC(model.Start_up)


library(car)
plot(model.Start_up) 

influenceIndexPlot(model.Start_up)

model.Start_upfnl <- lm(Profit~R.D.Spend+Administration+Marketing.Spend, data = Start_up[-50,])
summary(model.Start_upfnl)





