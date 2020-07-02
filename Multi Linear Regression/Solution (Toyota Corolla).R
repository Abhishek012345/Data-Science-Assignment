Toy_corr <- read.csv(file.choose())
View(Toy_corr)
data = subset(Toy_corr, select = -c(Id,Model,Fuel_Type,Color,Mfg_Month,Mfg_Year,Fuel_Type,Met_Color,Color,Automatic,Cylinders,Mfr_Guarantee,BOVAG_Guarantee,Guarantee_Period,ABS,Airbag_1,Airbag_2,Airco,Automatic_airco,Boardcomputer,CD_Player,Central_Lock,Powered_Windows,Power_Steering,Radio,Mistlamps,Sport_Model,Backseat_Divider,Metallic_Rim,Radio_cassette,Tow_Bar))
View(data)
summary(data)

########### Exploring More EDA Parts ##############
attach(Toy_corr)
qqnorm(Price)
qqline(Price)

plot(Price, Age_08_04)
pairs(Toy_corr)
cor(Price, Age_08_04)
hist(Price)
Boxplot(Price)


Corolla_Model <- lm(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = Toy_corr)
summary(Corolla_Model)

library(car)
vif(Corolla_Model) 

avPlots(Corolla_Model)

library(MASS)
stepAIC(Corolla_Model)

Corolla_Model_final <- lm(Price ~ Age_08_04+KM+HP+log(cc)+Gears+Quarterly_Tax+Weight,data = Toy_corr)
summary(Corolla_Model_final)
