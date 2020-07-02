library(forecast)
library(fpp)
library(smooth)
library(tseries)

coke <- CocaCola_Sales_Rawdata$Sales
coke <- as.ts(coke)
View(coke)
class(coke)
coke1 <- ts(coke, start = c(1986,1),end = c(1995,6),frequency = 4)
start(coke1)
summary(coke1)

decom_data <- decompose(coke1, "multiplicative")
plot(decom_data)  
  
plot(decom_data$seasonal)

cycle(coke1)

newmodel <- auto.arima(coke1)
newmodel

auto.arima(coke1, ic="aic", trace = TRUE)
plot.ts(newmodel$residuals)

feat_forecast <- forecast(newmodel, level = c(95), h=10*12)
plot(feat_forecast)

Box.test(newmodel$residuals, lag = 5)

  
  