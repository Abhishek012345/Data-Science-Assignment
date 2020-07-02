library(forecast)

attach(Airlines_Data)

Ap <- Airlines_Data
class(Ap)
summary(Ap)

plot(Ap, ylab= "Passengers (1000's)")
plot(decompose(AirPassengers))

apts <- ts(AirPassengers, frequency=12)
f<- decompose(apts)

library(tseries)
adf.test(apts, alternative ="stationary", k=12)

findbest <- auto.arima(AirPassengers)
findbest

plot(forecast(findbest,h=20))
fit <- arima(AirPassengers, order=c(0, 1, 1), list(order=c(0, 1, 0), period = 12))
fit

fore <- predict(fit, n.ahead=24)
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
ts.plot(AirPassengers, fore$pred, U, L, col=c(1, 2, 4, 4), lty=c(1, 1, 2, 2))

res <- residuals(fit)
acf(res)

qqnorm(residuals(fit))
qqline(residuals(fit))

library(tseries)
adf.test(fit$residuals, alternative = "stationary")