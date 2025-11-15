# MGSC 661 Lec10_Part2  

library(gridExtra)
library(smooth)
library(fpp2)



######################################################
#
# ARMA
#
######################################################

ausbeer

aus_train = window(ausbeer, end = c(2003,4))
aus_test  = window(ausbeer, start = c(2004,1))

fit.arma = auto.arima(aus_train, d=0, seasonal=FALSE)

summary(fit.arma)

autoplot(aus_train) +
  autolayer(fitted(fit.arma), series="ARMA")

aus.naive = naive(aus_train, h=length(aus_test))
aus.ave   = meanf(aus_train, h=length(aus_test)) 
aus.drift = rwf(aus_train, drift=TRUE, h=length(aus_test))
aus.sma   = sma(aus_train, h=length(aus_test))
aus.ses   = ses(aus_train, h=length(aus_test))
aus.ets   = forecast(aus_train, h=length(aus_test))

aus.arma  = forecast(fit.arma, h=length(aus_test))

aus.arma

autoplot(ausbeer) +
  autolayer(aus.naive$mean, series="Naive") +
  autolayer(aus.ave$mean, series="Average") +
  autolayer(aus.drift$mean, series="Drift") +
  autolayer(aus.sma$forecast, series="SMA") +
  autolayer(aus.ses$mean, series="SES") +
  autolayer(aus.ets$mean, series="ETS") +
  autolayer(aus.arma$mean, series="ARMA") 

autoplot(window(ausbeer, start = c(2000,1))) +
  autolayer(aus.naive$mean, series="Naive") +
  autolayer(aus.ave$mean, series="Average") +
  autolayer(aus.drift$mean, series="Drift") +
  autolayer(aus.sma$forecast, series="SMA") +
  autolayer(aus.ses$mean, series="SES") +
  autolayer(aus.ets$mean, series="ETS") +
  autolayer(aus.arma$mean, series="ARMA")

autoplot(forecast(fit.arma, h=200)$mean)

checkresiduals(aus.naive)
checkresiduals(aus.ave)
checkresiduals(aus.drift)
checkresiduals(aus.sma)
checkresiduals(aus.ses)
checkresiduals(aus.ets)
checkresiduals(aus.arma)

accuracy(aus.naive$mean, aus_test)
accuracy(aus.ave$mean, aus_test)
accuracy(aus.drift$mean, aus_test)
accuracy(aus.sma$forecast, aus_test)
accuracy(aus.ses$mean, aus_test)
accuracy(aus.ets$mean, aus_test)
accuracy(aus.arma$mean, aus_test)


######################################################
#
# ARIMA 
#
######################################################

fit.arima = auto.arima(aus_train, seasonal=FALSE)
fit.sarima = auto.arima(aus_train)

summary(fit.arima)
summary(fit.sarima)

aus.arima  = forecast(fit.arima, h=length(aus_test))
aus.sarima = forecast(fit.sarima, h=length(aus_test)) 

autoplot(window(ausbeer, start = c(2000,1))) +
  autolayer(aus.arma$mean, series="ARMA") +
  autolayer(aus.arima$mean, series="ARIMA") +
  autolayer(aus.sarima$mean, series="SARIMA")

checkresiduals(aus.arima)
checkresiduals(aus.sarima)

accuracy(aus.arima$mean, aus_test)
accuracy(aus.sarima$mean, aus_test)

