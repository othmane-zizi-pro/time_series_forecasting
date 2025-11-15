# MGSC 661 Lec10_Part1

 
install.packages("gridExtra")
install.packages("smooth")
install.packages("zoo")

library(gridExtra)
library(smooth)
library(fpp2)
library(zoo)
library(dygraphs)


amt.data = read.csv("Amtrak data.csv")

# Testing decomposition with different frequencies
autoplot(decompose(ts(amt.data$Ridership, freq=3)))
autoplot(decompose(ts(amt.data$Ridership, freq=12)))
autoplot(decompose(ts(amt.data$Ridership, freq=48)))


# findfrequency(amt.data$Ridership)

# Testing Naive and Drift forecasts with different frequencies
naive(ts(amt.data$Ridership, freq=12), h=5)
naive(ts(amt.data$Ridership, freq=48), h=5)
naive(ts(amt.data$Ridership, freq=3), h=5)

rwf(ts(amt.data$Ridership, freq=12), drift=TRUE, h=5)
rwf(ts(amt.data$Ridership, freq=48), drift=TRUE, h=5)
rwf(ts(amt.data$Ridership, freq=3), drift=TRUE, h=5)

# Bikedata
bikedata = read.csv("bikesharing.csv")
tsbike1 = ts(bikedata$trip,frequency = 1)
tsbike365 = ts(bikedata$trip,frequency = 365)
tsbike365.25 = ts(bikedata$trip,frequency = 365.25)

ggAcf(tsbike1, lag.max = 800)
ggAcf(tsbike365, lag.max = 800)
ggAcf(tsbike365.25, lag.max = 800)

decomp1 = decompose(tsbike1, "additive")
decomp365 = decompose(tsbike365, "additive")
decomp365.25 = decompose(tsbike365.25, "additive")

autoplot(decomp1)
autoplot(decomp365)
autoplot(decomp365.25)


autoplot(window(tsbike365, start=1, end=3))
autoplot(window(tsbike365, start=1, end=1+2/365))
autoplot(window(tsbike365.25, start=1, end=3))


######################################################
#
# Simple Smoothing： SMA SES
#
######################################################

tsridership = ts(amt.data$Ridership, start=c(1991,1), freq=12)
tsridership_train = window(tsridership, end=c(2001,12))
tsridership_test  = window(tsridership, start=c(2002,1)) # 27 months

# Fitting SMA
sma5  = sma(tsridership_train, order=5)
sma10 = sma(tsridership_train, order=10)
sma40 = sma(tsridership_train, order=40)

sma5

summary(sma5)

names(sma5)

sma5$fitted

fitted(sma5) # equivalent to sma5$fitted

# Plotting all SMAs in the same graph
autoplot(tsridership_train) +
  autolayer(fitted(sma5), series="sma5") +
  autolayer(fitted(sma10), series="sma10") +
  autolayer(fitted(sma40), series="sma40")

# Plotting SMAs in different graphs but in the same panel
p1 = autoplot(tsridership_train)
p2 = autoplot(fitted(sma5)) 
p3 = autoplot(fitted(sma10))
p4 = autoplot(fitted(sma40))

grid.arrange(p1,p2,p3,p4, ncol=2, nrow=2)

# Fitting SES
ses01 = ses(tsridership_train, alpha = 0.1)
ses05 = ses(tsridership_train, alpha = 0.5)
ses08 = ses(tsridership_train, alpha = 0.8)

# Plotting all SESs in the same graph
autoplot(tsridership_train) +
  autolayer(fitted(ses01), series="ses01") +
  autolayer(fitted(ses05), series="ses05") +
  autolayer(fitted(ses08), series="ses08")

# Plotting SESs in different graphs but in the same panel
p1 = autoplot(tsridership_train)
p2 = autoplot(fitted(ses01))
p3 = autoplot(fitted(ses05))
p4 = autoplot(fitted(ses08))

grid.arrange(p1,p2,p3,p4, ncol=2, nrow=2)

# Forecasting with five different methods
rider.naive = naive(tsridership_train, h=27)
rider.ave   = meanf(tsridership_train, h=27) 
rider.drift = rwf(tsridership_train, drift=TRUE, h=27)
rider.sma   = sma(tsridership_train, h=27)
rider.ses   = ses(tsridership_train, h=27)

summary(rider.sma)

summary(rider.ses)

autoplot(tsridership) +
  autolayer(rider.naive$mean, series="Naive") +
  autolayer(rider.ave$mean, series="Average") +
  autolayer(rider.drift$mean, series="Drift") +
  autolayer(rider.sma$forecast, series="SMA") +
  autolayer(rider.ses$mean, series="SES")


# Checking the residuals
checkresiduals(rider.naive)
checkresiduals(rider.ave)
checkresiduals(rider.drift)
checkresiduals(rider.sma)
checkresiduals(rider.ses)

# Alternative 
names(rider.naive)
Box.test(rider.naive$residuals, lag = 10, type = "Lj")

names(rider.ave)
Box.test(rider.ave$residuals, lag = 10, type = "Lj")

names(rider.drift)
Box.test(rider.drift$residuals, lag = 10, type = "Lj")

names(rider.sma)
Box.test(rider.sma$residuals, lag = 10, type = "Lj")

names(rider.ses)
Box.test(rider.ses$residuals, lag = 10, type = "Lj")


# Checking the accuracies
accuracy(rider.naive$mean, tsridership_test)
accuracy(rider.ave$mean, tsridership_test)
accuracy(rider.drift$mean, tsridership_test)
accuracy(rider.sma$forecast, tsridership_test)
accuracy(rider.ses$mean, tsridership_test)



######################################################
#
# Exponential Smoothing: ETS
#
######################################################

# Fitting ETS
fit.ets = ets(tsridership_train)

summary(fit.ets)

# Plotting the fitted values of the "best" ETS
autoplot(tsridership_train) +
  autolayer(fitted(fit.ets), series="ets") 

# Forecasting with ETS
rider.ets = forecast(fit.ets, h=27)


# Plotting the forecasts
autoplot(tsridership) +
  autolayer(rider.naive$mean, series="Naive") +
  autolayer(rider.ave$mean, series="Average") +
  autolayer(rider.drift$mean, series="Drift") +
  autolayer(rider.sma$forecast, series="SMA") +
  autolayer(rider.ses$mean, series="SES") +
  autolayer(rider.ets$mean, series="ETS") 

# Use more granular display on x and y axis
autoplot(tsridership) +
  autolayer(rider.naive$mean, series="Naive") +
  autolayer(rider.ave$mean, series="Average") +
  autolayer(rider.drift$mean, series="Drift") +
  autolayer(rider.sma$forecast, series="SMA") +
  autolayer(rider.ses$mean, series="SES") +
  autolayer(rider.ets$mean, series="ETS") +
  scale_x_continuous(breaks = 1991:2005) +
  scale_y_continuous(breaks = seq(1000,2250,by=50))

autoplot(window(tsridership, start=2000)) +
  autolayer(rider.naive$mean, series="Naive") +
  autolayer(rider.ave$mean, series="Average") +
  autolayer(rider.drift$mean, series="Drift") +
  autolayer(rider.sma$forecast, series="SMA") +
  autolayer(rider.ses$mean, series="SES") +
  autolayer(rider.ets$mean, series="ETS") +
  scale_x_continuous(breaks = 1991:2005) +
  scale_y_continuous(breaks = seq(1000,2250,by=50))

# Checking the residuals
checkresiduals(rider.ets)



# Checking the accuracy
accuracy(rider.ets$mean, tsridership_test)

# Put the forecasts together and create a csv file
results = data.frame(rider.naive$mean, rider.ave$mean, rider.drift$mean, rider.sma$forecast, rider.ses$mean, rider.ets$mean) 

results

colnames(results) = c("Naive", "Average", "Drift", "SMA", "SES", "ETS") # changing the names of the columns

results

 
