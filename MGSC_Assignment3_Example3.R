#MGSC 661 - Assignment 1 (Example 3)

#You can find all similar codes from the inclass lab

#Instruction: 
#You may replace the ***** in each line
#Please submit the R script together with the word doc


library(gridExtra)
library(smooth)
library(fpp2)

# Q1
usdata = read.csv("USchange data.csv")


# Q2 
consumption=usdata[,"Consumption"]  # this step extracts a column from the excel

tsdata = ts(consumption, start = c(1970,1), freq = 4) #hint: choose between freq=4 and freq=12?


# Q3
train_data = window(tsdata, end = c(2012,4))
test_data  = window(tsdata, start = c(2013,1))

# Q4
us.naive = naive(train_data, h = length(test_data))
us.ave   = meanf(train_data, h = length(test_data))
us.drift  = rwf(train_data, drift=TRUE, h=length(test_data))
us.sma   = sma(train_data, order = 4, h = length(test_data))
us.ses   = ses(train_data, h = length(test_data))
us.ets   = forecast(ets(train_data), h = length(test_data))
us.arma  = forecast(auto.arima(train_data, d = 0, seasonal = FALSE), h = length(test_data))
us.arima = forecast(auto.arima(train_data, seasonal = FALSE), h = length(test_data))
us.sarima = forecast(auto.arima(train_data), h = length(test_data))

accuracy(us.naive$mean, test_data)
accuracy(us.ave$mean, test_data)
accuracy(us.drift$mean, test_data)
accuracy(us.sma$forecast, test_data)
accuracy(us.ses$mean, test_data)
accuracy(us.ets$mean, test_data)
accuracy(us.arma$mean, test_data)
accuracy(us.arima$mean, test_data)
accuracy(us.sarima$mean, test_data)





