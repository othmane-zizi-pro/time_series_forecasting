#MGSC 661 - Assignment 1 (Example 3)

#You can find all similar codes from the inclass lab

#Instruction: 
#You may replace the ***** in each line
#Please submit the R script together with the word doc


library(gridExtra)
library(smooth)
library(fpp2)

# Q1
usdata = read.csv(****)



# Q2 
consumption=tsdata[,"Consumption"]  # this step extracts a column from the excel

tsdata = ts(****, start = c(1970,1)，freq = *****) #hint: choose between freq=4 and freq=12?


# Q3
train_data = window(****, end = ****)
test_data  = window(****, start = ****)

# Q4
us.naive  = naive(train_data, h=nrow(test_data))
us.ave    = meanf(****, ****) 
us.drift  = rwf(****, ****)
us.sma    = sma(****, ****)
us.ses    = ses(****, ****)
us.ets    = forecast(****, ****)
us.arma   = forecast(auto.arima(****,d=****,seasonal=FALSE), h=nrow(test_data))
us.arima  = forecast(auto.arima(****,seasonal=FALSE), h=****)
us.sarima = forecast(auto.arima(****), h=****)

accuracy(us.naive$mean, test_data)
accuracy(****, ****)
accuracy(****, ****)
accuracy(****, ****)
accuracy(****, ****)
accuracy(****, ****)
accuracy(****, ****)
accuracy(****, ****)
accuracy(****, ****)




