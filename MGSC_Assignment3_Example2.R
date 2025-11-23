#MGSC 661 - Assignment 1 (Example 2)

#You can find all similar codes from the inclass labs

#Instruction: 
#You may replace the ***** in each line
#Please submit the R script together with the word doc


install.packages("readxl")

library(readxl)
library(fpp2)
library(smooth)


#Load data
retaildata = read_excel("retail.xlsx", skip=1)

#create time series
tsretail = ts(retaildata$A3349462J, start = c(1982,4), freq=12) # liquor sales


#Q1.	 
autoplot(tsretail)

#Q2.  
autoplot(decompose(tsretail, type = "multiplicative"))


#Q3. 
train_data = window(tsretail, end=c(2011,12))
test_data = window(tsretail, start=c(2012,1))

#Q4.  
retail.naive = naive(train_data, h=24)
retail.ave   = meanf(train_data, h=24) 
retail.drift = rwf(train_data, h=24, drift=TRUE)
retail.sma   = sma(train_data, order=12, h=24)
retail.ses   = ses(train_data, h=24)
retail.ets   = forecast(ets(train_data), h=24)

autoplot(tsretail) +
  autolayer(retail.naive$mean, series = "Naive") +
  autolayer(retail.ave$mean, series = "Average") +
  autolayer(retail.drift$mean, series = "Drift") +
  autolayer(retail.sma$forecast, series = "SMA") +
  autolayer(retail.ses$mean, series = "SES") +
  autolayer(retail.ets$mean, series = "ETS") 



accuracy(retail.naive, test_data)
accuracy(retail.ave, test_data)
accuracy(retail.drift, test_data)
accuracy(retail.sma, test_data)
accuracy(retail.ses, test_data)
accuracy(retail.ets, test_data)
 