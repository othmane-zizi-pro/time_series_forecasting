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
autoplot(*****)

#Q2.  
autoplot(decompose(*****, type = *****))


#Q3. 
train_data = window(*****, end=c(2011,12))
test_data = window(*****, start=c(2012,1))

#Q4.  
retail.naive = naive(train_data, h=24)
retail.ave   = meanf(*****, *****) 
retail.drift = rwf(*****, *****)
retail.sma   = sma(*****, *****)
retail.ses   = ses(*****, *****)
retail.ets   = forecast(*****, *****)

autoplot(tsretail) +
  autolayer(retail.naive$mean, series = "Naive") +
  autolayer(*****, series = "Average") +
  autolayer(*****, series = "Drift") +
  autolayer(*****, series = "SMA") +
  autolayer(*****, series = "SES") +
  autolayer(*****, series = "ETS") 



accuracy(*****, *****)
accuracy(*****, *****)
accuracy(*****, *****)
accuracy(*****, *****)
accuracy(*****, *****)
accuracy(*****, *****)
 