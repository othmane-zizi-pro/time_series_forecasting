# MGSC 661 Class 9 R-script

#install.packages("plotly")
#install.packages("dygraphs")

library(fpp2)
library(plotly)
library(dygraphs)

#------load data and create time series
amt.data = read.csv("Amtrak data.csv")
tsridership = ts(amt.data$Ridership, start=c(1991,1), end=c(2004,3), freq=12)


#------graphs
p = autoplot(tsridership) +
  ggtitle("Monthly Amtrak Ridership") +
  xlab("Time") +
  ylab("Ridership")

ggplotly(p) # this is the plotly version of interactive plot

dyRangeSelector(dygraph(tsridership)) # this is the dygraphs version of interactive plot


# check subset of data
 window(tsridership, start=c(2002,1), end=c(2003,4))
 window(tsridership, start=2002, end=2003)
 window(tsridership, start=2002, end=2003+3/12)


subset(tsridership, month=5) # the number of riderships in May

subset(tsridership, month="May")
 


######################################################
#
# Naive Forecast
#
######################################################

tsforecast = forecast(tsridership, h=8)  #horizon=8

autoplot(tsforecast) 

summary(tsforecast)


 


######################################################
#
# Autocorrelation
#
######################################################


#--------- Amtrack data
gglagplot(tsridership, set.lags = 1, diag.col = "black", do.lines = FALSE)
gglagplot(tsridership, set.lags = 12, diag.col = "black", do.lines = FALSE)


ggAcf(tsridership, plot=FALSE)
ggAcf(tsridership) # if you want plot

ggAcf(tsridership, lag.max=48) #adjust the lag length


#--------- google data
autoplot(goog200) +
  ggtitle("Google stock (daily ending Dec 6, 2013)") +
  xlab("Day") +
  ylab("US$")

ggAcf(goog200, lag.max=100)

#--------- energy data
autoplot(elec) +
  ggtitle("Monthly Australian electricity demand") +
  xlab("Year") +
  ylab("GWh")

ggAcf(elec, lag.max=100)



######################################################
#
# White noise
#
######################################################

#----google stock example
naive(goog200) 
names(naive(goog200))

# You can specify the length of forecast horizon and 
# confidence level for prediction intervals
naive(goog200, h=20, level=c(80,95,99)) 

names(naive(goog200))

#compute residuals
cbind(goog200, naive(goog200)$fitted) # create a matrix with two columns

res = residuals(naive(goog200)) #check the "goog200-naive(goog200)$fitted"

res

# Equivalent to residuals(naive(goog200))
naive(goog200)$residuals 


autoplot(res)
ggAcf(res)

dyRangeSelector(dygraph(res))
checkresiduals(res) 

#-----------ausbeer example: decomposition

aus.decomp = decompose(ausbeer, "additive")

autoplot(aus.decomp)

ggAcf(aus.decomp$random)

checkresiduals(aus.decomp$random)

Box.test(aus.decomp$random, lag=10, type="Lj")



######################################################
#
# Partial autocorrelation
#
######################################################

ggPacf(tsridership, lag.max=48)
ggPacf(goog200, lag.max=100)
ggPacf(elec, lag.max=48)



######################################################
#
# Evaluating forecast accuracy
#
######################################################

#-----set training data
goog.train = window(goog200, end = 180)

#-----forecast using three methods
goog.naive = naive(goog.train, h=20)
goog.ave = meanf(goog.train, h=20) 
goog.drift = rwf(goog.train, drift=TRUE, h=20)

names(goog.naive) #check which results are included 

#-----plot
autoplot(goog200) +
  autolayer(goog.naive, PI=FALSE, series="Naive") +
  autolayer(goog.ave, PI=FALSE, series="Average") +
  autolayer(goog.drift, PI=FALSE, series="Drift") +
  xlab("Day") +
  ylab("US$") +
  ggtitle("Google stock price")

# If lines 142-148 do not work, try the following:
autoplot(goog200) +
  autolayer(goog.naive$mean, series="Naive") +
  autolayer(goog.ave$mean, series="Average") +
  autolayer(goog.drift$mean, series="Drift") +
  xlab("Day") +
  ylab("US$") +
  ggtitle("Google stock price")

#-----set testing data
goog.test = window(goog200, start=181, end=200) # our test data


#-----check residuals (training data)
checkresiduals(goog.naive)
checkresiduals(goog.ave)
checkresiduals(goog.drift)


#-----check accuracy (test data)
accuracy(goog.naive, goog.test)
accuracy(goog.ave, goog.test)
accuracy(goog.drift, goog.test)







