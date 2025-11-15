# MGSC 661 Lecture 9 R-script

#set working directory


2+3 # addition

2-3 # subtraction

2*3 # multiplication

2/3 # division

2^3 # exponentiation

2*3-4 # combined multiplication and subtraction

(2+3)^2/4 # combined addition, exponentiation, and division

log(100) # natural logarithm of 100

log(100, base=2) # logarithm with base 2

exp(3) # e^3

1:4 # integer sequence

4:1 # decreasing sequence

seq(1,4) # equivalent to 1:4

seq(2,8, by=2) # specify the interval between elements

seq(0,1, length=11) # specify the number of elements

x=6 # assigning 6 to variable x

y=2 # assigning 2 to variable y

x+y # adding x and y

z = c(1,3,4,6,7) # assigning a sequence (vector) to z

2*z # multiplying z with 2

########################################
# Create a time series                #
########################################

obs=c(12,34,27,17,20) # creating a sequence of observation

tsdata = ts(obs, start=2001) # turning obs into a ts object

tsdata

########################################
# Armtrack data                        #
########################################

amt.data = read.csv("Amtrak data.csv")

amt.data

ncol(amt.data)

nrow(amt.data)

names(amt.data)

amt.data$Month # accessing only the month column

amt.data$Ridership # accessing only the ridership column

summary(amt.data)

tsridership = ts(amt.data$Ridership, start=c(1991,1), end=c(2004,3), freq=12)

tsridership

# Try the following and see what happens:
#ts(amt.data$Ridership, start=c(1991,1), freq=12) # monthly
#ts(amt.data$Ridership, start=c(1991,1), freq=4) # quarterly
#ts(amt.data$Ridership, start=c(1991,1), freq=1) # annual



#------- plot

plot(tsridership, xlab="Time", ylab="Ridership", main="Monthly Amtrak Ridership")

install.packages("fpp2")

library(fpp2)

########################################

autoplot(tsridership) +
  ggtitle("Monthly Amtrak Ridership") +
  xlab("Time") +
  ylab("Ridership")

# Try also:
autoplot(window(tsridership, start=2000)) +
ggtitle("Monthly Amtrak Ridership") +
xlab("Time") +
ylab("Ridership")

autoplot(window(tsridership, start=c(2000,3))) +
ggtitle("Monthly Amtrak Ridership") +
xlab("Time") +
ylab("Ridership")

autoplot(window(tsridership, start=c(2000,3), end=2002)) +
ggtitle("Monthly Amtrak Ridership") +
xlab("Time") +
ylab("Ridership")

ggseasonplot(tsridership, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Ridership") +
  ggtitle("Seasonal plot for Amtrak ridership")

# Also try the following:
ggseasonplot(tsridership, year.labels=FALSE, year.labels.left=FALSE) +
ylab("Ridership") +
ggtitle("Seasonal plot for Amtrak ridership")


ggseasonplot(tsridership, polar=TRUE) +
  ylab("Ridership") +
  ggtitle("Seasonal plot for Amtrak ridership")

ggsubseriesplot(tsridership) +
  ylab("Ridership") +
  ggtitle("Seasonal subseries plot for Amtrak ridership")

#Decomposition

add.decomp = decompose(tsridership, "additive")

autoplot(add.decomp)

add.decomp$seasonal

add.decomp$trend

add.decomp$random

# Is the random component really random? How can we tell?
#autoplot(add.decomp$random)
#ggAcf(add.decomp$random)
#ggAcf(add.decomp$random, plot=FALSE)


#######################################

forecast(tsridership, h=8)

autoplot(forecast(tsridership, h=8))

# Try
autoplot(forecast(tsridership, h=8, level = c(80,99)))

summary(forecast(tsridership, h=8))


########################################
# Armtrack data (beautiful plots)      #
########################################
install.packages('plotly')
library(plotly)
install.packages('dygraphs')
library(dygraphs)


#------Interactive Plots
p=autoplot(tsridership)+
  ggtitle("Monthly Armtrack Ridership")+
  xlab("Time")+
  ylab("Ridership")

ggplotly(p)


dyRangeSelector(dygraph(tsridership))

#------Extract Subset of Data
subset(tsridership, month=5)
subset(tsridership, month="May")


#---- Save Results to Excel
tsforecast=forecast(tsridership,h=8)
autoplot(tsforecast)
summary(tsforecast)

install.packages("writexl")
library(writexl)

forecast_df <- as.data.frame(tsforecast)
write_xlsx(forecast_df, "tsforecast.xlsx")
