#MGSC 661 - Assignment 1 (Example 1)

#You can find all similar codes from the inclass labs

#Instruction:
#You only need to replace the ***** in each line
#Please submit the R script together with the word doc


library(fpp2)


#Q1 (2 points)----------------------------------------------------------------------------------

bikedata = read.csv("bikesharing.csv")
number_of_rows=nrow(bikedata)


#Q2 (2 points)----------------------------------------------------------------------------------
tsbike = ts(bikedata$trip,frequency = 1)



#Q3 (4points) ----------------------------------------------------------------------------------

autoplot(tsbike) +
  ylab("Trips") +
  ggtitle("Daily Bluebike Trips")



#Q4 (8 points) ----------------------------------------------------------------------------------

ggAcf(tsbike, lag.max=200)


#Q5 (8 points)
 ggAcf(tsbike, lag.max=800)



#Q6 (4 points) ----------------------------------------------------------------------------------

 ggPacf(tsbike, lag.max = 100)


#Q7 (20 points) ----------------------------------------------------------------------------------

#(a)
train_data = window(tsbike, end = 1400)
test_data = window(tsbike, start = 1401)


#(b)

naive_bike = naive(train_data, h=24, level= c(80, 99) )
naive_bike


#(c)
autoplot(train_data) +
  autolayer(naive_bike, series="Naive")


#(d)

autoplot(tsbike) + 
  autolayer(naive_bike, series="Naive") + 
  autolayer(test_data, series="Test", linetype="dashed", color="black")

#(e).
checkresiduals(naive_bike)

#(f)
accuracy(naive_bike, test_data)

