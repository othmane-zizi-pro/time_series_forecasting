
#MGSC 661 - Assignment 1 (Example 1)

#You can find all similar codes from the inclass labs

#Instruction: 
#You only need to replace the ***** in each line
#Please submit the R script together with the word doc


library(fpp2)


#Q1 (2 points)----------------------------------------------------------------------------------

bikedata = read.csv(*****)
number_of_rows=nrow(*****)

#Q2 (2 points)----------------------------------------------------------------------------------
tsbike = ts(*****,frequency = 1)



#Q3 (4points) ----------------------------------------------------------------------------------

autoplot(*****) +
  ylab(*****) +
  ggtitle(*****)



#Q4 (8 points) ----------------------------------------------------------------------------------

ggAcf(*****, lag.max=*****)

 
#Q5 (8 points)
 ggAcf(*****, lag.max=*****)



#Q6 (4 points) ----------------------------------------------------------------------------------

ggPacf(*****, lag.max = *****)

 
#Q7 (20 points) ----------------------------------------------------------------------------------

#(a)
train_data = window(*****, end = *****)
test_data = window(*****, start = *****)


#(b)

naive_bike = naive(*****, h=*****, level= ***** )
naive_bike


#(c)	
autoplot(*****) +
  autolayer(*****, series="Naive")


#(d) 
autoplot(*****) +
  autolayer(*****, series="Naive")

#(e).	
checkresiduals(*****)

#(f)
accuracy(*****, *****)
 


