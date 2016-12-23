# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}
Flutrain=read.csv("FluTrain.csv")
str(Flutrain)
which.max(Flutrain$Queries) ## finding the maximum of flu realted queries week
Flutrain$Week[303]
hist(Flutrain$ILI)
## i obseve heavy skewness to the right
 plot(Flutrain$Queries, log(Flutrain$ILI))
## a positive linear relationship is observed
 flutrend1=lm(log(Flutrain$ILI)~Flutrain$Queries,data=Flutrain)
 summary(flutrend1)
 FluTest=read.csv("FluTest.csv")
 PredTest1 = exp(predict(flutrend1, newdata=FluTest))
 str(PredTest1)
 which(FluTest$Week=="2012-03-11 - 2012-03-17")
 PredTest1[11]
 test=(FluTest$ILI-PredTest1[1:52])/FluTest$ILI
 test[11]
 SSE = sum((PredTest1[1:52]-FluTest$ILI)^2)
 RMSE = sqrt(SSE / nrow(FluTest))
 RMSE
 ## using time series anlysis
 library(zoo)
 ILILag2 = lag(zoo(Flutrain$ILI), -2, na.pad=TRUE)

 Flutrain$ILILag2 = coredata(ILILag2)
 plot(log(ILILag2)~log(ILI),data=Flutrain)
 flutrend2=lm(log(Flutrain$ILI)~log(Flutrain$ILILag2)+Flutrain$Queries)
 summary(flutrend2)
 ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
 FluTest$ILILag2=coredata(ILILag2)
 summary(FluTest$ILILag2)
 FluTest$ILILag2[1]=Flutrain$ILI[416]
 FluTest$ILILag2[2]=Flutrain$ILI[417]
 FluTest$ILILag2[1]
 FluTest$ILI
 predictions=exp(predict(flutrend2,newdata = FluTest,na.action = na.exclude))

 summary(predictions)
 predictions[1:52]
 SSE = sum((predictions[1:52]-FluTest$ILI)^2)
 SSE
 nrow(FluTest$ILI)
 RMSE = sqrt(SSE / nrow(FluTest))
 RMSE
