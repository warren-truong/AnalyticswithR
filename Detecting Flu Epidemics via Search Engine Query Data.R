#Load data
FluTrain = read.csv("FluTrain.csv")
FluTest = read.csv("FluTest.csv")

#Visualising the data (Dependent variable is ILI)
hist(FluTrain$ILI)

#Since the data is skewed right, will plot the natural log of the dependent variable rather than the dependent variable.
plot(FluTrain$Queries,log(FluTrain$ILI))

#Since there was a positive linear relationship between log(dependent variable) and the independent variable
FluTrend1 = lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)

#For a single variable linear regression model, there is a direct relationship between 
the R-squared and the correlation between the independent and the dependent variables. 

a = cor(log(FluTrain$ILI),FluTrain$Queries)

#It appears that Correlation^2 is equal to the R-squared value. It can be proved that this is always the case. 

#Normally, generate predictions of ILI like this on the new data, this will output predictions for log(ILI)
PredTest1 = predict(FluTrend1, newdata = FluTest)

#Want ILI instead. So take the exponential of the logarithm.
PredTest1 = exp(predict(FluTrend1, newdata = FluTest))

#Estimate the percentage of ILI-related physician visits for the week of March 11 2012
which(FluTest$Week == "2012-03-11 - 2012-03-17")
PredTest1[11]

#Relative error between the estimate and observed value for March 11, 2012.
#(Observed ILI - Estimated ILI)/Observed ILI

(FluTest$ILI[11] - PredTest1[11])/(FluTest$ILI[11])

#Root Mean Squared Error of Test Set

SSE = sum((PredTest1 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))

#Training a Time Series Model 

#This can be done with the zoo package.
install.packages("zoo")
library(zoo)

#Create a new variable, ILILag2 which uses the value of ILI 2 weeks prior as its value.
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad = TRUE)
FluTrain$ILILag2 = coredata(ILILag2)

summary(ILILag2)
#Determine if there is a relationship between the ILI and ILILag2 variable.
plot(log(ILILag2), log(FluTrain$ILI))

#Create a new statistical model with the new variable, ILILag2
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)

#Evaluating the time-series model in the test set. First create new ILILag2 variable on test set. 
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad = TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest)

#Filling in the missing data. FluTrain to FluTest is sequential. Therefore: 
FluTest$ILILag2[1] = FluTrain$ILI[416] 
FluTest$ILILag2[2] = FluTrain$ILI[417]
str(FluTest)

#Test set predictions on FluTrend2 

PredTest2 = exp(predict(FluTrend2, newdata = FluTest))

SSE = sum((PredTest2 - FluTest$ILI)^2)
RMSE2 = sqrt(SSE/nrow(FluTest))

#?arima

