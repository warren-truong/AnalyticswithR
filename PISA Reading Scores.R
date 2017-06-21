pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")
tapply(pisaTrain$readingScore,pisaTrain$male, mean)
hist(pisaTrain$readingScore)

summary(pisaTrain)

#Omit all the missing values
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

#By default, reference level is always the first alphabetical variable and not the most common. 
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore ~ ., data =pisaTrain)
summary(lmScore)

#Calculate training RMSE off model
SSE = sum((lmScore$residuals)^2)
RMSE = sqrt(SSE/nrow(pisaTrain))

#Predicting on unseen data
predTest = predict(lmScore, newdata = pisaTest)
summary(predTest)

SSEtest = sum((predTest - pisaTest$readingScore)^2)

RMSE = sqrt(SSEtest/nrow(pisaTest))

#Computed baseline i.e. average reading score of training data
baseline = mean(pisaTrain$readingScore)

SST = sum((pisaTest$readingScore - baseline)^2)

Rsquared = 1-SSEtest/SST