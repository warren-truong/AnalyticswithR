#Load Data
elantra = read.csv("elantra.csv")
str(elantra)

#split into training and testing sets
ElantraTrain = subset(elantra, Year <=2012)
ElantraTest = subset(elantra, Year > 2012)

#Build linear regression model
linreg = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data = ElantraTrain)
summary(linreg)

monthlysales = lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries, data = ElantraTrain)
summary(linreg)

#Convert month from numeric to factor variable, so we are not restricted to be linear and to capture more complex effects
ElantraTrain$MonthFactor = as.factor(ElantraTrain$Month)
ElantraTest$MonthFactor = as.factor(ElantraTest$Month)
monthlysales2 = lm(ElantraSales ~ MonthFactor + Unemployment + CPI_all + CPI_energy + Queries, data = ElantraTrain)

#Reducing the model
monthlysales3 = lm(ElantraSales ~ MonthFactor + Unemployment + CPI_all + CPI_energy, data = ElantraTrain)

predictTT = predict(monthlysales3, newdata = ElantraTest)
SSE = sum((predictTT - ElantraTest$ElantraSales)^2)
Baseline = mean(ElantraTrain$ElantraSales)
SST = sum((Baseline - ElantraTest$ElantraSales)^2)
Rsquared = 1-SSE/SST

#Largest absolute error made on test set predictions
sort(abs(predictTT - ElantraTest$ElantraSales))

#Check for multicolinearity between variables
cor(ElantraTrain[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])
cor(ElantraTrain[c("Unemployment","Month","Queries","CPI_energy","CPI_all")])

ElantraTest$Month[5]
ElantraTest$Year[5]

pair = abs(predictTT - ElantraTest$ElantraSales)
which.max(pair)

