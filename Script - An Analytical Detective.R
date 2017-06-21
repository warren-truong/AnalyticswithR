mvt = read.csv("mvtWeek1.csv")
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
mvt$Month = months(DateConvert)

mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert


TopS = subset(mvt, LocationDescription == "STREET")
TopP = subset(mvt, LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)")
TopA = subset(mvt, LocationDescription == "ALLEY")
TopG = subset(mvt, LocationDescription == "GAS STATION")
TopD = subset(mvt, LocationDescription == "DRIVEWAY - RESIDENTIAL")

Top5 = rbind(TopS,TopP,TopA,TopG,TopD)
str(Top5)

table(Top5$LocationDescription, Top5$Weekday)
Top5$LocationDescription = factor(Top5$LocationDescription)
table(Top5$Weekday, Top5$LocationDescription == "GAS STATION")
N