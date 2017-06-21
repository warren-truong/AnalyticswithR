CPS = read.csv("CPSData.csv")
str(CPS)
summary(CPS)
table(CPS$Industry)

sort(table(CPS$State)) 
table(CPS$Citizenship)

table(CPS$Race, CPS$Hispanic)

table(CPS$Age,is.na(CPS$Married))

table(CPS$State,is.na(CPS$MetroAreaCode))
table(CPS$Region,is.na(CPS$MetroAreaCode))

#which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
sort(tapply(is.na(CPS$MetroAreaCode),CPS$State,mean))


MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryofBirthCode = read.csv("CountryCodes.csv")

#Mapping code to actual value of the variable
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)


sort(tapply(CPS$Hispanic,CPS$MetroArea, mean))

sort(tapply(CPS$Race =="Asian",CPS$MetroArea,mean))

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE))


#inner join

CPS = merge(CPS,CountryofBirthCode, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

tapply(CPS$Country!="United States", CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA", na.rm=TRUE, mean)

table(CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA",CPS$Country!="United States")

sort(table(CPS$Country=="India",CPS$MetroArea))

sort(tapply(CPS$Country=="India",CPS$MetroArea,sum,na.rm=TRUE))

sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=TRUE))

sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm=TRUE))

sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE))
