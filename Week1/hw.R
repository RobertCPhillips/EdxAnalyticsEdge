#An analytical detective

#  ID: a unique identifier for each observation
#  Date: the date the crime occurred
#  LocationDescription: the location where the crime occurred
#  Arrest: whether or not an arrest was made for the crime (TRUE if an arrest was made, and FALSE if an arrest was not made)
#  Domestic: whether or not the crime was a domestic crime, meaning that it was committed against a family member (TRUE if it was domestic, and FALSE if it was not domestic)
#  Beat: the area, or "beat" in which the crime occurred. This is the smallest regional division defined by the Chicago police department.
#  District: the police district in which the crime occured. Each district is composed of many beats, and are defined by the Chicago Police Department.
#  CommunityArea: the community area in which the crime occurred. Since the 1920s, Chicago has been divided into what are called "community areas", of which there are now 77. The community areas were devised in an attempt to create socially homogeneous regions.
#  Year: the year in which the crime occurred.
#  Latitude: the latitude of the location at which the crime occurred.
#  Longitude: the longitude of the location at which the crime occurred.

data <- read.csv("mvtWeek1.csv")
str(data)

#----------------------------------------
# 1
#----------------------------------------
max(data$ID)
min(data$Beat)
sum(data$Arrest)
sum(data$LocationDescription  == "ALLEY")

#----------------------------------------
# 2
#----------------------------------------
DateConvert <- as.Date(strptime(data$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

data$Month = months(DateConvert)
data$Weekday = weekdays(DateConvert)
data$Date = DateConvert

table(data$Month)
table(data$Weekday)
table(data$Month,data$Arrest)
table(data$Month,data$Arrest)

#----------------------------------------
# 3
#----------------------------------------
hist(data$Date, breaks=100)
boxplot(Date~Arrest,data=data)

data.2001 <- subset(data, Year == 2001)
prop.table(table(data.2001$Arrest))

data.2007 <- subset(data, Year == 2007)
prop.table(table(data.2007$Arrest))

data.2012 <- subset(data, Year == 2012)
prop.table(table(data.2012$Arrest))

#----------------------------------------
# 4
#----------------------------------------
head(sort(table(data$LocationDescription),decreasing=T),6)

top5Locations <- c("ALLEY","GAS STATION","STREET","PARKING LOT/GARAGE(NON.RESID.)","DRIVEWAY - RESIDENTIAL")
dataTop5 <- subset(data, LocationDescription %in% top5Locations)
dataTop5$LocationDescription = factor(dataTop5$LocationDescription)
table(dataTop5$LocationDescription,dataTop5$Arrest)

table(subset(data,LocationDescription == "GAS STATION")$Weekday)
table(subset(data,LocationDescription == "DRIVEWAY - RESIDENTIAL")$Weekday)
