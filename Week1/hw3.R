#Demographics and employment in the united states

#  PeopleInHousehold: The number of people in the interviewee's household.
#  Region: The census region where the interviewee lives.
#  State: The state where the interviewee lives.
#  MetroAreaCode: A code that identifies the metropolitan area in which the interviewee lives (missing if the interviewee does not live in a metropolitan area). The mapping from codes to names of metropolitan areas is provided in the file MetroAreaCodes.csv.
#  Age: The age, in years, of the interviewee. 80 represents people aged 80-84, and 85 represents people aged 85 and higher.
#  Married: The marriage status of the interviewee.
#  Sex: The sex of the interviewee.
#  Education: The maximum level of education obtained by the interviewee.
#  Race: The race of the interviewee.
#  Hispanic: Whether the interviewee is of Hispanic ethnicity.
#  CountryOfBirthCode: A code identifying the country of birth of the interviewee. The mapping from codes to names of countries is provided in the file CountryCodes.csv.
#  Citizenship: The United States citizenship status of the interviewee.
#  EmploymentStatus: The status of employment of the interviewee.
#  Industry: The industry of employment of the interviewee (only available if they are employed).

#----------------------------------------
# 1
#----------------------------------------
CPS <- read.csv("CPSData.csv")
summary(CPS)
str(CPS)

sort(table(CPS$Industry))
sort(table(CPS$State))
prop.table(table(CPS$Citizen))
sort(table(subset(CPS, Hispanic == 1)$Race))

#----------------------------------------
# 2
#----------------------------------------
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

table(CPS$State, is.na(CPS$MetroAreaCode))
prop.table(table(CPS$Region, is.na(CPS$MetroAreaCode)),1)
prop.table(table(CPS$State, is.na(CPS$MetroAreaCode)),1)


#----------------------------------------
# 3
#----------------------------------------
MetroAreaMap <- read.csv("MetroAreaCodes.csv")
str(MetroAreaMap)

CPS <- merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
sort(table(CPS$MetroArea))

sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=T))

#----------------------------------------
# 4
#----------------------------------------
CountryMap <- read.csv("CountryCodes.csv")
str(CountryMap)
CPS <- merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)


sort(tapply(CPS$Race == "Asian", CPS$Country, mean))
sort(tapply(!is.na(CPS$Country) & CPS$Country != "United States", CPS$MetroArea, mean))
sort(tapply(!is.na(CPS$Country) & CPS$Country == "India", CPS$MetroArea, sum))
sort(tapply(!is.na(CPS$Country) & CPS$Country == "Brazil", CPS$MetroArea, sum))
sort(tapply(!is.na(CPS$Country) & CPS$Country == "Somalia", CPS$MetroArea, sum))







