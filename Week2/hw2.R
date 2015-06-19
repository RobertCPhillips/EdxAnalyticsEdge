#Each row in the datasets pisa2009train.csv and pisa2009test.csv represents one student taking the exam. The datasets have the following variables:
# - grade: The grade in school of the student (most 15-year-olds in America are in 10th grade)
# - male: Whether the student is male (1/0)
# - raceeth: The race/ethnicity composite of the student
# - preschool: Whether the student attended preschool (1/0)
# - expectBachelors: Whether the student expects to obtain a bachelor's degree (1/0)
# - motherHS: Whether the student's mother completed high school (1/0)
# - motherBachelors: Whether the student's mother obtained a bachelor's degree (1/0)
# - motherWork: Whether the student's mother has part-time or full-time work (1/0)
# - fatherHS: Whether the student's father completed high school (1/0)
# - fatherBachelors: Whether the student's father obtained a bachelor's degree (1/0)
# - fatherWork: Whether the student's father has part-time or full-time work (1/0)
# - selfBornUS: Whether the student was born in the United States of America (1/0)
# - motherBornUS: Whether the student's mother was born in the United States of America (1/0)
# - fatherBornUS: Whether the student's father was born in the United States of America (1/0)
# - englishAtHome: Whether the student speaks English at home (1/0)
# - computerForSchoolwork: Whether the student has access to a computer for schoolwork (1/0)
# - read30MinsADay: Whether the student reads for pleasure for 30 minutes/day (1/0)
# - minutesPerWeekEnglish: The number of minutes per week the student spend in English class
# - studentsInEnglish: The number of students in this student's English class at school
# - schoolHasLibrary: Whether this student's school has a library (1/0)
# - publicSchool: Whether this student attends a public school (1/0)
# - urban: Whether this student's school is in an urban area (1/0)
# - schoolSize: The number of students in this student's school
# - readingScore: The student's reading score, on a 1000-point scale

training <- read.csv('pisa2009train.csv')
test <- read.csv('pisa2009test.csv')

tapply(training$readingScore,training$male,mean)
summary(training)

sapply(training, function(x) sum(is.na(x)) )

pisaTrain <- na.omit(training)
pisaTest <- na.omit(test)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore <- lm(readingScore~., data=pisaTrain)
summary(lmScore)
sqrt(mean(lmScore$residuals^2))

predTest <- predict(lmScore,newdata=pisaTest)
max(predTest) - min(predTest)
sum((pisaTest$readingScore - predTest)^2)
sqrt(mean((pisaTest$readingScore - predTest)^2))

mean(pisaTrain$readingScore)
predTest.sst <- sum((mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
predTest.sse <- sum((pisaTest$readingScore - predTest)^2)
predTest.rsq <- 1 - predTest.sse/predTest.sst

