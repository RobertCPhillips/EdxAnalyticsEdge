# Week - The range of dates represented by this observation, in year/month/day format.
# ILI - This column lists the percentage of ILI-related physician visits for the corresponding week.
# Queries - This column lists the fraction of queries that are ILI-related for the corresponding week, adjusted to be between 0 and 1 (higher values correspond to more ILI-related search queries).

fluTrain <- read.csv("flutrain.csv")
fluTest <- read.csv("flutest.csv")
str(fluTrain)

maxILI <- which.max(FluTrain$ILI) # which(fluTrain$ILI == max(fluTrain$ILI))
fluTrain$Week[maxILI]

maxQuery <- which.max(FluTrain$Queries) # which(fluTrain$Queries == max(fluTrain$Queries))
fluTrain$Week[maxQuery]

hist(fluTrain$ILI)
plot(log(ILI)~Queries,data=fluTrain)

fluTrend1 <- lm(log(ILI)~Queries,data=fluTrain)
summary(fluTrend1)
cor(log(fluTrain$ILI),fluTrain$Queries)^2

predTest1 <- exp(predict(fluTrend1, newdata=fluTest))
mar112012 <- which(fluTest$Week == "2012-03-11 - 2012-03-17")
predTest1[mar112012]

#relative error - (Observed ILI - Estimated ILI)/Observed ILI
with(fluTest[mar112012,],(ILI - predTest1[mar112012])/ILI)


sqrt(mean((fluTest$ILI - predTest1)^2))

#time series
require(zoo)

ILILag2 <- lag(zoo(fluTrain$ILI), -2, na.pad=TRUE)
fluTrain$ILILag2 = coredata(ILILag2)

plot(log(ILILag2)~log(ILI), data=fluTrain)

fluTrend2 <- lm(log(ILI)~Queries+log(ILILag2),data=fluTrain)
summary(fluTrend2)

ILILag2b <- lag(zoo(fluTest$ILI), -2, na.pad=TRUE)
fluTest$ILILag2 <- coredata(ILILag2b)
trainRows <- nrow(fluTrain)
fluTest$ILILag2[1] <- fluTrain$ILI[trainRows-1]
fluTest$ILILag2[2] <- fluTrain$ILI[trainRows]

predTest2 <- predict(fluTrend2,newdata=fluTest)

sqrt(mean((fluTest$ILI - exp(predTest2))^2))






