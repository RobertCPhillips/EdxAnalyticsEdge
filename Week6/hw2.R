#There are seven different variables in the dataset, described below:
#    Balance = number of miles eligible for award travel
#    QualMiles = number of miles qualifying for TopFlight status
#    BonusMiles = number of miles earned from non-flight bonus transactions in the past 12 months
#    BonusTrans = number of non-flight bonus transactions in the past 12 months
#    FlightMiles = number of flight miles in the past 12 months
#    FlightTrans = number of flight transactions in the past 12 months
#    DaysSinceEnroll = number of days since enrolled in the frequent flyer program

airlines <- read.csv('AirlinesCluster.csv')
str(airlines)
head(airlines,5)
summary(airlines)

require(caret)

airlines.preproc <- preProcess(airlines)
airlines.norm <- predict(airlines.preproc, airlines)

summary(airlines.norm)
apply(airlines.norm, 2, sd)

#----------------------------------------

distances <- dist(airlines.norm, method="euclidean")
clustergroups <- hclust(distances,method="ward.D")
plot(clustergroups)

k <- 5
clustergroups2 <- cutree(clustergroups, k=k)
table(clustergroups2) # 776  519  494  868 1342 

tapply(airlines$Balance, clustergroups2, mean)
tapply(airlines$QualMiles, clustergroups2, mean)
tapply(airlines$BonusMiles, clustergroups2, mean)
tapply(airlines$BonusTrans, clustergroups2, mean)
tapply(airlines$FlightMiles, clustergroups2, mean)
tapply(airlines$FlightTrans, clustergroups2, mean)
tapply(airlines$DaysSinceEnroll, clustergroups2, mean)

#------------------------------------------

set.seed(88)
kmc <- kmeans(airlines.norm, centers=k, iter.max = 1000)
table(kmc$cluster) #146 144 277 2063 163 329 308











