#Each row (observation) in our dataset represents a unique household.  The dataset contains the following variables:
# NumVisits = the number of times the household visited the retailer 
# AvgProdCount = the average number of products purchased per transaction
# AvgDiscount = the average discount per transaction from coupon usage
# AvgSalesValue = the average sales value per transaction
# MorningPct = the percentage of visits in the morning (8am - 1:59pm)
# AfternoonPct = the percentage of visits in the afternoon (2pm - 7:59pm)

hh <- read.csv('households.csv')

require(caret)
hh.preproc <- preProcess(hh)
hh.norm <- predict(hh.preproc, hh)

summary(hh.norm)

set.seed(200)
distances <- dist(hh.norm, method="euclidean")
clustered <- hclust(distances, method = "ward.D")
plot(clustered, labels = FALSE)

set.seed(200)
k <- 10
hh.kmc <- kmeans(hh.norm, centers=k)
sort(table(hh.kmc$cluster))

hh.kmc.cc1 <- hh[hh.kmc$cluster == 1,]
hh.kmc.cc2 <- hh[hh.kmc$cluster == 2,]
hh.kmc.cc3 <- hh[hh.kmc$cluster == 3,]
hh.kmc.cc4 <- hh[hh.kmc$cluster == 4,]
hh.kmc.cc5 <- hh[hh.kmc$cluster == 5,]
hh.kmc.cc6 <- hh[hh.kmc$cluster == 6,]
hh.kmc.cc7 <- hh[hh.kmc$cluster == 7,]
hh.kmc.cc8 <- hh[hh.kmc$cluster == 8,]
hh.kmc.cc9 <- hh[hh.kmc$cluster == 9,]
hh.kmc.cc10 <- hh[hh.kmc$cluster == 10,]


tail(sort(colMeans(hh.kmc.cc1)))
tail(sort(colMeans(hh.kmc.cc2)))
tail(sort(colMeans(hh.kmc.cc3)))
tail(sort(colMeans(hh.kmc.cc4)))
tail(sort(colMeans(hh.kmc.cc5)))
tail(sort(colMeans(hh.kmc.cc6)))
tail(sort(colMeans(hh.kmc.cc7)))
tail(sort(colMeans(hh.kmc.cc8)))
tail(sort(colMeans(hh.kmc.cc9)))
tail(sort(colMeans(hh.kmc.cc10)))


#---------------------------------------------------
set.seed(5000)
k2 <- 5
hh.kmc2 <- kmeans(hh.norm, centers=k2)
sort(table(hh.kmc2$cluster))

#------------------------------------------
hh2 <- hh
hh2$cluster <- hh.kmc$cluster

boxplot(hh2$NumVisits ~ hh2$cluster)
