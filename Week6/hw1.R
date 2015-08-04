dailykos <- read.csv('dailykos.csv')
str(dailykos)

distances <- dist(dailykos, method="euclidean")
clustermovies <- hclust(distances,method="ward.D")
plot(clustermovies)

k <- 7
clustergroups <- cutree(clustermovies, k=k)
table(clustergroups) #1266  321  374  139  407  714  209 

dailykos.c1 <- dailykos[clustergroups == 1,]
dailykos.c2 <- dailykos[clustergroups == 2,]
dailykos.c3 <- dailykos[clustergroups == 3,]
dailykos.c4 <- dailykos[clustergroups == 4,]
dailykos.c5 <- dailykos[clustergroups == 5,]
dailykos.c6 <- dailykos[clustergroups == 6,]
dailykos.c7 <- dailykos[clustergroups == 7,]

tail(sort(colMeans(dailykos.c1)))
tail(sort(colMeans(dailykos.c2)))
tail(sort(colMeans(dailykos.c3)))
tail(sort(colMeans(dailykos.c4)))
tail(sort(colMeans(dailykos.c5)))
tail(sort(colMeans(dailykos.c6)))
tail(sort(colMeans(dailykos.c7)))


#------------------------------------------

set.seed(1000)
kmc <- kmeans(dailykos, centers=k)
table(kmc$cluster) #146 144 277 2063 163 329 308
str(kmc)

dailykos.cc1 <- dailykos[kmc$cluster == 1,]
dailykos.cc2 <- dailykos[kmc$cluster == 2,]
dailykos.cc3 <- dailykos[kmc$cluster == 3,]
dailykos.cc4 <- dailykos[kmc$cluster == 4,]
dailykos.cc5 <- dailykos[kmc$cluster == 5,]
dailykos.cc6 <- dailykos[kmc$cluster == 6,]
dailykos.cc7 <- dailykos[kmc$cluster == 7,]

tail(sort(colMeans(dailykos.cc1)))
tail(sort(colMeans(dailykos.cc2)))
tail(sort(colMeans(dailykos.cc3)))
tail(sort(colMeans(dailykos.cc4)))
tail(sort(colMeans(dailykos.cc5)))
tail(sort(colMeans(dailykos.cc6)))
tail(sort(colMeans(dailykos.cc7)))

table(clustergroups, kmc$cluster)
