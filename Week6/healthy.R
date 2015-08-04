healthy <- read.csv('healthy.csv', header = F)

healthymatrix <- as.matrix(healthy)
str(healthymatrix)

image(healthymatrix, col=grey(seq(0,1,length=256)))

healthyvector <- as.vector(healthymatrix)
str(healthyvector)

#distance <- dist(healthyvector, method = 'euclidean')

k <- 5
set.seed(1)

kmc <- kmeans(healthyvector, centers=k, iter.max = 1000)
str(kmc)

healthyclusters <- kmc$cluster

table(healthyclusters)
tapply(healthyvector, healthyclusters, mean)
kmc$centers

dim(healthyclusters) <- c(nrow(healthymatrix), ncol(healthymatrix))
image(healthyclusters, col=rainbow(k))

#--------------------------------------
tumor <- read.csv('tumor.csv', header = F)

tumormatrix <- as.matrix(tumor)
str(tumormatrix)
tumorvector <- as.vector(tumormatrix)

require(flexclust)

kmc.kcca <- as.kcca(kmc, healthyvector)

tumorclusters <- predict(kmc.kcca, newdata=tumorvector)
dim(tumorclusters) <- c(nrow(tumormatrix), ncol(tumormatrix))
image(tumorclusters, col=rainbow(k))
