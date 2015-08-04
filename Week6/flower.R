flower <- read.csv('flower.csv', header = F)
head(flower,1)
str(flower)

flowermatrix <- as.matrix(flower)
str(flowermatrix)

flowervector <- as.vector(flowermatrix)

distance <- dist(flowervector, method = 'euclidean')

clusterIntensity <- hclust(distance, method='ward.D')
plot(clusterIntensity)

rect.hclust(clusterIntensity, k=3, border='red')

flowerclusters <- cutree(clusterIntensity, k=3)

table(flowerclusters)

tapply(flowervector, flowerclusters, mean)

dim(flowerclusters) <- c(50,50)

image(flowerclusters)
image(flowermatrix, col = grey(seq(0,1,length=256)))
