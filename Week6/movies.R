movies <- read.table('u.item',header=F, sep='|', quote='\"')
str(movies)
head(movies,2)

colnames(movies) <- c("ID","Title","ReleaseDate","VideoReleaseDate", "IMDB", 
                      "Unknown", "Action", "Adventure","Animation", "Childrens", "Comedy", 
                      "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror",
                      "Musical", "Mystery","Romance","SciFi","Thriller", "War", "Western")
str(movies)

movies$ID <- NULL
movies$ReleaseDate <- NULL
movies$VideoReleaseDate <- NULL
movies$IMDB <- NULL

str(movies)

movies <- unique(movies)

str(movies)

sum(movies$Comedy)
sum(movies$Western)
sum(movies$Romance == 1 & movies$Drama == 1)

#-----------------------------------------------------

distances <- dist(movies[2:20], method="euclidean")
clustermovies <- hclust(distances,method="ward.D")

plot(clustermovies)

clustergroups <- cutree(clustermovies, k=10)

tapply(movies$Action, clustergroups, mean)
tapply(movies$Romance, clustergroups, mean)

subset(movies, Title == "Men in Black (1997)")
clustergroups[257]

cluster2 <- subset(movies, clustergroups == 2)
cluster2$Title[1:10]


clustergroups2 <- cutree(clustermovies, k=2)
table(clustergroups2)

subset(movies, clustergroups2 == 2)[1:5,]
