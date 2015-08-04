#The first file, edges.csv, contains variables V1 and V2, which 
#  label the endpoints of edges in our network. Each row represents a pair of 
#  users in our graph who are Facebook friends. For a pair of friends A and B, 
#  edges.csv will only contain a single row -- the smaller identifier will be listed 
#  first in this row.

#The second file, users.csv, contains information about the Facebook users, who are the 
#  vertices in our network. This file contains the following variables:
  
#id: A unique identifier for this user; this is the value that appears in the rows of edges.csv

#gender: An identifier for the gender of a user taking the values A and B. Because the 
#        data is anonymized, we don't know which value refers to males and which value 
#        refers to females.

#school: An identifier for the school the user attended taking the values A and AB 
#        (users with AB attended school A as well as another school B). Because the 
#        data is anonymized, we don't know the schools represented by A and B.

#locale: An identifier for the locale of the user taking the values A and B. Because 
#        the data is anonymized, we don't know which value refers to what locale.
edges <- read.csv('edges.csv', header=T)
users <- read.csv('users.csv', header=T)

str(edges)
str(users)

table(users$locale)

mean(sapply(users$id, function(x){sum(edges$V1 == x | edges$V2 == x)}))

table(users$gender, users$school)

require(igraph)

g <- graph.data.frame(edges, FALSE, users) 
plot(g, vertex.size=5, vertex.label=NA)

V(g)$size <- degree(g)/2+2
plot(g, vertex.label=NA)

V(g)$color <- "black"
V(g)$color[V(g)$gender == "A"] <- "red"
V(g)$color[V(g)$gender == "B"] <- "gray"
plot(g, vertex.label=NA)

V(g)$color <- "yellow"
V(g)$color[V(g)$school == "A"] <- "red"
V(g)$color[V(g)$school == "AB"] <- "blue"
plot(g, vertex.label=NA)

V(g)$color <- "yellow"
V(g)$color[V(g)$locale == "A"] <- "red"
V(g)$color[V(g)$locale == "B"] <- "blue"
plot(g, vertex.label=NA, edge.width=3)

require(rgl)
rglplot(g, vertex.label=NA, edge.width=3)
