murders <- read.csv("murders.csv", header = T)
str(murders)

statesmap <- map_data("state")
str(statesmap)

ggplot(data=statesmap, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color="black")

murders$region <- tolower(murders$State)

murdermap <- merge(statesmap, murders, by="region")
str(murdermap)

ggplot(data=murdermap, aes(x=long, y=lat, group=group, fill=Murders)) + geom_polygon(color="black") +
  scale_fill_gradient(low="black", high="red", guide = "legend")

ggplot(data=murdermap, aes(x=long, y=lat, group=group, fill=Population)) + geom_polygon(color="black") +
  scale_fill_gradient(low="black", high="red", guide = "legend")

murdermap$MurderRate <- murdermap$Murders / murdermap$Population * 100000

ggplot(data=murdermap, aes(x=long, y=lat, group=group, fill=MurderRate)) + geom_polygon(color="black") +
  scale_fill_gradient(low="black", high="red", guide = "legend")

ggplot(data=murdermap, aes(x=long, y=lat, group=group, fill=MurderRate)) + geom_polygon(color="black") +
  scale_fill_gradient(low="black", high="red", guide = "legend", limits=c(0,10))

ggplot(data=murdermap, aes(x=long, y=lat, group=group, fill=GunOwnership)) + geom_polygon(color="black") +
  scale_fill_gradient(low="black", high="red", guide = "legend")
