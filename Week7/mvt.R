mvt <- read.csv('mvt.csv',header=T, stringsAsFactors = F)
str(mvt)
summary(mvt)

mvt$Date <- strptime(mvt$Date, format = "%m/%d/%y %H:%M")
mvt$Weekday <- weekdays(mvt$Date)
mvt$Hour <- mvt$Date$hour
str(mvt)
summary(mvt)

weekdayCounts <- as.data.frame(table(mvt$Weekday))
str(weekdayCounts)

require(ggplot2)
ggplot(weekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))

weekdayCounts$Var1 <- factor(weekdayCounts$Var1, ordered = T, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
ggplot(weekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) +
  xlab("Day of the Week") + ylab("Total Thefts")

dayhourcounts <- as.data.frame(table(mvt$Weekday, mvt$Hour))
str(dayhourcounts)

dayhourcounts$Hour <- as.numeric(as.character(dayhourcounts$Var2))
str(dayhourcounts)

ggplot(dayhourcounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Var1, size=2))
dayhourcounts$Var1 <- factor(dayhourcounts$Var1, ordered = T, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
str(dayhourcounts)

ggplot(dayhourcounts, aes(x=Hour, y=Var1)) + geom_tile(aes(fill=Freq)) +
  xlab("Hour of Day") + ylab("Day of the Week") +
  scale_fill_gradient(name="Total Thefts", low="white", high="red")

require(maps)
require(ggmap)

chicago <- get_map(location = "chicago", zoom = 11)
ggmap(chicago) +
  geom_point(data=mvt[1:100,], aes(x=Longitude, y=Latitude))

latlongcounts <- as.data.frame(table(round(mvt$Longitude, 2), round(mvt$Latitude, 2)))
str(latlongcounts)
latlongcounts$Long <- as.numeric(as.character(latlongcounts$Var1))
latlongcounts$Lat <- as.numeric(as.character(latlongcounts$Var2))
str(latlongcounts)

ggmap(chicago) + geom_point(data=latlongcounts, aes(x=Long, y=Lat, color=Freq, size=Freq)) +
  scale_color_gradient(low="yellow", high="red")

ggmap(chicago) + geom_tile(data=latlongcounts, aes(x=Long, y=Lat, alpha=Freq, fill="red"))

LatLonCounts2 <- subset(latlongcounts, Freq > 0)

ggmap(chicago) + geom_tile(data=LatLonCounts2, aes(x=Long, y=Lat, alpha=Freq, fill="red"))
