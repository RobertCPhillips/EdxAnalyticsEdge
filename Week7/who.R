who <- read.csv('who.csv',header=T)
str(who)
summary(who)

require(ggplot2)

plot(who$GNI, who$FertilityRate)

scatter.plot <- ggplot(data=who, aes(x = GNI, y=FertilityRate))
scatter.plot + geom_point()
scatter.plot + geom_line()
fertility.plot <- scatter.plot + geom_point(color="blue", size=3, shape=15) + ggtitle("Gross National Income")

pdf("myplot.pdf")
print(fertility.plot)
dev.off()

scatter.plot2 <- ggplot(data=who, aes(x = GNI, y=FertilityRate, color=LifeExpectancy))
scatter.plot2 + geom_point()

scatter.plot3 <- ggplot(data=who, aes(x = FertilityRate, y=Under15))
scatter.plot3 + geom_point()

scatter.plot4 <- ggplot(data=who, aes(x = log(FertilityRate), y=Under15, color=Region))
scatter.plot4 + geom_point()

model <- lm(Under15~log(FertilityRate), data=who)
summary(model)

scatter.plot4 + geom_point() + stat_smooth(method = "lm", se=F, color='red') + scale_color_brewer(palette="Dark2")
