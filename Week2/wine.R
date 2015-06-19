wine <- read.csv('wine.csv')
wineTest <- read.csv('wine_test.csv')
str(wine)

model1 <- lm(Price~AGST, data=wine)
plot(Price~AGST, data=wine)
abline(model1)
summary(model1)
sse1 <- sum(model1$residuals^2)
predict1 <- predict(model1, newdata=wineTest)

model2 <- lm(Price~AGST+HarvestRain, data=wine)
summary(model2)
sse2 <- sum(model2$residuals^2)

model3 <- lm(Price~WinterRain+HarvestRain, data=wine)
summary(model3)
sse3 <- sum(model3$residuals^2)

