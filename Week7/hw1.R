require(ggplot2)
require(maps)
require(ggmap)

statesMap = map_data("state")
str(statesMap)
length(unique(statesMap$group))

ggplot(statesMap, aes(x = long, y = lat, group = group)) + 
  geom_polygon(fill = "white", color = "black") 

polling <- read.csv("PollingImputed.csv")
str(polling)

polling.train <- subset(polling, Year <= 2008)
polling.test <- subset(polling, Year == 2012)

polling.fit1 <- glm(Republican~SurveyUSA+DiffCount, data=polling.train, family="binomial")
polling.fit1.predict <- predict(polling.fit1, newdata=polling.test, type="response")
polling.fit1.predict.bin <- as.numeric(polling.fit1.predict > 0.5)

polling.pred.dataframe <- data.frame(polling.fit1.predict, 
                                     polling.fit1.predict.bin, 
                                     polling.test$State)

polling.pred.dataframe$region = tolower(polling.pred.dataframe$polling.test.State)
predictionMap = merge(statesMap, polling.pred.dataframe, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = polling.fit1.predict.bin)) + 
  geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = polling.fit1.predict.bin)) + 
  geom_polygon(color = "black", size=3, alpha=.3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = polling.fit1.predict)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "grey", high = "black", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")


