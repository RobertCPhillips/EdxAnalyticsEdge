# - age = the age of the individual in years
# - workclass = the classification of the individual's working status (does the person work for the federal government, work for the local government, work without pay, and so on)
# - education = the level of education of the individual (e.g., 5th-6th grade, high school graduate, PhD, so on)
# - maritalstatus = the marital status of the individual
# - occupation = the type of work the individual does (e.g., administrative/clerical work, farming/fishing, sales and so on)
# - relationship = relationship of individual to his/her household
# - race = the individual's race
# - sex = the individual's sex
# - capitalgain = the capital gains of the individual in 1994 (from selling an asset such as a stock or bond for more than the original purchase price)
# - capitalloss = the capital losses of the individual in 1994 (from selling an asset such as a stock or bond for less than the original purchase price)
# - hoursperweek = the number of hours the individual works per week
# - nativecountry = the native country of the individual
# - over50k = whether or not the individual earned more than $50,000 in 1994

census <- read.csv('census.csv')
str(census)

set.seed(2000)
split <- sample.split(census$over50k, .60)
census.train <- subset(census, split==T)
census.test <- subset(census, split==F)

census.logistic <- glm(over50k ~ ., data=census.train, family="binomial")
summary(census.logistic) 

census.logistic.predict <- predict(census.logistic, census.test, type="response")
census.logistic.t <- table(census.test$over50k, census.logistic.predict >= .50)
census.logistic.acc <- (census.logistic.t[1,1]+census.logistic.t[2,2])/sum(census.logistic.t)

census.base.t <- table(census.test$over50k)
census.base.acc <- max(census.base.t)/sum(census.base.t)

require(ROCR)
census.logistic.rocr <- prediction(census.logistic.predict, census.test$over50k)
census.logistic.perf <- performance(census.logistic.rocr,"tpr","fpr")
census.logistic.auc <- as.numeric(performance(census.logistic.rocr, "auc")@y.values)
plot(census.logistic.perf,colorize=T,print.cutoffs.at=seq(0,1,.1))

#-----------------------------------------------------------------

require(rpart)
require(rpart.plot)

census.cart <- rpart(over50k ~ ., data=census.train, method="class")
prp(census.cart)

census.cart.predict <- predict(census.cart, newdata=census.test, type="class")
census.cart.predict.t <- table(census.test$over50k,census.cart.predict)
census.cart.acc <- (census.cart.predict.t[1,1]+census.cart.predict.t[2,2])/sum(census.cart.predict.t)

census.cart.predict.res <- predict(census.cart, newdata=census.test)

require(ROCR)

census.cart.rocr <- prediction(census.cart.predict.res[,2], census.test$over50k)
census.cart.perf <- performance(census.cart.rocr,"tpr","fpr")
census.cart.auc <- as.numeric(performance(census.cart.rocr, "auc")@y.values)
plot(census.cart.perf,colorize=T,print.cutoffs.at=seq(0,1,.1))

#-----------------------------------------------------------------
require(randomForest)
set.seed(1)
census.train.small <- census.train[sample(nrow(census.train), 2000), ]

set.seed(1)
census.forrest <- randomForest(over50k ~ ., data=census.train.small)

census.forrest.predict <- predict(census.forrest, newdata=census.test)
census.forrest.predict.t <- table(census.test$over50k,census.forrest.predict)
census.forrest.acc <- (census.forrest.predict.t[1,1]+census.forrest.predict.t[2,2])/sum(census.forrest.predict.t)

vu <- varUsed(census.forrest, count=TRUE)
vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(census.forrest$forest$xlevels[vusorted$ix]))
varImpPlot(census.forrest)

#--------------------------------------------------------
require(caret)
require(e1071)
set.seed(2)
numfolds <- trainControl(method="cv", number=10)
cpgrid <- expand.grid(.cp=seq(0.002,0.1,0.002))

train(over50k ~ ., data=census.train, method="rpart", trControl=numfolds, tuneGrid=cpgrid)

census.cart2 <- rpart(over50k ~ ., data=census.train, method="class",cp=0.002)
prp(census.cart2)

census.cart2.predict <- predict(census.cart2, newdata=census.test, type="class")
census.cart2.predict.t <- table(census.test$over50k,census.cart2.predict)
census.cart2.acc <- (census.cart2.predict.t[1,1]+census.cart2.predict.t[2,2])/sum(census.cart2.predict.t)









