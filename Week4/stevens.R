steve <- read.csv('stevens.csv')
str(steve)

require(caTools)
set.seed(3000)
split <- sample.split(steve$Reverse, .70)

steve.train <- subset(steve, split==T)
steve.test <- subset(steve, split==F)

require(rpart)
require(rpart.plot)

steve.tree <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                    data=steve.train, method="class", minbucket=25)
summary(steve.tree)

prp(steve.tree)

steve.predict <- predict(steve.tree, newdata=steve.test, type="class")
steve.predict.t <- table(steve.test$Reverse,steve.predict)
steve.predict.acc <- (steve.predict.t[1,1]+steve.predict.t[2,2])/(sum(steve.predict.t))

require(ROCR)
steve.predict.roc <- predict(steve.tree, newdata=steve.test)
steve.pred <- prediction(steve.predict.roc[,2], steve.test$Reverse)
steve.perf <- performance(steve.pred,"tpr","fpr")
plot(steve.perf)

steve.pred.auc <- as.numeric(performance(steve.pred, "auc")@y.values)

#--------------------------
steve.tree5 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                     data=steve.train, method="class", minbucket=5)
prp(steve.tree5)

steve.tree100 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                       data=steve.train, method="class", minbucket=100)
prp(steve.tree100)

#----------------------------
require(randomForest)
steve.train$Reverse <- as.factor(steve.train$Reverse)
steve.test$Reverse <- as.factor(steve.test$Reverse)

steve.forrest <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                              data=steve.train, nodesize=25, ntree=200)

steve.forrest.predict <- predict(steve.forrest, newdata=steve.test)
steve.forrest.predict.t <- table(steve.test$Reverse,steve.forrest.predict)
steve.forrest.predict.acc <- (steve.forrest.predict.t[1,1]+steve.forrest.predict.t[2,2])/(sum(steve.forrest.predict.t))

#------------------------------------
set.seed(100)
steve.forrest100 <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                                 data=steve.train, nodesize=25, ntree=200)

steve.forrest100.predict <- predict(steve.forrest100, newdata=steve.test)
steve.forrest100.predict.t <- table(steve.test$Reverse,steve.forrest100.predict)
steve.forrest100.predict.acc <- (steve.forrest100.predict.t[1,1]+steve.forrest100.predict.t[2,2])/(sum(steve.forrest100.predict.t))

#------------------------------------
set.seed(200)
steve.forrest200 <- randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                                 data=steve.train, nodesize=25, ntree=200)

steve.forrest200.predict <- predict(steve.forrest200, newdata=steve.test)
steve.forrest200.predict.t <- table(steve.test$Reverse,steve.forrest200.predict)
steve.forrest200.predict.acc <- (steve.forrest200.predict.t[1,1]+steve.forrest200.predict.t[2,2])/(sum(steve.forrest200.predict.t))

#------------------------------
require(caret)
require(e1071)

numfolds <- trainControl(method="cv", number=10)
cpgrid <- expand.grid(.cp=seq(.01,.5,.01))

train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
      data=steve.train, method="rpart", trControl=numfolds, tuneGrid=cpgrid)

steve.tree.cv <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                       data=steve.train, method="class", cp=0.19)

steve.tree.cv.predict <- predict(steve.tree.cv, newdata=steve.test, type="class")
steve.tree.cv.predict.t <- table(steve.test$Reverse, steve.tree.cv.predict)
steve.tree.cv.predict.acc <- (steve.tree.cv.predict.t[1,1]+steve.tree.cv.predict.t[2,2])/(sum(steve.tree.cv.predict.t))

prp(steve.tree.cv)



