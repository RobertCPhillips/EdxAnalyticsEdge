
#Date: The date the change was announced.
#Chairman: The name of the Federal Reserve Chairman at the time the change was announced.
#PreviousRate: The federal funds rate in the prior month.
#Streak: The current streak of raising or not raising the rate, e.g. +8 indicates the rate has been increased 8 months in a row, whereas -3 indicates the rate has been lowered or stayed the same for 3 months in a row.
#GDP: The U.S. Gross Domestic Product, in Billions of Chained 2009 US Dollars.
#Unemployment: The unemployment rate in the U.S.
#CPI: The Consumer Price Index, an indicator of inflation, in 
#HomeownershipRate: The rate of homeownership in the u.S.
#DebtAsPctGDP: The U.S. national debt as a percentage of GDP
#DemocraticPres: Whether the sitting U.S. President is a Democrat (DemocraticPres=1) or a Republican (DemocraticPres=0)
#MonthsUntilElection: The number of remaining months until the next U.S. presidential election.

fedFunds <- read.csv('federalFundsRate.csv', stringsAsFactors=FALSE)
mean(fedFunds$Streak > 0)
table(fedFunds$Chairman)

fedFunds$Chairman <- as.factor(fedFunds$Chairman)
fedFunds$DemocraticPres <- as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds <- as.factor(fedFunds$RaisedFedFunds)

set.seed(201)
library(caTools)
spl = sample.split(fedFunds$RaisedFedFunds, 0.7)

fedFunds.train <- subset(fedFunds, spl == T)
fedFunds.test <- subset(fedFunds, spl == F)

fedFunds.glm <- glm(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + 
                                     HomeownershipRate + DemocraticPres + 
                                     MonthsUntilElection, data=fedFunds.train,
                    family="binomial")
summary(fedFunds.glm)

#fedFunds.glm$coefficients %*% c(1,1.7,-3,5.1,65.3,0,18)
q6 <- data.frame(PreviousRate=1.7,
                 Streak=-3,
                 Unemployment=5,1,
                 HomeownershipRate=65.3,
                 DemocraticPres=factor(0, levels=c(0,1)),
                 MonthsUntilElection=18)

predict(fedFunds.glm, newdata=q6, type="response")

fedFunds.glm.test <- predict(fedFunds.glm, fedFunds.test, type="response")
fedFunds.glm.test.t <- table(fedFunds.test$RaisedFedFunds, fedFunds.glm.test >= .5)
fedFunds.test.t <- table(fedFunds.test$RaisedFedFunds)
fedFunds.train.t <- table(fedFunds.train$RaisedFedFunds)
sum(fedFunds.glm.test.t[1,1], fedFunds.glm.test.t[2,2]) - max(fedFunds.test.t)

require(ROCR)
fedFunds.glm.test.rocr <- prediction(fedFunds.glm.test, fedFunds.test$RaisedFedFunds)
fedFunds.glm.test.perf <- performance(fedFunds.glm.test.rocr, "tpr", "fpr")
fedFunds.glm.test.auc <- as.numeric(performance(fedFunds.glm.test.rocr, "auc")@y.values)

plot(fedFunds.glm.test.perf,colorize=T,print.cutoffs.at=seq(0,1,.1))

#------------------------------
require(caret)
require(e1071)
require(rpart)
require(rpart.plot)

numfolds <- trainControl(method="cv", number=10)
cpgrid <- expand.grid(.cp=seq(.001,.05,.001))

set.seed(201)

train(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + 
        HomeownershipRate + DemocraticPres + MonthsUntilElection,
      data=fedFunds.train, method="rpart", 
      trControl=numfolds, tuneGrid=cpgrid)

fedFunds.cart <- rpart(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + 
                         HomeownershipRate + DemocraticPres + MonthsUntilElection,
                       data=fedFunds.train, method="class", cp=0.016)
prp(fedFunds.cart)

fedFunds.cart.test <- predict(fedFunds.cart, fedFunds.test, type="class")
fedFunds.cart.test.t <- table(fedFunds.test$RaisedFedFunds, fedFunds.cart.test)
fedFunds.cart.test.acc <- sum(fedFunds.cart.test.t[1,1] + fedFunds.cart.test.t[2,2]) / sum(fedFunds.cart.test.t)

