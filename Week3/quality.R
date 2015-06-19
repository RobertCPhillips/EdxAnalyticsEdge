q <- read.csv("quality.csv")
str(q)

plot(Narcotics~TotalVisits, data=q)

table(q$PoorCare)

require(caTools)
set.seed(88)

split <- sample.split(q$PoorCare, SplitRatio=.75)

qtrain <- subset(q,split == T)
qtest <- subset(q,split == F)

qlog <- glm(PoorCare~OfficeVisits+Narcotics, data=qtrain, family="binomial")
summary(qlog)

predict.train <- predict(qlog, type="response")
summary(predict.train)

#-------------------------
qlog2 <- glm(PoorCare~StartedOnCombination+ProviderCount, data=qtrain, family="binomial")
summary(qlog2)

t1 <- table(qtrain$PoorCare,predict.train > .5)
t1
sens1 <- t1[2,2]/(t1[2,2] + t1[2,1])
spec1 <- t1[1,1]/(t1[1,1] + t1[1,2])

t2 <- table(qtrain$PoorCare,predict.train > .7)
t2
sens2 <- t2[2,2]/(t2[2,2] + t2[2,1])
spec2 <- t2[1,1]/(t2[1,1] + t2[1,2])

require(ROCR)

predict.rocr <- prediction(predict.train,qtrain$PoorCare)
predict.perf <- performance(predict.rocr,"tpr","fpr")
plot(predict.perf,colorize=T,print.cutoffs.at=seq(0,1,.1))

#-----------------------
predict.test <- predict(qlog, type="response", newdata=qtest)
predict.test.roc <- prediction(predict.test, qtest$PoorCare)
auc <- as.numeric(performance(predict.test.roc, "auc")@y.values)
