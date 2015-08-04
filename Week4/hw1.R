
# - "Civic Duty" (variable civicduty) group members were sent a letter that simply said "DO YOUR CIVIC DUTY - VOTE!"
# - "Hawthorne Effect" (variable hawthorne) group members were sent a letter that had the "Civic Duty" message plus the additional message "YOU ARE BEING STUDIED" and they were informed that their voting behavior would be examined by means of public records.
# - "Self" (variable self) group members received the "Civic Duty" message as well as the recent voting record of everyone in that household and a message stating that another message would be sent after the election with updated records.
# - "Neighbors" (variable neighbors) group members were given the same message as that for the "Self" group, except the message not only had the household voting records but also that of neighbors - maximizing social pressure.
# - "Control" (variable control) group members were not sent anything, and represented the typical voting situation.
# - Additional variables include sex (0 for male, 1 for female), 
# - yob (year of birth), 
# - and the dependent variable voting (1 if they voted, 0 otherwise).

gerber <- read.csv("gerber.csv")
str(gerber)

q1 <- sum(gerber$voting == 1)/nrow(gerber)

voted <- subset(gerber, voting == 1)
summary(voted) # q2 <- 'neighbors'

gerber.logistic <- glm(voting ~ hawthorne + civicduty + neighbors + self,
                       data=gerber, family="binomial")
summary(gerber.logistic) # q3 <- all four
gerber.logistic.predict.res <- predict(gerber.logistic, type="response")
gerber.logistic.t1 <- table(gerber$voting, gerber.logistic.predict.res >= .3)
gerber.logistic.t2 <- table(gerber$voting, gerber.logistic.predict.res >= .5)

gerber.logistic.acc1 <- (gerber.logistic.t1[1,1]+gerber.logistic.t1[2,2])/sum(gerber.logistic.t1)
gerber.logistic.acc2 <- (gerber.logistic.t2[1,1]+0)/sum(gerber.logistic.t2)

require(ROCR)
gerber.logistic.rocr <- prediction(gerber.logistic.predict.res, gerber$voting)
gerber.logistic.perf <- performance(gerber.logistic.rocr, "tpr", "fpr")
gerber.logistic.auc <- as.numeric(performance(gerber.logistic.rocr, "auc")@y.values)

#----------------------------------------------------------------

gerber.cart <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(gerber.cart)

gerber.cart2 <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(gerber.cart2)

gerber.cart3 <- rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(gerber.cart3)

#----------------------------------------------------------------

gerber.cart4 <- rpart(voting ~ control, data=gerber, cp=0.0)
prp(gerber.cart4,digits=6)
q3.1 <- abs(.296638-.34)

gerber.cart5 <- rpart(voting ~ control+sex, data=gerber, cp=0.0)
prp(gerber.cart5,digits=6)

gerber.logistic2 <- glm(voting ~ control + sex, data=gerber, family="binomial")
summary(gerber.logistic2)

possibile <- data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(gerber.logistic2, newdata=possibile, type="response")

gerber.logistic3 <- glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(gerber.logistic3)
predict(gerber.logistic3, newdata=possibile, type="response")


