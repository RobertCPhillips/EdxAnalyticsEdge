par <- read.csv("parole.csv")
str(par)
summary(par)

# male: 1 if the parolee is male, 0 if female
# race: 1 if the parolee is white, 2 otherwise
# age: the parolee's age (in years) when he or she was released from prison
# state: a code for the parolee's state. 2 is Kentucky, 3 is Louisiana, 4 is Virginia, and 1 is any other state. The three states were selected due to having a high representation in the dataset.
# time.served: the number of months the parolee served in prison (limited by the inclusion criteria to not exceed 6 months).
# max.sentence: the maximum sentence length for all charges, in months (limited by the inclusion criteria to not exceed 18 months).
# multiple.offenses: 1 if the parolee was incarcerated for multiple offenses, 0 otherwise.
# crime: a code for the parolee's main crime leading to incarceration. 2 is larceny, 3 is drug-related crime, 4 is driving-related crime, and 1 is any other crime.
# violator: 1 if the parolee violated the parole, and 0 if the parolee completed the parole without violation.

q2 <- sum(par$violator == 1)

par$state <- as.factor(par$state)
par$crime <- as.factor(par$crime)

require(caTools)
set.seed(144)
split <- sample.split(par$violator, SplitRatio = 0.7)
train <- subset(par, split == TRUE)
test <- subset(par, split == FALSE)

par.mod1 <- glm(violator ~ ., data=train, family="binomial")
summary(par.mod1)


q43.data <- data.frame(male=1, race=1, age=50, state=factor(1,levels=c(1,2,3,4)), time.served=3, max.sentence=12, multiple.offenses=0, crime=factor(2,levels=c(1,2,3,4)))
                       #state2=0, state3=0, state4=0,
                       #crime2=1, crime3=0, crime4=0)

q43.predict.res <- predict(par.mod1,q43.data,type="response")
q43.predict <- predict(par.mod1,q43.data)
q43.odds <- exp(q43.predict)
q43.prob <- 1/(1+exp(-q43.predict))
  #q43.predict.res/(1-q43.predict.res)

par.mod1.test <- predict(par.mod1,test,type="response")
max(par.mod1.test)

par.mod1.t <- table(test$violator, par.mod1.test >= .5)

par.mod1.sens <- par.mod1.t[2,2]/(par.mod1.t[2,2] + par.mod1.t[2,1])
par.mod1.spec <- par.mod1.t[1,1]/(par.mod1.t[1,1] + par.mod1.t[1,2])
par.mod1.acc <- (par.mod1.t[1,1]+par.mod1.t[2,2])/sum(par.mod1.t)

table(test$violator)

par.mod1.t2 <- table(test$violator, par.mod1.test >= .75)
par.mod1.t3 <- table(test$violator, par.mod1.test >= .25)

par.mod1.acc3 <- (par.mod1.t3[1,1]+par.mod1.t3[2,2])/sum(par.mod1.t3)

require(ROCR)
par.mod1.rocr <- prediction(par.mod1.test, test$violator)
par.mod1.perf <- performance(par.mod1.rocr, "tpr", "fpr")
par.mod1.auc <- as.numeric(performance(par.mod1.rocr, "auc")@y.values)
