#Each observation in the dataset is the monthly returns of a particular company in a particular year. 
#The years included are 2000-2009. 
#The companies are limited to tickers that were listed on the exchange for the entire period 2000-2009, 
#and whose stock price never fell below $1

#This dataset contains the following variables:
#    ReturnJan = the return for the company's stock during January (in the year of the observation). 
#    ReturnFeb = the return for the company's stock during February (in the year of the observation). 
#   ReturnMar = the return for the company's stock during March (in the year of the observation). 
#    ReturnApr = the return for the company's stock during April (in the year of the observation). 
#    ReturnMay = the return for the company's stock during May (in the year of the observation). 
#    ReturnJune = the return for the company's stock during June (in the year of the observation). 
#    ReturnJuly = the return for the company's stock during July (in the year of the observation). 
#    ReturnAug = the return for the company's stock during August (in the year of the observation). 
#    ReturnSep = the return for the company's stock during September (in the year of the observation). 
#    ReturnOct = the return for the company's stock during October (in the year of the observation). 
#    ReturnNov = the return for the company's stock during November (in the year of the observation). 
#    PositiveDec = whether or not the company's stock had a positive return in December (in the year of the observation). 
#                  This variable takes value 1 if the return was positive, and value 0 if the return was not positive.

stocks <- read.csv('StocksCluster.csv')
summary(stocks)

stocks.cor <- cor(stocks)
max(stocks.cor[stocks.cor != 1])
round(stocks.cor,7) == .1916728

require(caTools)
set.seed(144)
spl <- sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocks.train <- subset(stocks, spl == TRUE)
stocks.test <- subset(stocks, spl == FALSE)

#----------------------------------------------
stocks.logistic <- glm(PositiveDec ~ ., data = stocks.train, family = "binomial")
summary(stocks.logistic)

stocks.logistic.predict <- predict(stocks.logistic, type="response")
stocks.logistic.t <- table(stocks.train$PositiveDec, stocks.logistic.predict >= .50)
stocks.logistic.acc <- (stocks.logistic.t[1,1]+stocks.logistic.t[2,2])/sum(stocks.logistic.t)

stocks.logistic.predict2 <- predict(stocks.logistic, stocks.test, type="response")
stocks.logistic.t2 <- table(stocks.test$PositiveDec, stocks.logistic.predict2 >= .50)
stocks.logistic.acc2 <- (stocks.logistic.t2[1,1]+stocks.logistic.t2[2,2])/sum(stocks.logistic.t2)

max(table(stocks.test$PositiveDec))/sum(table(stocks.test$PositiveDec))


#----------------------------------------------
limited.train = stocks.train
limited.train$PositiveDec = NULL

limited.test = stocks.test
limited.test$PositiveDec = NULL

require(caret)

preproc <- preProcess(limited.train)
limited.train.norm <- predict(preproc, limited.train)
limited.test.norm <- predict(preproc, limited.test)

summary(limited.train.norm)
summary(limited.test.norm)

k <- 3
set.seed(144)
kmc <- kmeans(limited.train.norm, centers=k)
table(kmc$cluster)

require(flexclust)
kmc.kcca <- as.kcca(kmc, limited.train.norm)
limited.train.norm.predict <- predict(kmc.kcca)
limited.test.norm.predict <- predict(kmc.kcca, newdata=limited.test.norm)

table(limited.test.norm.predict)

stocks.train.c1 <- subset(stocks.train, limited.train.norm.predict == 1)
stocks.train.c2 <- subset(stocks.train, limited.train.norm.predict == 2)
stocks.train.c3 <- subset(stocks.train, limited.train.norm.predict == 3)

stocks.test.c1 <- subset(stocks.test, limited.test.norm.predict == 1)
stocks.test.c2 <- subset(stocks.test, limited.test.norm.predict == 2)
stocks.test.c3 <- subset(stocks.test, limited.test.norm.predict == 3)

mean(stocks.train.c1$PositiveDec)
mean(stocks.train.c2$PositiveDec)
mean(stocks.train.c3$PositiveDec)

#------------------------------------------
stocks.train.c1.logistic <- glm(PositiveDec ~ ., data = stocks.train.c1, family = "binomial")
stocks.train.c2.logistic <- glm(PositiveDec ~ ., data = stocks.train.c2, family = "binomial")
stocks.train.c3.logistic <- glm(PositiveDec ~ ., data = stocks.train.c3, family = "binomial")

summary(stocks.train.c1.logistic)
summary(stocks.train.c2.logistic)
summary(stocks.train.c3.logistic)


stocks.c1.predict <- predict(stocks.train.c1.logistic, newdata=stocks.test.c1, type="response")
stocks.c1.t <- table(stocks.test.c1$PositiveDec, stocks.c1.predict >= .50)
stocks.c1.acc <- (stocks.c1.t[1,1]+stocks.c1.t[2,2])/sum(stocks.c1.t)

stocks.c2.predict <- predict(stocks.train.c2.logistic, newdata=stocks.test.c2, type="response")
stocks.c2.t <- table(stocks.test.c2$PositiveDec, stocks.c2.predict >= .50)
stocks.c2.acc <- (stocks.c2.t[1,1]+stocks.c2.t[2,2])/sum(stocks.c2.t)

stocks.c3.predict <- predict(stocks.train.c3.logistic, newdata=stocks.test.c3, type="response")
stocks.c3.t <- table(stocks.test.c3$PositiveDec, stocks.c3.predict >= .50)
stocks.c3.acc <- (stocks.c3.t[1,1]+stocks.c3.t[2,2])/sum(stocks.c3.t)

#---------------------------------------------

allpredictions <- c(stocks.c1.predict, stocks.c2.predict, stocks.c3.predict)
alloutcomes <- c(stocks.test.c1$PositiveDec, stocks.test.c2$PositiveDec, stocks.test.c3$PositiveDec)
all.t <- table(alloutcomes, allpredictions >= .5)
all.acc <- (all.t[1,1]+all.t[2,2])/sum(all.t)
