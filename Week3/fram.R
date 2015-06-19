fram <- read.csv("framingham.csv")
str(fram)

require(caTools)
set.seed(1000)

split <- sample.split(fram$TenYearCHD, SplitRatio=.65)

ftrain <- subset(fram,split == T)
ftest <- subset(fram,split == F)

flog <- glm(TenYearCHD~., data=ftrain, family="binomial")
summary(flog)

flog.test <- predict(flog,ftest,type="response")
flog.table <- table(ftest$TenYearCHD, flog.test > .5)
flog.table

flog.acc <- (flog.table[1,1]+flog.table[2,2])/sum(flog.table)
flog.bas <- (flog.table[1,1]+flog.table[1,2])/sum(flog.table)


flog.sens <- flog.table[2,2]/(flog.table[2,2] + flog.table[2,1])
flog.spec <- flog.table[1,1]/(flog.table[1,1] + flog.table[1,2])

require(ROCR)

flog.rocr <- prediction(flog.test, ftest$TenYearCHD)
flog.perf <- performance(flog.rocr, "tpr", "fpr")
flog.auc <- as.numeric(performance(flog.rocr, "auc")@y.values)

plot(flog.perf, colorize=T, print.cutoffs.at=seq(0,1,.1))
