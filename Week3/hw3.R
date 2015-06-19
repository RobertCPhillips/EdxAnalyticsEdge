# credit.policy: 1 if the customer meets the credit underwriting criteria of LendingClub.com, and 0 otherwise.
# purpose: The purpose of the loan (takes values "credit_card", "debt_consolidation", "educational", "major_purchase", "small_business", and "all_other").
# int.rate: The interest rate of the loan, as a proportion (a rate of 11% would be stored as 0.11). Borrowers judged by LendingClub.com to be more risky are assigned higher interest rates.
# installment: The monthly installments ($) owed by the borrower if the loan is funded.
# log.annual.inc: The natural log of the self-reported annual income of the borrower.
# dti: The debt-to-income ratio of the borrower (amount of debt divided by annual income).
# fico: The FICO credit score of the borrower.
# days.with.cr.line: The number of days the borrower has had a credit line.
# revol.bal: The borrower's revolving balance (amount unpaid at the end of the credit card billing cycle).
# revol.util: The borrower's revolving line utilization rate (the amount of the credit line used relative to total credit available).
# inq.last.6mths: The borrower's number of inquiries by creditors in the last 6 months.
# delinq.2yrs: The number of times the borrower had been 30+ days past due on a payment in the past 2 years.
# pub.rec: The borrower's number of derogatory public records (bankruptcy filings, tax liens, or judgments).
# not.fully.paid: indicates that the loan was not paid back in full

loans <- read.csv("loans.csv")
loans.imp <- read.csv("loans_imputed.csv")
str(loans)
summary(loans)
summary(loans.imp)

t1 <- table(loans$not.fully.paid)
t1[2]/sum(t1)

sapply(loans, function(x) sum(is.na(x)) )

loans.missing <- subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) |
                               is.na(inq.last.6mths) | is.na(delinq.2yrs) |
                               is.na(pub.rec) | is.na(revol.util))
table(loans.missing$not.fully.paid)

vars.for.imputation <- setdiff(names(loans), "not.fully.paid")

set.seed(144)

loans.split <- sample.split(loans.imp$not.fully.paid, SplitRatio=.70)

loans.train <- subset(loans.imp, loans.split == T)
loans.test <- subset(loans.imp, loans.split == F)

loans.mod1 <- glm(not.fully.paid ~ ., data=loans.train, family="binomial")
summary(loans.mod1)

q2.2 <- -9.317e-03*700 - -9.317e-03*710
q2.2b <- exp(-9.317e-03*700)/exp(-9.317e-03*710)

loans.pred <- predict(loans.mod1, loans.test, type="response")
loans.test$predicted.risk <- loans.pred

loans.mod1.t <- table(loans.test$not.fully.paid, loans.pred >= .5)
loans.mod1.t

loans.mod1.sens <- loans.mod1.t[2,2]/(loans.mod1.t[2,2] + loans.mod1.t[2,1])
loans.mod1.spec <- loans.mod1.t[1,1]/(loans.mod1.t[1,1] + loans.mod1.t[1,2])
loans.mod1.acc <- (loans.mod1.t[1,1] + loans.mod1.t[2,2])/sum(loans.mod1.t)

loans.base.t <- table(loans.imp$not.fully.paid)
loans.base <- max(loans.base.t)/sum(loans.base.t)


require(ROCR)
loans.mod1.rocr <- prediction(loans.pred, loans.test$not.fully.paid)
loans.mod1.perf <- performance(loans.mod1.rocr, "tpr", "fpr")
loans.mod1.auc <- as.numeric(performance(loans.mod1.rocr, "auc")@y.values)

#-------------------------------------------------------------

loans.mod2 <- glm(not.fully.paid ~ int.rate, data=loans.train, family="binomial")
summary(loans.mod2)

loans.mod2.pred <- predict(loans.mod2, loans.test, type="response")
max(loans.mod2.pred)

loans.mod2.t <- table(loans.test$not.fully.paid, loans.mod2.pred >= .5)
loans.mod2.t

require(ROCR)
loans.mod2.rocr <- prediction(loans.mod2.pred, loans.test$not.fully.paid)
loans.mod2.auc <- as.numeric(performance(loans.mod2.rocr, "auc")@y.values)

#------------------------------------
10*exp(.06*3)

loans.test$profit <- exp(loans.test$int.rate*3) - 1
loans.test$profit[loans.test$not.fully.paid == 1] <- -1

10*max(loans.test$profit)

highInterest <- subset(loans.test, int.rate >= .15)
mean(highInterest$profit)
table(highInterest$not.fully.paid)

cutoff <- sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans  <- subset(highInterest, predicted.risk <= cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
