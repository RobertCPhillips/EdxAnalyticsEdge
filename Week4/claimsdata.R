claims <- read.csv('claimsdata.csv')
str(claims)

table(claims$bucket2009)/nrow(claims)

require(caTools)
set.seed(88)
split <- sample.split(claims$bucket2009, .60)

claims.train <- subset(claims, split==T)
claims.test <- subset(claims, split==F)

mean(claims.train$age)
sum(claims.train$diabetes == 1)/nrow(claims.train)
summary(claims.train)

#-----------------------------------
claims.base.t <- table(claims.test$bucket2009, claims.test$bucket2008)
claims.base.acc <- (claims.base.t[1,1]+claims.base.t[2,2]
                    +claims.base.t[3,3]+claims.base.t[4,4]
                    +claims.base.t[5,5])/sum(claims.base.t)

penalty <- matrix(c(0, 1, 2, 3, 4, 
                    2, 0, 1, 2, 3, 
                    4, 2, 0, 1, 2,
                    6, 4, 2, 0, 1, 
                    8, 6, 4, 2, 0),byrow=T,nrow=5)

claims.base.pen <- as.matrix(claims.base.t)*penalty
claims.base.err <- sum(claims.base.pen)/nrow(claims.test)

claims.base2.acc <- sum(claims.base.t[1,])/sum(claims.base.t)
claims.bsae2.pen <- matrix(c(sum(claims.base.t[1,]),sum(claims.base.t[2,]),
                             sum(claims.base.t[3,]),sum(claims.base.t[4,]),
                             sum(claims.base.t[5,]))) * penalty[,1]
claims.bsae2.err <- sum(claims.bsae2.pen)/nrow(claims.test)

#-----------------------------------
require(rpart)
require(rpart.plot)

claims.tree <- rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + 
                                  copd + depression + diabetes + heart.failure +
                                  ihd + kidney + osteoporosis + stroke +
                                  bucket2008 + reimbursement2008, data=claims.train,
                                  method="class", cp=.00005)
prp(claims.tree)

claims.tree.predict <- predict(claims.tree, newdata=claims.test, type="class")
claims.tree.predict.t <- table(claims.test$bucket2009, claims.tree.predict)

claims.tree.acc <- (claims.tree.predict.t[1,1]+claims.tree.predict.t[2,2]
                    +claims.tree.predict.t[3,3]+claims.tree.predict.t[4,4]
                    +claims.tree.predict.t[5,5])/sum(claims.tree.predict.t)

claims.tree.pen <- as.matrix(claims.tree.predict.t)*penalty
claims.tree.err <- sum(claims.tree.pen)/nrow(claims.test)


claims.tree2 <- rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + 
                                   copd + depression + diabetes + heart.failure +
                                   ihd + kidney + osteoporosis + stroke +
                                   bucket2008 + reimbursement2008, 
                      data=claims.train, method="class", 
                      cp=.00005,parms=list(loss=penalty))

claims.tree2.predict <- predict(claims.tree2, newdata=claims.test, type="class")
claims.tree2.predict.t <- table(claims.test$bucket2009, claims.tree2.predict)

claims.tree2.acc <- (claims.tree2.predict.t[1,1]+claims.tree2.predict.t[2,2]
                    +claims.tree2.predict.t[3,3]+claims.tree2.predict.t[4,4]
                    +claims.tree2.predict.t[5,5])/sum(claims.tree2.predict.t)

claims.tree2.pen <- as.matrix(claims.tree2.predict.t)*penalty
claims.tree2.err <- sum(claims.tree2.pen)/nrow(claims.test)
