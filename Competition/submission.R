
# File descriptions
#----------------------------------------
# The data provided for this competition is split into two files:
  
# eBayiPadTrain.csv = the training data set. It consists of 1861 listings.
# eBayiPadTest.csv = the testing data set. It consists of 798 listings.

# Data fields
#----------------------------------------
# The dependent variable in this problem is the variable sold, which labels 
# if an iPad listed on the eBay site was sold (equal to 1 if it did, and 0 if it did not). 
# The dependent variable is provided in the training data set, 
# but not the testing dataset. 

# The independent variables consist of 9 pieces of product data available at the time 
# the iPad listing is posted, and a unique identifier:
  
# description = The text description of the product provided by the seller.
# biddable = Whether this is an auction (biddable=1) or a sale with a fixed price (biddable=0).
# startprice = The start price (in US Dollars) for the auction (if biddable=1) or the sale price (if biddable=0).
# condition = The condition of the product (new, used, etc.)
# cellular = Whether the iPad has cellular connectivity (cellular=1) or not (cellular=0).
# carrier = The cellular carrier for which the iPad is equipped (if cellular=1); listed as "None" if cellular=0.
# color = The color of the iPad.
# storage = The iPad's storage capacity (in gigabytes).
# productline = The name of the product being sold.

eBayTrain <- read.csv("eBayiPadTrain.csv", stringsAsFactors=FALSE)
eBayTest <- read.csv("eBayiPadTest.csv", stringsAsFactors=FALSE)

productlinelevels <- unique(c(eBayTrain$productline, eBayTest$productline))

eBayTrain$biddable <- as.factor(eBayTrain$biddable)
eBayTrain$condition <- as.factor(eBayTrain$condition)
eBayTrain$cellular <- as.factor(eBayTrain$cellular)
eBayTrain$carrier <- as.factor(eBayTrain$carrier)
eBayTrain$color <- as.factor(eBayTrain$color)
eBayTrain$storage <- as.factor(eBayTrain$storage)
eBayTrain$productline <- as.factor(eBayTrain$productline)
eBayTrain$sold <- as.factor(eBayTrain$sold)

eBayTest$biddable <- as.factor(eBayTest$biddable)
eBayTest$condition <- as.factor(eBayTest$condition)
eBayTest$cellular <- as.factor(eBayTest$cellular)
eBayTest$carrier <- as.factor(eBayTest$carrier)
eBayTest$color <- as.factor(eBayTest$color)
eBayTest$storage <- as.factor(eBayTest$storage)
eBayTest$productline <- factor(eBayTest$productline, levels=productlinelevels)

#--------------------------------
# exploratory
#--------------------------------
par(mfrow=c(3,3)) #par(mfrow=c(1,1))
barplot(table(eBayTrain$sold, eBayTrain$biddable), main = 'Biddable', beside=T)
barplot(table(eBayTrain$sold, eBayTrain$condition), main = 'Condition', beside=T)
barplot(table(eBayTrain$sold, eBayTrain$cellular), main = 'Cellular', beside=T)
barplot(table(eBayTrain$sold, eBayTrain$carrier), main = 'Carrier', beside=T)
barplot(table(eBayTrain$sold, eBayTrain$color), main = 'Color', beside=T)
barplot(table(eBayTrain$sold, eBayTrain$storage), main = 'Storage', beside=T)
barplot(table(eBayTrain$sold, eBayTrain$productline), main = 'Product Line', beside=T)

#--------------------------------
# baseline accuracy - train
#--------------------------------
baseline.t <- table(eBayTrain$sold)
baseline.acc <- max(baseline.t) / sum(baseline.t)

#--------------------------------
# logistic regression model 1
# - all fields except text
#--------------------------------
model.logistic <- glm(sold ~ biddable + startprice + condition + cellular + carrier + color + storage + productline, 
                      data=eBayTrain, family="binomial")

model.logistic.acc.p <- predict(model.logistic, type="response")
model.logistic.acc.t <- table(eBayTrain$sold, model.logistic.acc.p >= .5)
model.logistic.acc <- (model.logistic.acc.t[1,1]+model.logistic.acc.t[2,2])/sum(model.logistic.acc.t)

model.logistic.pred <- predict(model.logistic, newdata=eBayTest, type="response")

model.logistic.sub <- data.frame(UniqueID = eBayTest$UniqueID, Probability1 = model.logistic.pred)

write.csv(model.logistic.sub, "Submission.logistic.csv", row.names=FALSE)

#--------------------------------
# logistic regression model 2
# - removed insignificant fields from model 1
#--------------------------------
model.logistic2 <- glm(sold ~ biddable + startprice + condition + storage + productline, 
                       data=eBayTrain, family=binomial)

model.logistic2.acc.p <- predict(model.logistic2, type="response")
model.logistic2.acc.t <- table(eBayTrain$sold, model.logistic2.acc.p >= .5)
model.logistic2.acc <- (model.logistic2.acc.t[1,1]+model.logistic2.acc.t[2,2])/sum(model.logistic2.acc.t)

model.logistic2.pred <- predict(model.logistic2, newdata=eBayTest, type="response")

model.logistic2.sub <- data.frame(UniqueID = eBayTest$UniqueID, Probability1 = model.logistic2.pred)

write.csv(model.logistic2.sub, "Submission.logistic2.csv", row.names=FALSE)

#----
require(ROCR)

model.logistic2.rocr <- prediction(model.logistic2.acc.p, eBayTrain$sold)
model.logistic2.perf <- performance(model.logistic2.rocr, "tpr", "fpr")
model.logistic2.auc <- as.numeric(performance(model.logistic2.rocr, "auc")@y.values)

par(mfrow=c(1,1))
plot(model.logistic2.perf, colorize=T, print.cutoffs.at=seq(0,1,.1))

#--------------------------------
# random forrest model
# - removed insignificant fields from model 1
#--------------------------------
require(randomForest)

model.forrest <- randomForest(sold ~ biddable + startprice + condition + storage + productline,
                              data=eBayTrain, nodesize=25, ntree=200)

model.forrest.acc.p <- predict(model.forrest, type="class")
model.forrest.acc.t <- table(eBayTrain$sold, model.forrest.acc.p)
model.forrest.acc <- (model.forrest.acc.t[1,1]+model.forrest.acc.t[2,2])/sum(model.forrest.acc.t)

model.forrest.pred <- predict(model.forrest, newdata=eBayTest, type="prob")

model.forrest.sub <- data.frame(UniqueID = eBayTest$UniqueID, Probability1 = model.forrest.pred[,2])

write.csv(model.forrest.sub, "Submission.rf.csv", row.names=FALSE)

#--------------------------------
# cart model
# - removed insignificant fields from model 1
#--------------------------------

require(rpart)
require(rpart.plot)

model.cart <- rpart(sold ~ biddable + startprice + condition + storage + productline, 
                    data=eBayTrain, method="class")

prp(model.cart)

model.cart.acc.p <- predict(model.cart, type="class")
model.cart.acc.t <- table(eBayTrain$sold, model.cart.acc.p)
model.cart.acc <- (model.cart.acc.t[1,1]+model.cart.acc.t[2,2])/sum(model.cart.acc.t)


model.cart.pred <- predict(model.cart, newdata=eBayTest)

model.cart.sub <- data.frame(UniqueID = eBayTest$UniqueID, Probability1 = model.cart.pred[,2])
write.csv(model.cart.sub, "Submission.cart.csv", row.names=FALSE)

#---------------------------------------------
# incorporate text matrix into dataset
#---------------------------------------------

require(tm)
eBayTrain.corpus <-  Corpus(VectorSource(eBayTrain$description))
eBayTrain.corpus <- tm_map(eBayTrain.corpus, tolower)
eBayTrain.corpus <- tm_map(eBayTrain.corpus, PlainTextDocument)
eBayTrain.corpus <- tm_map(eBayTrain.corpus, removePunctuation)
eBayTrain.corpus <- tm_map(eBayTrain.corpus, removeWords, stopwords("english"))
eBayTrain.corpus <- tm_map(eBayTrain.corpus, stemDocument)
eBayTrain.dtm <- DocumentTermMatrix(eBayTrain.corpus)
eBayTrain.spdtm <- removeSparseTerms(eBayTrain.dtm, .95)

eBayTrain.Sparse <- as.data.frame(as.matrix(eBayTrain.spdtm))
colnames(eBayTrain.Sparse) <- make.names(colnames(eBayTrain.Sparse))
rownames(eBayTrain.Sparse) <- rownames(eBayTrain)
sort(colSums(eBayTrain.Sparse))

eBayTest.corpus <-  Corpus(VectorSource(eBayTest$description))
eBayTest.corpus <- tm_map(eBayTest.corpus, tolower)
eBayTest.corpus <- tm_map(eBayTest.corpus, PlainTextDocument)
eBayTest.corpus <- tm_map(eBayTest.corpus, removePunctuation)
eBayTest.corpus <- tm_map(eBayTest.corpus, removeWords, stopwords("english"))
eBayTest.corpus <- tm_map(eBayTest.corpus, stemDocument)
eBayTest.dtm <- DocumentTermMatrix(eBayTest.corpus)
eBayTest.spdtm <- removeSparseTerms(eBayTest.dtm, .95)

eBayTest.Sparse <- as.data.frame(as.matrix(eBayTest.spdtm))
colnames(eBayTest.Sparse) <- make.names(colnames(eBayTest.Sparse))
rownames(eBayTest.Sparse) <- rownames(eBayTest)
sort(colSums(eBayTest.Sparse))

words.to.keep <- c("new","good","work","screen","ipad","scratch","use","condit")

sort(colSums(eBayTrain.Sparse[,words.to.keep]))
sort(colSums(eBayTest.Sparse[,words.to.keep]))

#biddable + startprice + condition + storage + productline
#new train
eBayTrain2 <- eBayTrain.Sparse
eBayTrain2$sold <- eBayTrain$sold
eBayTrain2$biddable <- eBayTrain$biddable
eBayTrain2$startprice <- eBayTrain$startprice
eBayTrain2$condition <- eBayTrain$condition
eBayTrain2$storage <- eBayTrain$storage
eBayTrain2$productline <- eBayTrain$productline

#new test
eBayTest2 <- eBayTest.Sparse
eBayTest2$biddable <- eBayTest$biddable
eBayTest2$startprice <- eBayTest$startprice
eBayTest2$condition <- eBayTest$condition
eBayTest2$storage <- eBayTest$storage
eBayTest2$productline <- eBayTest$productline
eBayTest2$UniqueID <- eBayTest$UniqueID

#--------------------------------
# random forrest model
# - removed insignificant fields from model 1, 
# - added text matrix
#--------------------------------

model.forrest2 <- randomForest(sold ~ ., data=eBayTrain2, nodesize=25, ntree=200)

model.forrest2.acc.p <- predict(model.forrest2, type="class")
model.forrest2.acc.t <- table(eBayTrain2$sold, model.forrest2.acc.p)
model.forrest2.acc <- (model.forrest2.acc.t[1,1]+model.forrest2.acc.t[2,2])/sum(model.forrest2.acc.t)

model.forrest2.pred <- predict(model.forrest2, newdata=eBayTest2, type="prob")

model.forrest2.sub <- data.frame(UniqueID = eBayTest$UniqueID, Probability1 = model.forrest2.pred[,2])

write.csv(model.forrest2.sub, "Submission.rf2.csv", row.names=FALSE)

#--------------------------------
# logistic regression model 3
# - model 2 + text
#--------------------------------
model.logistic3 <- glm(sold ~ ., data=eBayTrain2, family=binomial)

model.logistic3.acc.p <- predict(model.logistic3, type="response")
model.logistic3.acc.t <- table(eBayTrain2$sold, model.logistic3.acc.p >= .5)
model.logistic3.acc <- (model.logistic3.acc.t[1,1]+model.logistic3.acc.t[2,2])/sum(model.logistic3.acc.t)

model.logistic3.pred <- predict(model.logistic3, newdata=eBayTest2, type="response")

model.logistic3.sub <- data.frame(UniqueID = eBayTest$UniqueID, Probability1 = model.logistic3.pred)

write.csv(model.logistic3.sub, "Submission.logistic3.csv", row.names=FALSE)


#------------------------------
require(caret)
require(e1071)

numfolds <- trainControl(method="cv", number=10)
cpgrid <- expand.grid(.cp=seq(.01,.5,.01))

train(sold ~ ., data=eBayTrain2, method="rpart", trControl=numfolds, tuneGrid=cpgrid)

model.tree.cv <- rpart(sold ~ ., data=eBayTrain2, method="class", cp=.01)

model.tree.cv.acc.p <- predict(model.tree.cv, type="prob")
model.tree.cv.acc.t <- table(eBayTrain2$sold, model.tree.cv.acc.p[,2] > .5)
model.tree.cv.acc <- (model.tree.cv.acc.t[1,1]+model.tree.cv.acc.t[2,2])/(sum(model.tree.cv.acc.t))

prp(model.tree.cv)


#------------------------------------------
# clustering (manual)

eBayTrain.c1 <- subset(eBayTrain, biddable == 0 | (startprice >= 139.995 & productline %in% c('iPad 1','iPad 2','iPad 4','iPad mini','iPad mini 3','Unknown')))
eBayTrain.c2 <- subset(eBayTrain, biddable == 1 & (startprice < 139.995 | productline %in% c('iPad 3','iPad 5','iPad Air','iPad Air 2','iPad mini 2','iPad mini Retina')))

eBayTrain.c1.t <- table(eBayTrain.c1$sold)
eBayTrain.c2.t <- table(eBayTrain.c2$sold)
eBayTrain.c1.acc <- max(eBayTrain.c1.t) / sum(eBayTrain.c1.t)
eBayTrain.c2.acc <- max(eBayTrain.c2.t) / sum(eBayTrain.c2.t)


model.c1.logistic2 <- glm(sold ~ biddable + startprice + condition + storage + productline, 
                          data=eBayTrain.c1, family=binomial)

model.c1.logistic2.acc.p <- predict(model.c1.logistic2, type="response")
model.c1.logistic2.acc.t <- table(eBayTrain.c1$sold, model.c1.logistic2.acc.p >= .5)
model.c1.logistic2.acc <- (model.c1.logistic2.acc.t[1,1]+model.c1.logistic2.acc.t[2,2])/sum(model.c1.logistic2.acc.t)


model.c2.logistic2 <- glm(sold ~ startprice + condition + storage + productline, 
                          data=eBayTrain.c2, family=binomial)

model.c2.logistic2.acc.p <- predict(model.c2.logistic2, type="response")
model.c2.logistic2.acc.t <- table(eBayTrain.c2$sold, model.c2.logistic2.acc.p >= .5)
model.c2.logistic2.acc <- (model.c2.logistic2.acc.t[1,1]+model.c2.logistic2.acc.t[2,2])/sum(model.c2.logistic2.acc.t)


eBayTest.c1 <- subset(eBayTest, biddable == 0 | (startprice >= 139.995 & productline %in% c('iPad 1','iPad 2','iPad 4','iPad mini','iPad mini 3','Unknown')))
eBayTest.c2 <- subset(eBayTest, biddable == 1 & (startprice < 139.995 | productline %in% c('iPad 3','iPad 5','iPad Air','iPad Air 2','iPad mini 2','iPad mini Retina')))


model.c1.logistic2.pred <- predict(model.c1.logistic2, newdata=eBayTest.c1, type="response")
model.c2.logistic2.pred <- predict(model.c2.logistic2, newdata=eBayTest.c2, type="response")

eBayTest.c1.p <- eBayTest.c1
eBayTest.c1.p$p <- model.c1.logistic2.pred

eBayTest.c2.p <- eBayTest.c2
eBayTest.c2.p$p <- model.c2.logistic2.pred
  
model.c1c2.logistic2.pred <- rbind(eBayTest.c1.p,eBayTest.c2.p)

model.c1c2.logistic2.sub <- data.frame(UniqueID = model.c1c2.logistic2.pred$UniqueID, Probability1 = model.c1c2.logistic2.pred$p)
write.csv(model.c1c2.logistic2.sub, "Submission.logistic2.c1c2.csv", row.names=FALSE)

#-------------------------------------------------

eBayTrain2.c1 <- subset(eBayTrain2, biddable == 0 | (startprice >= 139.995 & productline %in% c('iPad 1','iPad 2','iPad 4','iPad mini','iPad mini 3','Unknown')))
eBayTrain2.c2 <- subset(eBayTrain2, biddable == 1 & (startprice < 139.995 | productline %in% c('iPad 3','iPad 5','iPad Air','iPad Air 2','iPad mini 2','iPad mini Retina')))

model2.c1.logistic2 <- glm(sold ~ ., data=eBayTrain2.c1, family=binomial)

model2.c1.logistic2.acc.p <- predict(model2.c1.logistic2, type="response")
model2.c1.logistic2.acc.t <- table(eBayTrain2.c1$sold, model2.c1.logistic2.acc.p >= .5)
model2.c1.logistic2.acc <- (model2.c1.logistic2.acc.t[1,1]+model2.c1.logistic2.acc.t[2,2])/sum(model2.c1.logistic2.acc.t)


model2.c2.logistic2 <- glm(sold ~ ., data=subset(eBayTrain2.c2, select=c(-biddable)), family=binomial)

model2.c2.logistic2.acc.p <- predict(model2.c2.logistic2, type="response")
model2.c2.logistic2.acc.t <- table(eBayTrain2.c2$sold, model2.c2.logistic2.acc.p >= .5)
model2.c2.logistic2.acc <- (model2.c2.logistic2.acc.t[1,1]+model2.c2.logistic2.acc.t[2,2])/sum(model2.c2.logistic2.acc.t)


eBayTest2.c1 <- subset(eBayTest2, biddable == 0 | (startprice >= 139.995 & productline %in% c('iPad 1','iPad 2','iPad 4','iPad mini','iPad mini 3','Unknown')))
eBayTest2.c2 <- subset(eBayTest2, biddable == 1 & (startprice < 139.995 | productline %in% c('iPad 3','iPad 5','iPad Air','iPad Air 2','iPad mini 2','iPad mini Retina')))


model2.c1.logistic2.pred <- predict(model2.c1.logistic2, newdata=eBayTest2.c1, type="response")
model2.c2.logistic2.pred <- predict(model2.c2.logistic2, newdata=eBayTest2.c2, type="response")

eBayTest2.c1.p <- eBayTest2.c1
eBayTest2.c1.p$p <- model2.c1.logistic2.pred

eBayTest2.c2.p <- eBayTest2.c2
eBayTest2.c2.p$p <- model2.c2.logistic2.pred

model2.c1c2.logistic2.pred <- rbind(eBayTest2.c1.p,eBayTest2.c2.p)

model2.c1c2.logistic2.sub <- data.frame(UniqueID = model2.c1c2.logistic2.pred$UniqueID, Probability1 = model2.c1c2.logistic2.pred$p)
write.csv(model2.c1c2.logistic2.sub, "Submission.logistic2.c1c2.2.csv", row.names=FALSE)
