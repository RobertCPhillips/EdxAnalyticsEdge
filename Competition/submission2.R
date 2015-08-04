
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

eBayTrain$startprice <- (eBayTrain$startprice - mean(eBayTrain$startprice)) / sd(eBayTrain$startprice)
eBayTest$startprice <- (eBayTest$startprice - mean(eBayTest$startprice)) / sd(eBayTest$startprice)

#--------------------------------
# baseline accuracy - train
#--------------------------------
baseline.t <- table(eBayTrain$sold)
baseline.acc <- max(baseline.t) / sum(baseline.t)

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
#         + cellular + carrier + color
#new train
eBayTrain2 <- eBayTrain.Sparse
eBayTrain2$sold <- eBayTrain$sold
eBayTrain2$biddable <- eBayTrain$biddable
eBayTrain2$startprice <- eBayTrain$startprice
eBayTrain2$condition <- eBayTrain$condition
eBayTrain2$storage <- eBayTrain$storage
eBayTrain2$productline <- eBayTrain$productline
eBayTrain2$cellular <- eBayTrain$cellular
eBayTrain2$carrier <- eBayTrain$carrier
eBayTrain2$color <- eBayTrain$color

#new test
eBayTest2 <- eBayTest.Sparse
eBayTest2$biddable <- eBayTest$biddable
eBayTest2$startprice <- eBayTest$startprice
eBayTest2$condition <- eBayTest$condition
eBayTest2$storage <- eBayTest$storage
eBayTest2$productline <- eBayTest$productline
eBayTest2$UniqueID <- eBayTest$UniqueID
eBayTest2$cellular <- eBayTest$cellular
eBayTest2$carrier <- eBayTest$carrier
eBayTest2$color <- eBayTest$color

#-------------------------------------------------
require(caret)
require(e1071)
require(rpart)
require(rpart.plot)

numfolds <- trainControl(method="cv", number=10)
cpgrid <- expand.grid(.cp=seq(.01,.5,.01))

train(sold ~ ., data=eBayTrain2, method="rpart", trControl=numfolds, tuneGrid=cpgrid)

model.tree.cv <- rpart(sold ~ ., data=eBayTrain2, method="class", cp=.01)

model.tree.cv.acc.p <- predict(model.tree.cv, type="prob")
model.tree.cv.acc.t <- table(eBayTrain2$sold, model.tree.cv.acc.p[,2] > .5)
model.tree.cv.acc <- (model.tree.cv.acc.t[1,1]+model.tree.cv.acc.t[2,2])/(sum(model.tree.cv.acc.t))

prp(model.tree.cv)


#-------------------------------------------------
priceThresh <- -0.42
products.c1 <- c('iPad 1','iPad 2','iPad 4','iPad mini','iPad mini 3','Unknown')
products.c2 <- c('iPad 3','iPad 5','iPad Air','iPad Air 2','iPad mini 2','iPad mini Retina')
  
#eBayTrain2.c1 <- subset(eBayTrain2, biddable == 0 | (startprice >= priceThresh & productline %in% products.c1))
#eBayTrain2.c2 <- subset(eBayTrain2, biddable == 1 & (startprice < priceThresh | productline %in% products.c2))
eBayTrain2.c1 <- subset(eBayTrain2, startprice >= priceThresh)
eBayTrain2.c2 <- subset(eBayTrain2, startprice < priceThresh)

model1.c1.logistic <- glm(sold ~ ., data=eBayTrain2.c1, family=binomial)
#model1.c1.logistic <- glm(sold ~ ipad + startprice + productline, 
#                          data=subset(eBayTrain2.c1, select=c(-biddable)), family=binomial)
                          
model1.c1.logistic.acc.p <- predict(model1.c1.logistic, type="response")
model1.c1.logistic.acc.t <- table(eBayTrain2.c1$sold, model1.c1.logistic.acc.p >= .5)
model1.c1.logistic.acc <- (model1.c1.logistic.acc.t[1,1]+model1.c1.logistic.acc.t[2,2])/sum(model1.c1.logistic.acc.t)


#model1.c2.logistic <- glm(sold ~ ., data=subset(eBayTrain2.c2, select=c(-biddable)), family=binomial)
model1.c2.logistic <- glm(sold ~ ., data=eBayTrain2.c2, family=binomial)

model1.c2.logistic.acc.p <- predict(model1.c2.logistic, type="response")
model1.c2.logistic.acc.t <- table(eBayTrain2.c2$sold, model1.c2.logistic.acc.p >= .5)
model1.c2.logistic.acc <- (model1.c2.logistic.acc.t[1,1]+model1.c2.logistic.acc.t[2,2])/sum(model1.c2.logistic.acc.t)


#eBayTest2.c1 <- subset(eBayTest2, biddable == 0 | (startprice >= priceThresh & productline %in% products.c1))
#eBayTest2.c2 <- subset(eBayTest2, biddable == 1 & (startprice < priceThresh | productline %in% products.c2))
eBayTest2.c1 <- subset(eBayTest2, startprice >= priceThresh)
eBayTest2.c2 <- subset(eBayTest2, startprice < priceThresh)

model1.c1.logistic.pred <- predict(model1.c1.logistic, newdata=eBayTest2.c1, type="response")
model1.c2.logistic.pred <- predict(model1.c2.logistic, newdata=eBayTest2.c2, type="response")

eBayTest2.c1.p <- eBayTest2.c1
eBayTest2.c1.p$p <- model1.c1.logistic.pred

eBayTest2.c2.p <- eBayTest2.c2
eBayTest2.c2.p$p <- model1.c2.logistic.pred

model1.c1c2.logistic.pred <- rbind(eBayTest2.c1.p,eBayTest2.c2.p)

model1.c1c2.logistic.sub <- data.frame(UniqueID = model1.c1c2.logistic.pred$UniqueID, Probability1 = model1.c1c2.logistic.pred$p)
write.csv(model1.c1c2.logistic.sub, "Submission2.logistic2.c1c2.csv", row.names=FALSE)


#--------------------------------
# exploratory
#--------------------------------
par(mfrow=c(3,3)) #par(mfrow=c(1,1))
barplot(table(eBayTrain2.c1$sold, eBayTrain2.c1$biddable), main = 'Biddable', beside=T)
barplot(table(eBayTrain2.c1$sold, eBayTrain2.c1$condition), main = 'Condition', beside=T)
barplot(table(eBayTrain2.c1$sold, eBayTrain2.c1$cellular), main = 'Cellular', beside=T)
barplot(table(eBayTrain2.c1$sold, eBayTrain2.c1$carrier), main = 'Carrier', beside=T)
barplot(table(eBayTrain2.c1$sold, eBayTrain2.c1$color), main = 'Color', beside=T)
barplot(table(eBayTrain2.c1$sold, eBayTrain2.c1$storage), main = 'Storage', beside=T)
barplot(table(eBayTrain2.c1$sold, eBayTrain2.c1$productline), main = 'Product Line', beside=T)
