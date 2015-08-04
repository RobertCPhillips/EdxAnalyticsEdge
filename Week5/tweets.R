tweets <- read.csv('tweets.csv', stringsAsFactors=F)
str(tweets)
head(tweets,5)

tweets$Negative <- as.factor(tweets$Avg <= -1)
str(tweets)
table(tweets$Negative)

require(tm)
require(SnowballC)

corpus <- Corpus(VectorSource(tweets$Tweet))
corpus

corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus[[1]]$content
corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]$content

length(stopwords("english"))
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]$content

corpus <- tm_map(corpus, stemDocument)
corpus[[1]]$content

frequencies <- DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005,500:515])
findFreqTerms(frequencies,lowfreq=20)
findFreqTerms(frequencies,lowfreq=100)

sparse <- removeSparseTerms(frequencies, .995)
sparse

tweets.sparse <- as.data.frame(as.matrix(sparse))
str(tweets.sparse)
head(tweets.sparse,5)

colnames(tweets.sparse) <- make.names(colnames(tweets.sparse))
str(tweets.sparse)

tweets.sparse$Negative <- tweets$Negative

require(caTools)
set.seed(123)
split <- sample.split(tweets.sparse$Negative, .70)
tweets.sparse.train <- subset(tweets.sparse, split==T)
tweets.sparse.test <- subset(tweets.sparse, split==F)

require(rpart)
require(rpart.plot)

tweets.sparse.cart <- rpart(Negative~., data=tweets.sparse.train, method="class")
prp(tweets.sparse.cart)

tweets.sparse.cart.predict <- predict(tweets.sparse.cart, newdata=tweets.sparse.test, type="class")
tweets.sparse.cart.predict.t <- table(tweets.sparse.test$Negative, tweets.sparse.cart.predict)
tweets.sparse.cart.acc <- (tweets.sparse.cart.predict.t[1,1]+tweets.sparse.cart.predict.t[2,2])/sum(tweets.sparse.cart.predict.t)

test.sparse.test.base.t <- table(tweets.sparse.test$Negative)
test.sparse.test.base.acc <- max(test.sparse.test.base.t)/sum(test.sparse.test.base.t)

require(randomForest)
set.seed(123)

tweets.sparse.forrest <- randomForest(Negative ~ ., data=tweets.sparse.train)

tweets.sparse.forrest.predict <- predict(tweets.sparse.forrest, newdata=tweets.sparse.test)
tweets.sparse.forrest.predict.t <- table(tweets.sparse.test$Negative, tweets.sparse.forrest.predict)
tweets.sparse.forrest.acc <- (tweets.sparse.forrest.predict.t[1,1]+tweets.sparse.forrest.predict.t[2,2])/sum(tweets.sparse.forrest.predict.t)

tweets.sparse.logistic <- glm(Negative ~ ., data=tweets.sparse.train, family="binomial")
summary(tweets.sparse.logistic) 

tweets.sparse.logistic.predict <- predict(tweets.sparse.logistic, tweets.sparse.test, type="response")
tweets.sparse.logistic.t <- table(tweets.sparse.test$Negative, tweets.sparse.logistic.predict >= .50)
tweets.sparse.logistic.acc <- (tweets.sparse.logistic.t[1,1]+tweets.sparse.logistic.t[2,2])/sum(tweets.sparse.logistic.t)




























