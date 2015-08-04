#The dataset contains just two fields:
#  text: The text of the email.
#  spam: A binary variable indicating if the email was spam.


emails <- read.csv('emails.csv', stringsAsFactors=F)
str(emails)
summary(emails)
head(emails$text, 10)

sum(emails$spam == 1)
table(emails$spam)

max(nchar(emails$text))
which(nchar(emails$text) == min(nchar(emails$text)))

#------------------------------------------------------
sw <- c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "would", "should", "could", "ought", "i'm", "you're", "he's", "she's", "it's", "we're", "they're", "i've", "you've", "we've", "they've", "i'd", "you'd", "he'd", "she'd", "we'd", "they'd", "i'll", "you'll", "he'll", "she'll", "we'll", "they'll", "isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't", "can't", "cannot", "couldn't", "mustn't", "let's", "that's", "who's", "what's", "here's", "there's", "when's", "where's", "why's", "how's", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very")

require(tm)
corpus <-  Corpus(VectorSource(emails$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, sw)
corpus <- tm_map(corpus, stemDocument)
dtm <- DocumentTermMatrix(corpus)
spdtm <- removeSparseTerms(dtm, .95)

emailsSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) <- make.names(colnames(emailsSparse))
sort(colSums(emailsSparse))

emailsSparse$spam <- emails$spam

hamcount <- colSums(subset(emailsSparse, spam == 0, select= -spam))
hamcountGt5000 <- subset(hamcount, hamcount >= 5000)

spamcount <- colSums(subset(emailsSparse, spam == 1, select= -spam))
spamcountGt1000 <- subset(spamcount, spamcount >= 1000)

#---------------------------------------------------------

emailsSparse$spam <- as.factor(emailsSparse$spam)

require(ROCR)
require(caTools)
set.seed(123)
split <- sample.split(emailsSparse$spam, .70)
train <- subset(emailsSparse, split==T)
test <- subset(emailsSparse, split==F)

#---------------------------------------------------------

emailsSparse.logistic <- glm(spam ~ ., data=train, family="binomial")
emailsSparse.logistic.predict.res <- predict(emailsSparse.logistic, type="response")
sum(emailsSparse.logistic.predict.res < 0.00001)
sum(emailsSparse.logistic.predict.res > 0.99999)
sum(emailsSparse.logistic.predict.res <= 0.99999 & emailsSparse.logistic.predict.res >= 0.00001)

summary(emailsSparse.logistic)

emailsSparse.logistic.t <- table(train$spam, emailsSparse.logistic.predict.res >= .5)
emailsSparse.logistic.acc <- (emailsSparse.logistic.t[1,1]+emailsSparse.logistic.t[2,2])/sum(emailsSparse.logistic.t)
emailsSparse.logistic.rocr <- prediction(emailsSparse.logistic.predict.res, train$spam)
emailsSparse.logistic.auc <- as.numeric(performance(emailsSparse.logistic.rocr, "auc")@y.values)

emailsSparse.logistic.predict.res2 <- predict(emailsSparse.logistic, newdata=test, type="response")
emailsSparse.logistic.t2 <- table(test$spam, emailsSparse.logistic.predict.res2 >= .5)
emailsSparse.logistic.acc2 <- (emailsSparse.logistic.t2[1,1]+emailsSparse.logistic.t2[2,2])/sum(emailsSparse.logistic.t2)
emailsSparse.logistic.rocr2 <- prediction(emailsSparse.logistic.predict.res2, test$spam)
emailsSparse.logistic.auc2 <- as.numeric(performance(emailsSparse.logistic.rocr2, "auc")@y.values)

#---------------------------------------------------------

require(rpart)
require(rpart.plot)

emailsSparse.cart <- rpart(spam ~ ., data=train, method="class")
emailsSparse.cart.predict.res <- predict(emailsSparse.cart)
prp(emailsSparse.cart)

emailsSparse.cart.predict.t <- table(train$spam, emailsSparse.cart.predict.res[,2] >= .5)
emailsSparse.cart.acc <- (emailsSparse.cart.predict.t[1,1]+emailsSparse.cart.predict.t[2,2])/sum(emailsSparse.cart.predict.t)
emailsSparse.cart.rocr <- prediction(emailsSparse.cart.predict.res[,2], train$spam)
emailsSparse.cart.auc <- as.numeric(performance(emailsSparse.cart.rocr, "auc")@y.values)

emailsSparse.cart.predict.res2 <- predict(emailsSparse.cart, newdata=test)
emailsSparse.cart.predict.t2 <- table(test$spam, emailsSparse.cart.predict.res2[,2] >= .5)
emailsSparse.cart.acc2 <- (emailsSparse.cart.predict.t2[1,1]+emailsSparse.cart.predict.t2[2,2])/sum(emailsSparse.cart.predict.t2)
emailsSparse.cart.rocr2 <- prediction(emailsSparse.cart.predict.res2[,2], test$spam)
emailsSparse.cart.auc2 <- as.numeric(performance(emailsSparse.cart.rocr2, "auc")@y.values)
#---------------------------------------------------------

set.seed(123)
emailsSparse.forrest <- randomForest(spam ~ ., data=train)
emailsSparse.forrest.predict.res <- predict(emailsSparse.forrest, type="prob")

emailsSparse.forrest.predict.t <- table(train$spam, emailsSparse.forrest.predict.res[,2] >= .5)
emailsSparse.forrest.acc <- (emailsSparse.forrest.predict.t[1,1]+emailsSparse.forrest.predict.t[2,2])/sum(emailsSparse.forrest.predict.t)

emailsSparse.forrest.rocr <- prediction(emailsSparse.forrest.predict.res[,2], train$spam)
emailsSparse.forrest.auc <- as.numeric(performance(emailsSparse.forrest.rocr, "auc")@y.values)

emailsSparse.forrest.predict.res2 <- predict(emailsSparse.forrest, newdata=test, type="prob")
emailsSparse.forrest.predict.t2 <- table(test$spam, emailsSparse.forrest.predict.res2[,2] >= .5)
emailsSparse.forrest.acc2 <- (emailsSparse.forrest.predict.t2[1,1]+emailsSparse.forrest.predict.t2[2,2])/sum(emailsSparse.forrest.predict.t2)
emailsSparse.forrest.rocr2 <- prediction(emailsSparse.forrest.predict.res2[,2], test$spam)
emailsSparse.forrest.auc2 <- as.numeric(performance(emailsSparse.forrest.rocr2, "auc")@y.values)























