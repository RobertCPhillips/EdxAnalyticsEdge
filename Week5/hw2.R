
trials <- read.csv('clinical_trial.csv', stringsAsFactors=F)
str(trials)
summary(trials)

trials$title.len <- nchar(trials$title)
trials$abstract.len <- nchar(trials$abstract)

max(trials$abstract.len)
sum(trials$abstract.len == 0)

which(trials$title.len == min(trials$title.len))
trials$title[1258]

#------------------------------------------------------
sw <- c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "would", "should", "could", "ought", "i'm", "you're", "he's", "she's", "it's", "we're", "they're", "i've", "you've", "we've", "they've", "i'd", "you'd", "he'd", "she'd", "we'd", "they'd", "i'll", "you'll", "he'll", "she'll", "we'll", "they'll", "isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't", "can't", "cannot", "couldn't", "mustn't", "let's", "that's", "who's", "what's", "here's", "there's", "when's", "where's", "why's", "how's", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very")

require(tm)
corpusTitle <-  Corpus(VectorSource(trials$title))       #trials$title
corpusAbstract <-  Corpus(VectorSource(trials$abstract)) #trials$abstract

corpusTitle <- tm_map(corpusTitle, tolower)
corpusAbstract <- tm_map(corpusAbstract, tolower)

corpusTitle <- tm_map(corpusTitle, PlainTextDocument)
corpusAbstract <- tm_map(corpusAbstract, PlainTextDocument)

corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract <- tm_map(corpusAbstract, removePunctuation)

corpusTitle <- tm_map(corpusTitle, removeWords, sw)
corpusAbstract <- tm_map(corpusAbstract, removeWords, sw)

corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)

dtmTitle <- removeSparseTerms(dtmTitle, .95)
dtmAbstract <- removeSparseTerms(dtmAbstract, .95)

dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))

colnames(dtmTitle) <- make.names(colnames(dtmTitle))
colnames(dtmAbstract) <- make.names(colnames(dtmAbstract))

max(colSums(dtmAbstract))

colnames(dtmTitle) <- paste("T", colnames(dtmTitle))
colnames(dtmAbstract) <- paste("A", colnames(dtmAbstract))

dtm <- cbind(dtmTitle, dtmAbstract)
dtm$trial <- trials$trial

require(caTools)
set.seed(144)
split <- sample.split(dtm$trial, .70)
train <- subset(dtm, split==T)
test <- subset(dtm, split==F)

train.t <- table(train$trial)
max(train.t)/sum(train.t)

#---------------------------------------------------------

require(rpart)
require(rpart.plot)

cart1 <- rpart(trial ~ ., data=train, method="class")
prp(cart1)

cart1.predict.res <- predict(cart1,data=train)
max(cart1.predict.res[,2])

cart1.predict1 <- predict(cart1, newdata=train, type="class")
cart1.predict1.t <- table(train$trial, cart1.predict1)
cart1.acc1 <- (cart1.predict1.t[1,1]+cart1.predict1.t[2,2])/sum(cart1.predict1.t)
cart1.sens1 <- cart1.predict1.t[2,2]/(cart1.predict1.t[2,2] + cart1.predict1.t[2,1])
cart1.spec1 <- cart1.predict1.t[1,1]/(cart1.predict1.t[1,1] + cart1.predict1.t[1,2])

cart1.predict2 <- predict(cart1, newdata=test, type="class")
cart1.predict2.t <- table(test$trial, cart1.predict2)
cart1.acc2 <- (cart1.predict2.t[1,1]+cart1.predict2.t[2,2])/sum(cart1.predict2.t)

require(ROCR)
cart1.predict2.res <- predict(cart1, newdata=test)
cart1.rocr1 <- prediction(cart1.predict2.res[,2], test$trial)
#cart1.perf1 <- performance(gerber.logistic.rocr, "tpr", "fpr")
cart1.auc1 <- as.numeric(performance(cart1.rocr1, "auc")@y.values)










