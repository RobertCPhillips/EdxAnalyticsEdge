#    Vandal = 1 if this edit was vandalism, 0 if not.
#    Minor = 1 if the user marked this edit as a "minor edit", 0 if not.
#    Loggedin = 1 if the user made this edit while using a Wikipedia account, 0 if they did not.
#    Added = The unique words added.
#    Removed = The unique words removed.
sw = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "would", "should", "could", "ought", "i'm", "you're", "he's", "she's", "it's", "we're", "they're", "i've", "you've", "we've", "they've", "i'd", "you'd", "he'd", "she'd", "we'd", "they'd", "i'll", "you'll", "he'll", "she'll", "we'll", "they'll", "isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't", "can't", "cannot", "couldn't", "mustn't", "let's", "that's", "who's", "what's", "here's", "there's", "when's", "where's", "why's", "how's", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very")

wiki <- read.csv('wiki.csv', stringsAsFactors=F)
wiki$Vandal = as.factor(wiki$Vandal)

str(wiki)

q1.1 <- sum(wiki$Vandal == 1)
table(wiki$Vandal)

require(tm)
require(SnowballC)

corpusAdded <- Corpus(VectorSource(wiki$Added))
corpusAdded <- tm_map(corpusAdded, PlainTextDocument)
corpusAdded[[1]]$content

corpusAdded <- tm_map(corpusAdded, removeWords, sw) #stopwords("english"))
corpusAdded[[1]]$content

corpusAdded <- tm_map(corpusAdded, stemDocument)
corpusAdded[[1]]$content

dtmAdded <- DocumentTermMatrix(corpusAdded)

sparse <- removeSparseTerms(dtmAdded, .997)
corpusAdded.sparse <- as.data.frame(as.matrix(sparse))
colnames(corpusAdded.sparse) = paste("A", colnames(corpusAdded.sparse))

corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved, PlainTextDocument)
corpusRemoved[[1]]$content

corpusRemoved <- tm_map(corpusRemoved, removeWords, sw) #stopwords("english"))
corpusRemoved[[1]]$content

corpusRemoved <- tm_map(corpusRemoved, stemDocument)
corpusRemoved[[1]]$content

dtmRemoved <- DocumentTermMatrix(corpusRemoved)

sparse <- removeSparseTerms(dtmRemoved, .997)
corpusRemoved.sparse <- as.data.frame(as.matrix(sparse))
colnames(corpusRemoved.sparse) <- paste("R", colnames(corpusRemoved.sparse))

wikiWords <- cbind(corpusAdded.sparse, corpusRemoved.sparse) 
wikiWords$Vandal <- wiki$Vandal
table(wikiWords$Vandal)

require(caTools)
set.seed(123)
split <- sample.split(wikiWords$Vandal, .70)
wikiWords.train <- subset(wikiWords, split==T)
wikiWords.test <- subset(wikiWords, split==F)

#---------------------------------------------------------

require(rpart)
require(rpart.plot)

wikiwords.cart <- rpart(Vandal ~ ., data=wikiWords.train, method="class")
prp(wikiwords.cart)

wikiwords.cart.predict <- predict(wikiwords.cart, newdata=wikiWords.test, type="class")
wikiwords.cart.predict.t <- table(wikiWords.test$Vandal, wikiwords.cart.predict)
wikiwords.cart.acc <- (wikiwords.cart.predict.t[1,1]+wikiwords.cart.predict.t[2,2])/sum(wikiwords.cart.predict.t)

#------------------------------------------

wikiWords2 = wikiWords
wikiWords2$HTTP <- ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
sum(wikiWords2$HTTP)

wikiWords2.train <- subset(wikiWords2, split==T)
wikiWords2.test <- subset(wikiWords2, split==F)

wikiwords2.cart <- rpart(Vandal ~ ., data=wikiWords2.train, method="class")
prp(wikiwords2.cart)

wikiwords2.cart.predict <- predict(wikiwords2.cart, newdata=wikiWords2.test, type="class")
wikiwords2.cart.predict.t <- table(wikiWords2.test$Vandal, wikiwords2.cart.predict)
wikiwords2.cart.acc <- (wikiwords2.cart.predict.t[1,1]+wikiwords2.cart.predict.t[2,2])/sum(wikiwords2.cart.predict.t)

#--------------------------------------------

wikiWords2$NumWordsAdded <- rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved <- rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

wikiWords2b.train <- subset(wikiWords2, split==T)
wikiWords2b.test <- subset(wikiWords2, split==F)

wikiwords2b.cart <- rpart(Vandal ~ ., data=wikiWords2b.train, method="class")
prp(wikiwords2b.cart)

wikiwords2b.cart.predict <- predict(wikiwords2b.cart, newdata=wikiWords2b.test, type="class")
wikiwords2b.cart.predict.t <- table(wikiWords2b.test$Vandal, wikiwords2b.cart.predict)
wikiwords2b.cart.acc <- (wikiwords2b.cart.predict.t[1,1]+wikiwords2b.cart.predict.t[2,2])/sum(wikiwords2b.cart.predict.t)

#------------------------------------------------

wikiWords3 <- wikiWords2
wikiWords3$Minor <- wiki$Minor
wikiWords3$Loggedin <- wiki$Loggedin

wikiWords3.train <- subset(wikiWords3, split==T)
wikiWords3.test <- subset(wikiWords3, split==F)

wikiwords3.cart <- rpart(Vandal ~ ., data=wikiWords3.train, method="class")
prp(wikiwords3.cart)

wikiwords3.cart.predict <- predict(wikiwords3.cart, newdata=wikiWords3.test, type="class")
wikiwords3.cart.predict.t <- table(wikiWords3.test$Vandal, wikiwords3.cart.predict)
wikiwords3.cart.acc <- (wikiwords3.cart.predict.t[1,1]+wikiwords3.cart.predict.t[2,2])/sum(wikiwords3.cart.predict.t)
