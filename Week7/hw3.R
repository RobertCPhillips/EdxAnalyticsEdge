#Tweet -- the text of the tweet
#Avg   -- the sentiment of the tweet, as assigned by users of Amazon 
#         Mechanical Turk. The score ranges on a scale from -2 to 2, 
#         where 2 means highly positive sentiment, -2 means highly negative 
#         sentiment, and 0 means neutral sentiment.
tweets <- read.csv('tweets.csv', stringsAsFactors=F)
str(tweets)

require(tm)

corpus <- Corpus(VectorSource(tweets$Tweet))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, removePunctuation)

length(stopwords("english"))
corpus <- tm_map(corpus, removeWords, c(stopwords("english")))

frequencies <- DocumentTermMatrix(corpus)
tweets.all <- as.data.frame(as.matrix(frequencies))

require(wordcloud)

wordcloud(colnames(tweets.all), colSums(tweets.all))

#-------------------------------------------------

corpus2 <- Corpus(VectorSource(tweets$Tweet))
corpus2 <- tm_map(corpus2, tolower)
corpus2 <- tm_map(corpus2, PlainTextDocument)
corpus2 <- tm_map(corpus2, removePunctuation)
corpus2 <- tm_map(corpus2, removeWords, c("apple", stopwords("english")))

frequencies2 <- DocumentTermMatrix(corpus2)
tweets.all2 <- as.data.frame(as.matrix(frequencies2))

wordcloud(colnames(tweets.all2), colSums(tweets.all2),scale=c(2, 0.25), colors=brewer.pal(8,"Greys"))
