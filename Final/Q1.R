#Name = the name of the movie
#Year = the year the movie was produced
#Rated = the rating given to the movie by the MPAA
#Runtime = the duration of the movie in minutes
#Action = binary variable that takes value 1 if the movie is an action movie, 0 otherwise
#Adventure, Crime, Drama, Thriller, Fantasy, Horror, Sci.Fi, Comedy, Family, Mystery, Romance, Animation, Music, History, Documentary are all defined like Action
#Wins = number of awards won by the movie
#Nominations = number of awards the movie was nominated for 
#Production.Budget = the natural logarithm of the production budget in dollars
#Worldwide = the natural logarithm of the worldwide revenue in dollars

movies <- read.csv("movies.csv")
summary(movies)
str(movies)

movies.train <- subset(movies, Year < 2010)
movies.test <- subset(movies, Year >= 2010)

#------------------------
#linear regression model
#------------------------
#remove name and year
movies.train2 <- movies.train[ , 3:ncol(movies.train)]

model1 <- lm(Worldwide ~ ., data=movies.train2)
summary(model1)

#only use model1 signficant fields
model2 <- lm(Worldwide ~ Runtime + Crime + Horror + Animation + 
                         History + Nominations + Production.Budget,
             data=movies.train2)
summary(model2)

model2.pred <- predict(model2, newdata=movies.test)

model2.pred.sse <- sum((movies.test$Worldwide - model2.pred)^2)
model2.pred.sst <- sum((mean(movies.train2$Worldwide) - movies.test$Worldwide)^2)
model2.pred.rsq <- 1 - model2.pred.sse/model2.pred.sst

#-------------------------------------------
#classification, using CART
#-------------------------------------------
movies$Performance = factor(ifelse(movies$Worldwide > quantile(movies$Worldwide, .75), "Excellent", 
                            ifelse(movies$Worldwide > quantile(movies$Worldwide, .25), "Average", "Poor")))
table(movies$Performance)

movies$Worldwide = NULL

require(caTools)
set.seed(15071)
split <- sample.split(movies$Performance, .7)
movies.train <- subset(movies, split==T)
movies.test <- subset(movies, split==F)

#remove name and year
movies.train2 <- movies.train[ , 3:ncol(movies.train)]

require(rpart)
require(rpart.plot)
        
model1.cart <- rpart(Performance ~ ., data=movies.train2, method="class")
prp(model1.cart)

model1.cart.train.pred <- predict(model1.cart, type="class")
model1.cart.train.pred.t <- table(model1.cart.train.pred, movies.train2$Performance)
model1.cart.train.acc <-  (sum(model1.cart.train.pred.t[1,1])+
                           sum(model1.cart.train.pred.t[2,2])+
                           sum(model1.cart.train.pred.t[3,3]))/ sum(model1.cart.train.pred.t)

movies.train2.t <- table(movies.train2$Performance)
movies.train2.acc <- max(movies.train2.t) / sum(movies.train2.t)

model1.cart.pred <- predict(model1.cart, newdata=movies.test, type="class")
model1.cart.pred.t <- table(model1.cart.pred, movies.test$Performance)
model1.cart.acc <-  (sum(model1.cart.pred.t[1,1])+
                     sum(model1.cart.pred.t[2,2])+
                     sum(model1.cart.pred.t[3,3]))/ sum(model1.cart.pred.t)

movies.test.t <- table(movies.test$Performance)
movies.test.acc <- max(movies.test.t) / sum(movies.test.t)
