
# - letter = the letter that the image corresponds to (A, B, P or R)
# - xbox = the horizontal position of where the smallest box covering the letter shape begins.
# - ybox = the vertical position of where the smallest box covering the letter shape begins.
# - width = the width of this smallest box.
# - height = the height of this smallest box.
# - onpix = the total number of "on" pixels in the character image
# - xbar = the mean horizontal position of all of the "on" pixels
# - ybar = the mean vertical position of all of the "on" pixels
# - x2bar = the mean squared horizontal position of all of the "on" pixels in the image
# - y2bar = the mean squared vertical position of all of the "on" pixels in the image
# - xybar = the mean of the product of the horizontal and vertical position of all of the "on" pixels in the image
# - x2ybar = the mean of the product of the squared horizontal position and the vertical position of all of the "on" pixels
# - xy2bar = the mean of the product of the horizontal position and the squared vertical position of all of the "on" pixels
# - xedge = the mean number of edges (the number of times an "off" pixel is followed by an "on" pixel, or the image boundary is hit) as the image is scanned from left to right, along the whole vertical length of the image
# - xedgeycor = the mean of the product of the number of horizontal edges at each vertical position and the vertical position
# - yedge = the mean number of edges as the images is scanned from top to bottom, along the whole horizontal length of the image
# - yedgexcor = the mean of the product of the number of vertical edges at each horizontal position and the horizontal position

abpr <- read.csv("letters_ABPR.csv")
str(abpr)
head(abpr)

abpr$isb <- as.factor(abpr$letter == "B")

set.seed(1000)
split <- sample.split(abpr$isb, .50)
abpr.train <- subset(abpr, split==T)
abpr.test <- subset(abpr, split==F)

abpr.base.t <- table(abpr.test$isb)
q1.1<- abpr.base.t[1]/sum(abpr.base.t)

require(rpart)
require(rpart.plot)

abpr.cart <- rpart(isb ~ . - letter, data=abpr.train, method="class")
prp(abpr.cart)

abpr.cart.predict <- predict(abpr.cart, newdata=abpr.test, type="class")
abpr.cart.predict.t <- table(abpr.test$isb,abpr.cart.predict)
abpr.cart.acc <- (abpr.cart.predict.t[1,1]+abpr.cart.predict.t[2,2])/sum(abpr.cart.predict.t)


require(randomForest)
set.seed(1000)
abpr.forrest <- randomForest(isb ~ . - letter, data=abpr.train)

abpr.forrest.predict <- predict(abpr.forrest, newdata=abpr.test, type="class")
abpr.forrest.predict.t <- table(abpr.test$isb, abpr.forrest.predict)
abpr.forrest.acc <- (abpr.forrest.predict.t[1,1]+abpr.forrest.predict.t[2,2])/sum(abpr.forrest.predict.t)

#-----------------------------------------------------------
str(abpr)
abpr$letter <- as.factor(abpr$letter) 

set.seed(2000)
split2 <- sample.split(abpr$letter, .50)
abpr.train2 <- subset(abpr, split2==T)
abpr.test2 <- subset(abpr, split2==F)

abpr.base2.t <- table(abpr.test2$letter)
q2.1<- max(abpr.base2.t)/sum(abpr.base2.t)

abpr.cart2 <- rpart(letter ~ . - isb, data=abpr.train2, method="class")
prp(abpr.cart2)

abpr.cart2.predict <- predict(abpr.cart2, newdata=abpr.test2, type="class")
abpr.cart2.predict.t <- table(abpr.test2$letter,abpr.cart2.predict)
abpr.cart2.acc <- (abpr.cart2.predict.t[1,1]+abpr.cart2.predict.t[2,2] +
                   abpr.cart2.predict.t[3,3]+abpr.cart2.predict.t[4,4])/sum(abpr.cart2.predict.t)

#-----------------------------------------------
set.seed(1000)
abpr.forrest2 <- randomForest(letter ~ . - isb, data=abpr.train2)

abpr.forrest2.predict <- predict(abpr.forrest2, newdata=abpr.test2, type="class")
abpr.forrest2.predict.t <- table(abpr.test2$letter, abpr.forrest2.predict)
abpr.forrest2.acc <- (abpr.forrest2.predict.t[1,1]+abpr.forrest2.predict.t[2,2]+
                        abpr.forrest2.predict.t[3,3]+abpr.forrest2.predict.t[4,4])/sum(abpr.forrest2.predict.t)





