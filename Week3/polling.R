poll <- read.csv("pollingdata.csv")
str(poll)
summary(poll)

require(mice)
simple <- poll[c("Rasmussen","SurveyUSA","DiffCount","PropR")]

set.seed(144)
imputed <- complete(mice(simple))
summary(imputed)

poll$Rasmussen <- imputed$Rasmussen
poll$SurveyUSA <- imputed$SurveyUSA
summary(poll)

ptrain <- subset(poll,Year == 2004 | Year == 2008)
ptest <- subset(poll,Year == 2012)

table(ptrain$Republican)
table(sign(ptrain$Rasmussen))
table(ptrain$Republican,sign(ptrain$Rasmussen))

cor(ptrain[c("Rasmussen","SurveyUSA","DiffCount","PropR","Republican")])

mod1 <- glm(Republican~PropR, data=ptrain, family="binomial")
summary(mod1)

mod1.pred <- predict(mod1,type="response")
table(ptrain$Republican,mod1.pred >= .5)

mod2 <- glm(Republican~SurveyUSA+DiffCount, data=ptrain, family="binomial")
summary(mod2)

mod2.pred <- predict(mod2, type="response")
table(ptrain$Republican,mod2.pred >= .5)

table(ptest$Republican,sign(ptest$Rasmussen))

mod2.test <- predict(mod2,ptest)
table(ptest$Republican,mod2.test >= .5)
