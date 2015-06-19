bb <- read.csv('baseball.csv')
str(bb)

mb <- subset(bb, Year < 2002)
mb$RD <- mb$RS - mb$RA
str(mb)

plot(W~RD,data=mb)
winsreg <- lm(W~RD,data=mb)
summary(winsreg)

predict(winsreg,newdata=data.frame(RD=713-614))

runsreg <- lm(RS~OBP+SLG+BA,data=mb)
summary(runsreg)

cor(mb[,c("OBP","SLG","BA")])

runsreg2 <- lm(RS~OBP+SLG,data=mb)
summary(runsreg2)
predict(runsreg2,data.frame(OBP=.311,SLG=.405))
predict(runsreg2,data.frame(OBP=c(.338,.391,.369,.313,.361),SLG=c(.54,.45,.374,.447,.5)))

runsreg3 <- lm(RA~OOBP+OSLG,data=mb)
summary(runsreg3)

----------------
teamRank <- c(1,2,3,3,4,4,4,4,5,5)
wins2012 <- c(94,88,95,88,93,94,98,97,93,94)
wins2013 <- c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank,wins2012)
cor(teamRank,wins2013)














