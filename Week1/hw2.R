
#Each data frame has two variables, described as follows:
#  Date: the date of the stock price, always given as the first of the month.
#  StockPrice: the average stock price of the company in the given month.

IBM <- read.csv("IBMStock.csv")
GE <- read.csv("GEStock.csv")
ProcterGamble <- read.csv("ProcterGambleStock.csv")
CocaCola <- read.csv("CocaColaStock.csv")
Boeing <- read.csv("BoeingStock.csv")

#----------------------------------------
# 1
#----------------------------------------
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

min(c(min(IBM$Date),min(GE$Date),min(CocaCola$Date),min(ProcterGamble$Date),min(Boeing$Date)))
max(c(max(IBM$Date),max(GE$Date),max(CocaCola$Date),max(ProcterGamble$Date),max(Boeing$Date)))

mean(IBM$StockPrice)
min(GE$StockPrice)
max(CocaCola$StockPrice)
median(Boeing$StockPrice)
sd(ProcterGamble$StockPrice)

#----------------------------------------
# 2
#----------------------------------------
plot(StockPrice~Date, data=CocaCola,type="l", col="red")
lines(StockPrice~Date, data=ProcterGamble, col="blue")
abline(v=as.Date(c("2000-03-01","1983-01-01")), lwd=2)

#----------------------------------------
# 3
#----------------------------------------
plot(StockPrice~Date, data=CocaCola[301:432,],type="l", col="red", ylim=c(0,210))
lines(StockPrice~Date, data=ProcterGamble[301:432,], col="blue", ylim=c(0,210))
lines(StockPrice~Date, data=IBM[301:432,], col="green", ylim=c(0,210))
lines(StockPrice~Date, data=Boeing[301:432,], col="purple", ylim=c(0,210))
lines(StockPrice~Date, data=GE[301:432,], col="black", ylim=c(0,210))
abline(v=as.Date(c("2000-03-01","1997-09-01","1997-11-01")), lwd=2)

#----------------------------------------
# 4
#----------------------------------------
ibm.monthMean <- tapply(IBM$StockPrice,months(IBM$Date),mean)
ibm.mean <- mean(IBM$StockPrice)
ibm.monthMean > ibm.mean

ge.monthMean <- tapply(GE$StockPrice,months(GE$Date),mean)
coke.monthMean <- tapply(CocaCola$StockPrice,months(CocaCola$Date),mean)
boeing.monthMean <- tapply(Boeing$StockPrice,months(Boeing$Date),mean)
pg.monthMean <- tapply(ProcterGamble$StockPrice,months(ProcterGamble$Date),mean)



