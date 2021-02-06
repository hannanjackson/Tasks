Data <- read.csv ('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
write.csv (Data, 'rawdata.csv', quote=F)
length('Data')
nrow('Data')
ncol('Data')
colnames('Data')
head('Data')
Data [1,]
Data [2,]
Data [1:3,]
Data [1:3, 4]
Data [1:5, 1:3]
Feeds <- which (Data[,9] == 'bottle')
berenMilk <- Data[Feeds,]
head (berenMilk)
Feeds <- which (Data[,'event'] == 'bottle')
Feeds <- which (Data$event == 'bottle')
dayID <- apply (Data, 1, function(x) paste(x[1:3], collapse = '-'))
dateID <- sapply (dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == 'birth')]
head(Data)
beren2 <- Data
beren3 <- beren2[order(beren2$age),]
berenMilk <- head(Data)
beren2 <- head(Data)
beren3 <- head(Data)
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
beren <- read.csv ("http://jonsmitchell.com/data/beren.csv", stringsAsFactors = F)
dayID <- apply (beren, 1, function (x) paste (x[1:3], collapse = "-"))
dateID <- sapply (dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
beren$age <- dateID - dateID [which(beren$event == "birth")]
beren2 <- beren [order(beren$age),]
beren2$value <- as.numeric (beren2$value)
source ("http://jonsmitchell.com/code/plotFxn02b.R")
Naps <- which (beren3$event == "naps")
avgNaps <- mean (beren3$value [Naps])
avgNaps <- tapply (beren3$value [Naps], beren3$age [Naps], mean)
varNaps <- tapply (beren3$value [Naps], beren3$age [Naps], var)
totalNaps <- tapply (beren3$value [Naps], beren3$age [Naps], sum)
numNaps <- tapply (beren3$value [Naps], beren3$age [Naps], length)
cor (beren3$value [Naps], beren3$age [Naps])
head (beren3$event == "naps")
head (beren3$day)
par (las= 1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(napTs), napTs, type="b", pch=16, xlab="age in days", ylab="number of naps per day")
abline (h=mean(napTs), lty=2, col='red')
pdf ("r02b - totalNapsByDay.pdf", height = 4, width = 4)
par (las = 1, mar = c (5,5,1,1), mgp = c (2,0.5,0), tck = -0.01)
plot (as.numeric(napTs), napTs, type = "b", pch = 16, xlab = "age in days", ylab = "number of naps per day")
abline (h = mean(totalNaps), lty = 'dashed', col = 'red')
dev.off()