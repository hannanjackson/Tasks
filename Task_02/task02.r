setwd('C:\\Desktop\\Evolution\\Tasks\\Task_02')
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
You can use a variable (like with event) or a number (like with 9) to access variables in your data-set ("bottles"). You can also use $ like in the last step to access variables in a data-set, such as "bottles."
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
The data indicates that Beren increased the amount he fed over time, showing that the amount of milk he drinks has changed over time.
beren <- read.csv ("http://jonsmitchell.com/data/beren.csv", stringsAsFactors = F)
dayID <- apply (beren, 1, function (x) paste (x[1:3], collapse = "-"))
dateID <- sapply (dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
beren$age <- dateID - dateID [which(beren$event == "birth")]
beren2 <- beren [order(beren$age),]
beren2$value <- as.numeric (beren2$value)
Feeds <- which (beren3$event == "bottle")
avgMilk <- mean (beren3$value [Feeds])
avgFeed <- tapply (beren3$value [Feeds], beren3$age [Feeds], mean)
varFeed <- tapply (beren3$value [Feeds], beren3$age [Feeds], var)
totalFeed <- tapply (beren3$value [Feeds], beren3$age [Feeds], sum)
numFeeds <- tapply (beren3$value [Feeds], beren3$age [Feeds], length)
?cor
cor (beren3$value [Feeds], beren3$age [Feeds])
head (Feeds)
head (beren3 [Feeds])
cor.test (beren3$value [Feeds], beren3$age [Feeds])
berenCor <??? cor.test (beren3$value[Feeds], beren3$age[Feeds])
summary (berenCor)
berenANOVA <- aov (beren3$value [Feeds] ~ beren3$caregiver [Feeds])
boxplot (beren3$value [Feeds] ~ beren3$caregiver [Feeds], xlab= "who gave the bottle", ylab= "amount of milk consumed (oz)")
?par
par (las= 1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline (h=mean(totalFeed), lty=2, col='red')
pdf ("r02b - totalMilkByDay.pdf", height = 4, width = 4)
par (las = 1, mar = c (5,5,1,1), mgp = c (2,0.5.0), tck = -0.01)
plot (as.numeric(names(totalFeed)), totalFeed, type = "b", pch = 16, xlab = "age in days", ylab = "ounces of milk")
abline (h = mean(totalFeed), lty = 2, col = 'red')
dev.off()