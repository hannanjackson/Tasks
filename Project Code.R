# Null hypothesis: The genotype of clades 126, 215, 222, 305, 313, and 319, of the symbiont, Regiella insecticola, and of the live host, Acyrthosiphon pisum, did not significantly increase the average rate (%) of sporulation per day on Acyrthrosiphon pisum.
# Alternative hypothesis: The genotype of clades 126, 215, 222, 305, 313, and 319, of the symbiont, Regiella insecticola, and of the live host, Acyrthosiphon pisum, significantly increased the average rate (%) of sporulation per day on Acyrthrosiphon pisum.

# I will perform a logistic regression for my data.
# Two graphs will be formed. One for the results of the logistic regression; and a bar graph representing the estimated rate of sporulation in Regiella insecticola infected and non-infected Acyrthosiphon pisum.

setwd("C:\\Users\\justj\\Desktop\\Evolution\\Tasks\\Project")
my_data <- read.csv(file = "data_complete.csv")
write.csv (my_data, 'finalproject.csv')

# Data collected from https://datadryad.org/stash/dataset/doi:10.5061/dryad.6q750
# Data obtained from host_x_symbiont_sporulation tab in the datasheet.
# Used clade number of "Host.Genotype" and "Symbiont.Genotype" for average percentage of "Sporulated" per day for the alternative hypothesis presented below.

my_data[,1]
unique(my_data[,1])
unique(my_data[,2])
host.genotype <- my_data$Host.Genotype
symbiont.genotype <- my_data$Symbiont.Genotype
sporulated <- my_data$Sporulated
summary (my_data)
summary (my_data$Host.Genotype)
summary (my_data$Symbiont.Genotype)
summary (my_data$Sporulated)

# Calculate the average of the host genotype.
x <- c (126, 215, 222, 305, 313, 319)
result.mean <- mean (x)
print (result.mean)
# The average is 250

# Calculate the average of the symbiont genotype.
y <- c (126, 215, 305, 313, 319, 222)
result.mean1 <- mean (y)
print (result.mean1)
# The average is 250

#Calculate the average estimated sporulation rate per day per Acyrthosiphon pisum clade (genotype).
y <- c (1, 1, 1, 1, 1, 1)
result.mean2 <- mean (y)
print (result.mean2)
# The average is 1

# Make a dataframe.
plotting.data <- expand.grid (
  host.genotype = seq(min(my_data$ï..Host.Genotype), max(my_data$ï..Host.Genotype), length.out=30),
  symbiont.genotype = c(min(my_data$Symbiont.Genotype), mean(my_data$Symbiont.Genotype), max(my_data$Symbiont.Genotype))
)

# Median, median, and mode can also be calculated by listing the value with the function.
mean (sporulated)
mean (plotting.data$host.genotype)
mean (symbiont.genotype)
median (sporulated)
median (plotting.data$host.genotype)
median (symbiont.genotype)
mode (sporulated)
mode (plotting.data$host.genotype)
mode (symbiont.genotype)

host.genotype <- c (126, 215, 222, 305, 313, 319)
sporulated <- c (1, 1, 1, 1, 1, 1)
symbiont.genotype <- c (126, 215, 305, 313, 319, 222)

# Calculate the range between the host genotype and symbiont genotype clade values.
my_range <- range (0, host.genotype, symbiont.genotype)

# Plot the average estimated sporulation rate (%) per genotype of the host and symbiont clades.
# Line graph (this is for extra inference, but this graph will not be represented in the paper).
yaxis <- c("0", "0.2", "0.4", "0.6", "0.8", "1")
a <- length(yaxis)
plot (host.genotype, type = "o", col = "blue", axes = FALSE, ann = FALSE)
axis (1, at = 1:6, lab = c ("126", "215", "222", "305", "313", "319"))
axis (3, at = 1:6, lab = c ("0", "0.2", "0.4", "0.6", "0.8", "1"))
box ()
lines (symbiont.genotype, type = "o", pch = 22, lty = 2, col = "red")

title (xlab = "Clade Number", col.lab = rgb (0, 0, 0))
title (ylab = "Mean of % Sporulated", col.lab = rgb (0, 0, 0))
legend("topleft", legend=c("Host Genotype", "Symbiont Genotype"), col=c("red", "blue"), lty=1:2, cex=0.8, bty="n")
head (my_data)
head (my_data$Host.Genotype)
head (my_data$Symbiont.Genotype)
head (my_data$Sporulated)

# Run a logistic regression.
colnames(my_data)
logdat <- my_data
colnames(logdat)[1] <- "Host.Genotype"
LogReg <- glm(Sporulated~as.factor(Host.Genotype)+as.factor(Symbiont.Genotype), family=binomial(link="logit"), data=logdat)

summary(LogReg)

# P value set to greater than 0.05
# Host and symbiont genotype significantly affect the rate (%) of sporulation.

# Create a bar graph.
hostGenoSpor <- tapply(my_data[,3], my_data[,1], function(x) sum(x)/length(x))
symGenoSpor <- tapply(my_data[,3], my_data[,2], function(x) sum(x)/length(x))
barplot(rbind(hostGenoSpor, symGenoSpor[names(hostGenoSpor)]), beside=T, border="black", xlab= "Clade Number", ylab= "Mean of % Sporulated", col=c("red", "blue"))
legend("topright", legend=c("Host Genotype", "Symbiont Genotype"), col=c("blue", "red"), lty=1:2, cex=0.8, lwd = 2)

x1_range <- seq(from=min(my_data$ï..Host.Genotype), to=max(my_data$Symbiont.Genotype), by=.01)
x1_range <- c (126, 215, 305, 313, 319, 222)
x2_val <- c(0, 0.2, 0.4, 0.6, 0.8, 1)

# Plotting logistic regression results.

plot(x1_range, x2_val, xlab= "Clade Number", ylab= "Mean of % Sporulated")
LogReg <- glm(Sporulated~as.factor(Host.Genotype)+as.factor(Symbiont.Genotype), family=binomial(link="logit"), data=logdat)
glm(Sporulated~as.factor(Host.Genotype)+as.factor(Symbiont.Genotype), family=binomial(link="logit"), data=logdat)
f <- ggplot2::ggplot(LogReg, ggplot2::aes(x1_range, x2_val))
abline(h=.5, lty=2)

# The genotype of the host and of the symbiont clades significantly increased the average rate (%) of sporulation per day on Acyrthosiphon pisum.
# The null hypothesis is rejected, and the alternative hypothesis is accepted.
# The null hypothesis is rejected under a logistic regression, as indicated by the p value.