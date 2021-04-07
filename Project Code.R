# Null hypothesis: The genotype of clades 126, 215, 222, 305, 313, and 319, of the symbiont, Regiella insecticola, and of the live host, Acyrthosiphon pisum, did not significantly increase the average rate of sporulation per day on cadaver Acyrthrosiphon pisum.
# Alternative hypothesis: The genotype of clades 126, 215, 222, 305, 313, and 319, of the symbiont, Regiella insecticola, and of the live host, Acyrthosiphon pisum, significantly increased the average rate of sporulation per day on cadaver Acyrthrosiphon pisum.

# Here is where I will perform a one-way ANOVA.
# Two graphs will be formed. One for the one-way ANOVA and a bar graph representing the estimated rate of sporulation in Regiella insecticola infected and non-infected cadaver Acyrthosiphon pisum.

setwd("C:\\Users\\justj\\Desktop\\Evolution\\Tasks\\Project")
my_data <- read.csv(file = "data_complete.csv")
write.csv (my_data, 'finalproject.csv')

# Data collected from https://datadryad.org/stash/dataset/doi:10.5061/dryad.6q750
# Data obtained from host_x_symbiont_sporulation tab in the datasheet.
# Used clade number of "Host.Genotype" and "Symbiont.Genotype" for average rate of "Sporulated" per day for the alternative hypothesis presented below.

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
# The average is 256.04

# Calculate the average of the symbiont genotype.
y <- c (126, 215, 305, 313, 319, 222)
result.mean <- mean (y)
print (result.mean)
# The average is 217.2

#Calculate the average sporulation rate per day per cadaver Acyrthosiphon pisum.
y <- c (1, 1, 1, 1, 1, 1)
result.mean <- mean (y)
print (result.mean)
# The average is 1.

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

# Run a one-way ANOVA.
oneway.test(my_data)
# P < 2.2e-16

# P value set to greater than 0.05
# Host and symbiont genotype significantly affect the rate of sporulation.

host.genotype <- c (126, 215, 222, 305, 313, 319)
sporulated <- c (1, 1, 1, 1, 1, 1)
symbiont.genotype <- c (126, 215, 305, 313, 319, 222)

# Calculate the range between the host genotype and symbiont genotype clade values.
my_range <- range (0, host.genotype, symbiont.genotype)

# Plot the sporulation rate per genotype of the host and symbiont clades.
# Line graph.
yaxis <- c("0", "0.2", "0.4", "0.6", "0.8", "1")
a <- length(yaxis)
plot (host.genotype, type = "o", col = "blue", axes = FALSE, ann = FALSE)
axis (1, at = 1:6, lab = c ("126", "215", "222", "305", "313", "319"))
axis (3, at = 1:6, lab = c ("0", "0.2", "0.4", "0.6", "0.8", "1"))
box ()
lines (symbiont.genotype, type = "o", pch = 22, lty = 2, col = "red")

# Main title overlaps with axis so it will not be used for this graph, although I have left the code here.
title (main = "Percent Sporulated of HxG vs. SxG Clade", col.main = "black", font.main = 1)
title (xlab = "Clade Number", col.lab = rgb (0, 0, 0))
title (ylab = "Estimated Rate of Sporulation", col.lab = rgb (0, 0, 0))
legend("bottomright", legend=c("Host Genotype per Clade Number", "Symbiont Genotype per Clade Number"), col=c("red", "blue"), lty=1:2, cex=0.8, lwd = 2, bty = "n")
head (my_data)
head (my_data$Host.Genotype)
head (my_data$Symbiont.Genotype)
head (my_data$Sporulated)

# Create a bar graph.
hostGenoSpor <- tapply(my_data[,3], my_data[,1], function(x) sum(x)/length(x))
symGenoSpor <- tapply(my_data[,3], my_data[,2], function(x) sum(x)/length(x))
barplot(rbind(hostGenoSpor, symGenoSpor[names(hostGenoSpor)]), beside=T, xlab= "Clade Number", ylab= "Estimated Rate of Sporulation", main= "Percent Sporulated of HxG vs. SxG Clade", col=c("red", "blue"))
legend("topright", legend=c("Host Genotype per Clade Number", "Symbiont Genotype per Clade Number"), col=c("blue", "red"), lty=1:2, cex=0.8, lwd = 2, bty = "n")

# The genotype of the host and of the symbiont clades significantly increased the rate of sporulation per day on cadaver Acyrthosiphon pisum.
# The null hypothesis is rejected, and the alternative hypothesis is accepted.
# The null hypothesis is rejected under a one-way ANOVA, as indicated by the p value.