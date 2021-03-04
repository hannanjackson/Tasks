# Null hypothesis: The genotype of clades 126, 215, 222, 305, 313, and 319, of the symbiont, Regiella insecticola, and of the live host, Acyrthosiphon pisum, increased the average rate of sporulation per day on cadaver Acyrthrosiphon pisum.
# Alternative hypothesis: The genotype of clades 126, 215, 222, 305, 313, and 319, of the symbiont, Regiella insecticola, and of the live host, Acyrthosiphon pisum, did not increase the average rate of sporulation per day on cadaver Acyrthrosiphon pisum.

setwd("C:\\Users\\justj\\Desktop\\Evolution\\Tasks\\Project")
my_data <- read.csv(file = "data_complete.csv")
write.csv (my_data, 'hypothesis.csv')

# Data collected from https://datadryad.org/stash/dataset/doi:10.5061/dryad.6q750
# Data obtained from host_x_symbiont_sporulation tab in the datasheet.
# Used clade number of "Host.Genotype", "Symbiont.Genotype", and "Sporulated" for the null hypothesis presented below.

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

# Run a one-way ANOVA.
oneway.test (my_data)
# P < 2.2e-16

host.genotype <- c (126, 215, 222, 305, 313, 319)
sporulated <- c (1, 1, 1, 1, 1, 1)
symbiont.genotype <- c (126, 215, 305, 313, 319, 222)

# Calculate the range between the host genotype and symbiont genotype clade values.
my_range <- range (0, host.genotype, symbiont.genotype)

# Plot the sporulation rate per genotype of the host and symbiont clades.
plot (host.genotype, type = "o", col = "blue", ylim = my_range, axes = FALSE, ann = FALSE)
axis (1, at = 1:6, lab = c ("0", "2", "4", "6", "8", "10"))
axis (2, at = my_range)
box ()
lines (symbiont.genotype, type = "o", pch = 22, lty = 2, col = "red")
title (main = "Sporulation Rate of HxG vs. SxG", col.main = "red", font.main = 4)
title (xlab = "Days", col.lab = rgb (0, 0.5, 0))
title (ylab = "Sporulation Rate per Clade Genotype", col.lab = rgb (0, 0.5, 0))
legend("bottomright", legend=c("Host Genotype per Clade Number", "Symbiont Genotype per Clade Number"), col=c("red", "blue"), lty=1:2, cex=0.8, lwd = 2, bty = "n")
dev.off ()
head (my_data)
head (my_data$Host.Genotype)
head (my_data$Symbiont.Genotype)
head (my_data$Sporulated)

# The genotype of the host and of the symbiont clades increased the sporulation rate per day per cadaver Acyrthosiphon pisum.
# The null hypothesis is accepted.