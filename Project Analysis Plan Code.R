# Null hypothesis: The genotype of clades 126, 215, 222, 305, 313, and 319, of the symbiont, Regiella insecticola, and of the live host, Acyrthosiphon pisum, did not significantly increase the average rate of sporulation per day on cadaver Acyrthrosiphon pisum.
# Alternative hypothesis: The genotype of clades 126, 215, 222, 305, 313, and 319, of the symbiont, Regiella insecticola, and of the live host, Acyrthosiphon pisum, significantly increased the average rate of sporulation per day on cadaver Acyrthrosiphon pisum.

# Here is where I will perform a two-way ANOVA and multivariable linear regression (if possible) on the parameters mentioned above.
# Two graphs will be formed. One for the two-way ANOVA and one for the multivariable linear regression.
# Here, I've written some "practice" code using a one-way ANOVA (although I want to perform a two-way ANOVA on the data).

setwd("C:\\Users\\justj\\Desktop\\Evolution\\Tasks\\Project")
my_data <- read.csv(file = "data_complete.csv")
write.csv (my_data, 'analysisplanhypothesis.csv')

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

# Check for correlation in the variables.
# This is the first step in a multivariable linear regression.
cor(my_data$ï..Host.Genotype, my_data$Symbiont.Genotype)
# -0.12

# Check to see if dependent variable, average rate of sporulation, follows a normal distribution.
hist (my_data$Sporulated)

# Sample graphs for a regression.
library (ggplot2)
sporulated.plot <- ggplot (my_data, aes (x=host.genotype, y=sporulated))
geom_point ()

library (ggplot2)
sporulated.plot2 <- ggplot (my_data, aes (x=symbiont.genotype, y=sporulated))
geom_point ()

# Back to the sample ANOVA!
# Run a one-way ANOVA. I'm running a one-way ANOVA for THIS example.
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
# We get a line graph in this example, although I want to produce a bar graph with the data.
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
# The null hypothesis is rejected, and the alternative hypothesis is accepted.

# The null hypothesis, in this example, would be rejected under a one-way ANOVA, as indicated by the p value.