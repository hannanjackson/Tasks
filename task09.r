setwd ('C:\\Users\\justj\\Desktop\\Evolution\\Tasks\\Task_09')
library (phytools)

# Questions 1-3: Simulate some trees!
desired_length <- 100
trees <- c(a=1, b=100)
is.vector(trees)
as.vector(trees)
all.equal(trees, as.vector(trees))
trees
births <- rep(25, 100)
births
Fractions <- rep(50, 100)
Fractions
n = 100
births[[50]]
Fractions[[25]]
for (i in 1:n) {
  births[[50]] = Fractions[[25]]
  trees <- pbtree (b=50, d=50*25, n=100)
  print (i)
}
pbtree (b=50, d=50*25, n=100)

# Question 4: How does the net diversification rate for each tree compare to the log of the total number of tips for each tree?
library(ape)
library(geiger)
library(apTreeshape)
mybranchtimes <- branching.times(trees)

# Making a plot.
ltt.plot(trees, log="y")
ltt.lines(trees, lty=2, )
bd.ms(time=100, n=100)

# Diversification rate: 0.03912023, rounded to 0.04
bd.km(time=100, n=100)
log(trees$edge.length)
# Log: -9.257716 rounded to -9.2577, -6.966852 rounded to -6.9669

# The net diversification rate is higher than the log of the total number of tips.

# Question 5: How does the speciation rate for each tree compare to the average branch length for each tree?

trees$edge.length
# Branch lengths: 9.537293e-05, 9.426157e-04

plotTree(trees, fsize=1.2, ftype="i", lwd= 2, bty= 2)
nodelabels(cex= .75, bg= "yellow")
edgelabels(cex=.75, bg= "pink")
tiplabels(cex=0.5, bg= "green")
bd.km(trees, time=10, n=50)
speciationrate <- 1:2

# The speciation rate is greater than that of the average branch length.

# Question 6: Use the cor() function to find the numerical relationship between speciation rate and average branch length.

cor(1:2, 1:2)
# 1

# Question 7: Simulate traits on a tree.
Tree <- rnorm(100)
plot(Tree, type= 'b')
box(lwd=0.5, which= "plot", lty= 1373)
lines(Tree, col= "red", pch=22)
rates <- c(a= 1:10, b= 1:10)
rates1 <- c(a= 1:10, b= 1:10)
is.vector(rates)
as.vector(rates)
all.equal(rates, as.vector(rates))
rates
traits <- rep(1:10, 2)
traits
rates[[10]]
for (i in 1:n) {
  rates[[10]] = traits[[10]]
  rates <- fastBM (trees, sig2= 10)
  print (i)
}
fastBM(trees, sig2= 10)

# Question 8: What is the correlation between the mean of traits and the rates?
y <- mean(traits)
# 5.5
x <- mean(rates)
# 5.5
C1 <- cor(x, y)
cor(5.5, 5.5)
plot (traits, rates1, type = "b", col = "blue", axes = FALSE, ann = FALSE)
axis (1, at = 1:10, lab = c (1:10))
axis (2, at = 1:10)
box ()
lines (traits, rates1, type = "o", pch = 22, lty = 2, col = "red")
title (ylab= "Rates", col.main = "red", font.main = 4)
title (xlab = "Traits", col.lab = rgb (0, 0.5, 0))
# There is positive correlation between the mean of traits and rates.

# Question 9: What is the correlation between the variance of traits and rates?
a <- var(traits)
# 8.68
b <- var(rates)
# 8.68
C2 <- cor(8.68, 8.68)
cor(a, b)
# There is no correlation between the variance of traits and rates.

# Question 10: What is the correlation between the first element of traits and the second element of traits? Is it significant and why?
traitMat <- cbind(traits[[1]], traits[[4]])
cor(traitMat)

# There is no correlation between the first element of traits and the second element of traits, therefore, it is not significant because there is no given relation between the two elements.