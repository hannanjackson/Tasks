trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm (1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm (1e6, trueMean2, trueSD2)
Size <- 50
Sample1 <- sample (population1, Size)
Sample2 <- sample (population2, Size)
boxplot (Sample1, Sample2)
source ("http://jonsmitchell.com/code/simFxn04.R")
head (makeFounder)
nrow (makeFounder)
MatGrandma <- makeFounder ("grandma_mom")
MatGrandpa <- makeFounder ("grandma_mom")
PatGrandma <- makeFounder ("grandma_da")
PatGrandpa <- makeFounder ("grandpa_da")
Alan <- makeBaby (PatGrandma, PatGrandpa)
head (makeBaby)
nrow (makeBaby)
Brenda <- makeBaby (MatGrandma, MatGrandpa)
Focus <- makeBaby (Brenda, Alan)
ToMom <- length (grep("mom", Focus))/length (Focus)
ToMomMom <- length (grep("grandma_mom", Focus))/length (Focus)
ToMomDad <- length (grep("grandma_mom", Focus))/length (Focus)
Sibling_01 <- makeBaby (Brenda, Alan)
ToSib <- length (intersect(Focus, Sibling_01))/length (Focus)
ManySiblings <- replicate (1e3, length (intersect(Focus, makeBaby(Brenda, Alan)))/length(Focus))
quantile (ManySiblings)
mean (ManySiblings)
plot (density(ManySiblings), main="", xlab="proportion shared genes")
Focus will share the least DNA with the grandma, grandpa, and siblings, and more DNA with Alan and Brenda. Hence, the siblings will share more DNA with Alan and Brenda, and less DNA with Focus and the grandma and grandpa. Traits reach an equillibrium, homogenize, and then can eventually become extinct over time.
HWE <- function (p) {
  aa <- p^2
  ab <- 2 * p * (1 - p)
  bb <- (1 - p)^2
  return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
plot (1, 1, type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="freq. allele a", ylab="geno. freq")
p <- seq (from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p,HWE))
lines (p, GenoFreq[, "aa"], lwd = 2, col='red')
lines (p, GenoFreq[,"ab"], lwd = 2, col = 'purple')
lines (p, GenoFreq[, "bb"], lwd = 2, col = 'blue')
legend ("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), lty = 1, lwd = 2, bty = "n")
Pop <- simPop(500)
points(Pop[, "freqa"], Pop[, "Genotypes.aa"]/500, pch = 21, bg = "red")
Pop <- simPop(50)
points(Pop[, "freqa"], Pop[, "Genotypes.aa"]/50, pch = 22, bg = "red")
As the genotype frequency increased, the frequency of allele aa increased. As the population size decreased, the higher the chance became that the trait will carry on.
library (learnPopGen)
library (learnPopGen)
library (learnPopGen)
x <- genetic.drift(Ne = 200, nrep = 5, pause = 0.01)
x <- genetic.drift(Ne = 1000, nrep = 5, pause = 0.01)
x <- genetic.drift(Ne = 4000, nrep = 5, pause = 0.01)
x <- genetic.drift(Ne = 100, nrep = 5, pause = 0.01)
PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
tExt <- sapply (Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
Line2 <- lm (tExt ~ Samples + 0)
There is more dispersal of traits in the population. Less DNA is shared between the members of the population because of genetic drift- once you reach a finite population size there will be homogenization of a trait(s).