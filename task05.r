learnPopGen::coalescent.plot(n = 10, ngen = 20, lwd = 2, colors = NULL)
learnPopGen::coalescent.plot(n = 20, ngen = 30, lwd = 2, colors = NULL)
learnPopGen::coalescent.plot(n = 30, ngen = 40, lwd = 2, colors = NULL)
# Begins with 2 alleles (diploid); 1 allele for haploid. Modified through col.order using sequential or alternating (mimics selection and drift).
# 4n generations for the allele to go to fixation.
# The haploid individual will produce half the amount of offspring that the diploid individual has. The longer the generation time, the greater the variance is in offspring, given selection occurs, then drift.
# The longer the generation time, the better the allele is at adapting to its environment, hence, having an increased fitness.
# Yes, the MRCA is alive in generation 0 at the focal locus, considering that is where the allele(s) first originated.
model <- coala::coal_model (sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) + coala::feat_mutation (10) + coala::feat_recombination (10) + coala::sumstat_trees() + coala::sumstat_nucleotide_div ()
stats <- simulate (model, nsim = 1)
Diversity <- stats$pi
Nloci <- length (stats$trees)
t1 <- ape::read.tree (text=stats$trees [[1]][1])
plot (t1)
ape::axisPhylo ()
# Individuals die off or are born into the population. Effective population size changes with its environment.
Agel <- max (ape::node.height(t1))
t2 <- ape::read.tree (text=stats$trees [[2]][1])
plot (t2)
ape::axisPhylo()
par (mfrow=c(1,2))
plot (t1)
ape::axisPhylo()
plot (t2)
ape::axisPhylo()
# The plots do not match.
ape::comparePhylo(t1, t2)
t1_1 <- ape::read.tree (text=stats$trees [[1]][1])
t1_2 <- ape::read.tree (text=stats$trees [[1]][2])
ape::comparePhylo(t1_1, t1_2)
for (locus in 1:Nloci) {
  ntrees <- length (stats$trees [[locus]])
  for (n in 1:ntrees) {
    if (locus == 1 && n == 1) {
      outPhy <- ape::read.tree (text=stats$trees [[locus]][n])
    }
    else {
      outPhy <- ape:::c.phylo(outPhy, ape::read.tree (text=stats$trees [[locus]][n]))
    }
  }
}
par (mfrow=c(1,1))
phytools::densityTree(outPhy)
for (locus in 1:Nloci) {
  ntrees <- length (stats$trees [[locus]])
  for (n in 1:ntrees) {
    if (locus == 1 && n == 1) {
      outPhy <- ape::read.tree (text=stats$trees [[locus]][n])
    }
    else {
      outPhy <- ape:::c.multiPhylo (outPhy, ape::read.tree (text=stats$trees [[locus]][n]))
    }
  }
}
par (mfrow=c(1,1))
phytools::densityTree(outPhy)
# No, the plot is not different than expected, yet, with recombination rate, for example, the plot would be expected to change.
model3 <- coala::coal_model(10, 50)
coala::feat_mutation(coala::par_prior("theta", sample.init(100, 1))) + coala::sumstat_nucleotide_div()
stats <- simulate (model3, nsim = 0)
mean_pi <- sapply (stats, function (x) mean (x$pi))
theta <- sapply (stats, function (x) x$pars [["theta"]])