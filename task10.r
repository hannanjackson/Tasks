setwd ('C:\\Users\\justj\\Desktop\\Evolution\\Tasks\\Task_10')
library(diversitree)

# Setting parameters.
transition_0to1 <- 0.1
transition_1to0 <- 0.1
speciation_0 <- 0.2
extinction_0 <- 0.1
speciation_1 <- 0.2
extinction_1 <- 0.1
maxN <- 1e3
maxT <- 50
Pars <- c(speciation_0, speciation_1, extinction_0,extinction_1, transition_0to1, transition_1to0)
simTree <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)
str(simTree)
?tree.bisse
stateTable <- table(simTree$tip.state)
stateTable / sum(stateTable)
# State 0: 0.71 frequency, State 1: 0.29 frequency.

# Time for some fun! Our first scenario!
# Is the frequency of state 1 ever higher than state 0 when the net diversification of state 1 is lower than that of state 0?

# Setting new parameters.
Pars1 <- c(0.1, 0.2, 0.03, 0.03, 0.01, 0.01)
simTree1 <- tree.bisse(Pars1, max.taxa = maxN, max.t = maxT)
str(simTree1)
stateTable1 <- table(simTree1$tip.state)
stateTable1 / sum(stateTable1)
# State 0: 0.09 frequency, State 1: 0.92 frequency.

# Net diversification is low in state 0, more likely to face extinction, while in state 1, net diversification is high, likely to evolve.
# The higher the net diversification, the higher the frequency. State 1 has to have a higher frequency than state 0 if the net diversification is higher in state 1. If state 1 had a lower frequency than state 0, then state 1 would have a lower net diversification.

# Let's make a plot!
set.seed(3)
phy <- tree.bisse(Pars1, max.taxa=30, x0=0)
phy$tip.state
h <- history.from.sim.discrete(phy, 0:1)
plot(h, phy)
head(phy$tip.state)
# Calculating likelihood.
lik <- make.bisse(phy, phy$tip.state)
lik(Pars1)
# -107.27
p <- starting.point.bisse(phy)
fit <- find.mle(lik, p, method="subplex")
logLik(fit)
# -101.88
rbind(real=Pars1,
      estimated=round(coef(fit), 2))
lik.l <- constrain(lik, lambda1 ~ lambda0)
fit.l <- find.mle(lik.l, p[-1], method="subplex")
logLik(fit.l)
# -104.09
# Running an ANOVA.
samples <- anova(fit, equal.lambda=fit.l)
anova(fit, equal.lambda=fit.l)

# Lets investigate a different scenario.
# How much variation is there in the frequency of state 1 when the parameters are the same?

# Setting all parameters to be the same.
Pars2 <- c(0.1, 0.1, 0.01, 0.01, 0.05, 0.05)
simTree2 <- tree.bisse(Pars2, max.taxa = maxN, max.t = maxT)
str(simTree2)
stateTable2 <- table(simTree2$tip.state)
stateTable1 / sum(stateTable1)
# State 0: 0.09 frequency, State 1: 0.92 frequency.

# There is little variation in the frequency of state 1 from when the parameters were different. Yet, there is big variation between state 0 and state 1 when parameters are the same. Net diversification is low in state 0, more likely to face extinction, while in state 1, net diversification is high, likely to evolve.

# Let's plot this!
set.seed(3)
phy1 <- tree.bisse(Pars2, max.taxa=30, x0=0)
phy1$tip.state
h1 <- history.from.sim.discrete(phy1, 0:1)
plot(h1, phy1)
head(phy$tip.state)
# Calculating likelihood.
lik1 <- make.bisse(phy1, phy1$tip.state)
lik1(Pars2)
# -111.19
p1 <- starting.point.bisse(phy1)
fit1 <- find.mle(lik1, p1, method="subplex")
logLik(fit1)
# -110.94
rbind(real=Pars2,
      estimated=round(coef(fit1), 2))
lik.l1 <- constrain(lik1, lambda1 ~ lambda0)
fit.l1 <- find.mle(lik.l1, p1[-1], method="subplex")
logLik(fit.l1)
# -110.94
# No ANOVA for this scenario.

# Let's do another scenario.
# Is the frequency of state 1 ever zero when the transition rates are both non-zero?

# Setting up some parameters.
transitionrate0to1 <- 1.0
transitionrate1to0 <- 1.0
speciation0 <- 0.2
extinction0 <- 0.1
speciation1 <- 0.2
extinction1 <- 0.1
maxN <- 1e3
maxT <- 50
Pars3 <- c(speciation0, speciation1, extinction0,extinction1, transitionrate0to1, transitionrate1to0)
simTree3 <- tree.bisse(Pars3, max.taxa = maxN, max.t = maxT)
str(simTree3)
stateTable3 <- table(simTree3$tip.state)
stateTable2 / sum(stateTable2)
# State 0: 0.57 frequency, State 1: 0.43 frequency.

# Now this is interesting! The frequency of state 1 is not zero but it is lower than state 0 when the transition rates are both non-zero.

# Let's make a plot!
set.seed(3)
phy2 <- tree.bisse(Pars3, max.taxa=30, x0=0)
phy2$tip.state
h2 <- history.from.sim.discrete(phy2, 0:1)
plot(h2, phy2)
head(phy2$tip.state)
# Calculating likelihood.
lik2 <- make.bisse(phy2, phy2$tip.state)
lik2(Pars3)
# -93.54
p2 <- starting.point.bisse(phy2)
fit2 <- find.mle(lik2, p2, method="subplex")
logLik(fit2)
# -89.31
rbind(real=Pars3,
      estimated=round(coef(fit2), 2))
lik.l2 <- constrain(lik2, lambda1 ~ lambda0)
fit.l2 <- find.mle(lik.l2, p2[-1], method="subplex")
logLik(fit.l2)
# -89.56
# Running an ANOVA.
samples2 <- anova(fit2, equal.lambda=fit.l2)
anova(fit2, equal.lambda=fit.l2)

# Onto our final scenario!
# Let's see if the frequency of state 1 is higher when the transition rates are both zero. This is not a likely phenomenon, since this would apply no "transition" of a trait through a lineage(s), yet I still want to see how this pans out when graphically represented!

# Setting up some parameters.
transitionrate.0to1 <- 0
transitionrate.1to0 <- 0
speciation.0 <- 0.5
extinction.0 <- 0.2
speciation.1 <- 0.5
extinction.1 <- 0.2
maxN <- 1e3
maxT <- 50
Pars4 <- c(speciation.0, speciation.1, extinction.0,extinction.1, transitionrate.0to1, transitionrate.1to0)
simTree4 <- tree.bisse(Pars4, max.taxa = maxN, max.t = maxT)
str(simTree4)
stateTable4 <- table(simTree4$tip.state)
stateTable3 / sum(stateTable3)
# State 0: 0.51 frequency, State 1: 0.47 frequency.

# This is rather surprising! The frequency of state 1 is lower than that of state 0 when the transition rates are both zero, although the frequency of state 1 did not change much from when both transition rates were non-zero.

# Let's make our final plot!
set.seed(3)
phy3 <- tree.bisse(Pars4, max.taxa=30, x0=0)
phy3$tip.state
h3 <- history.from.sim.discrete(phy3, 0:1)
plot(h3, phy3)
head(phy3$tip.state)

# Likelihood and ANOVA are not represented for this example due to the transition rates being zero.

# Our scenarios and plot simulations are now complete!