setwd ('C:\\Users\\justj\\Desktop\\Evolution\\Tasks\\Task_08')
library (phytools)
tree <- read.tree("https://jonsmitchell.com/data/anolis.tre")
plot(tree, type="fan")
tree$tip.label

# Question 1: There are 81 tips (82 species) and 161 branch lengths (162 svl points).

tree$edge.length
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors = F, row.names = 1)

# Examining the data.
data
data[,1]
head(data)
summary(data)
data.frame(data)

# Question 2: The data is different lizard species and their snout-to-vent length(s). The data-set contains 82 data points of 1 variable, snout-to-vent length(s).

# The data is read in as a csv file and is part of an assignment vector when used as "data <- read.csv"

# More precisely, the data contains 82 snout-vent length (body size) measurements of 82 lizard species.

svl <- setNames(data$svl, rownames(data))
Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
Ancestors
?fastAnc

# Question 3: The estimated values are stored in svl, which represents "x" in the fastAnc function and are tips on the tree. The CI95 element can compute the 95% confidence interval on ancestral state estimates.

# Question 4: Assumes variances on ancestral state estimates and that of 95% confidence intervals.

par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[tree$tip.label])
nodelabels(pch=16, cex=0.25*tree$ace)

obj <- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7, 0.9))

# New fossil data from "Amber fossils demonstrate deep-time stability of Caribbean lizard communities."
fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))

# Question 5: For each fossil, find the node that corresponds to the MRCA of the pair of tips in the data-frame.
fossilNodes <- c()
nodeN <- c()
for (i in 1:nrow(fossilData)){
  Node <- fastMRCA(tree, fossilData[i, "tip1"], fossilData[i, "tip2"])
  fossilNodes[i] <- fossilData[i, "svl"]
  nodeN[i] <- Node
}

names(fossilNodes) <- nodeN

# Question 6: Use fossils to estimate ancestral states by constraining the estimates at relevant nodes.
Ancestors_withFossils<- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)

# Question 7: How do fossils change estimated ancestral sizes?

# *Trying out some different graphs here!*
fossilgraph <- pbtree(n=26,tip.label=LETTERS)
plotTree(fossilgraph)
x <- fastBM(fossilgraph)
x
phenogram(fossilgraph,x,spread.labels=TRUE,spread.cost=c(1,0))
# Fossils increase estimated ancestral sizes.

# Question 8: Using fitContinuous, fit different models of evolution to the fossil data.

library(geiger)
? fitContinuous
# Time to run the models!
geo=get(data("geospiza"))

tmp=treedata(geo$phy, geo$dat)
phy=tmp$phy
dat=tmp$data

brownFit <-  fitContinuous(phy, dat[,"wingL"], SE=NA, control=list(niter=50), ncores=2)

print(names(brownFit))
print(brownFit)

flik=brownFit$lik
print(argn(flik))

fitGeospiza=function(trait=c("wingL","tarsusL","culmenL","beakD","gonysW")){
  
  trait=match.arg(trait, c("wingL","tarsusL","culmenL","beakD","gonysW"))
  
  models=c("BM", "OU", "EB", "white")
  summaries=c("diffusion", "Ornstein-Uhlenbeck", "early burst", "white noise")
  
  
  aic.se=numeric(length(models))
  lnl.se=numeric(length(models))
  
  for(m in 1:length(models)){
    cat("\n\n\n\n\t*** ", paste(toupper(summaries[m]),": fitting ", sep=""), models[m],
        " with SE *** \n", sep="")
    tmp=fitContinuous(phy,dat[,trait],SE=NA, model=models[m],
                      bounds=list(SE=c(0,0.5)), ncores=2)
    print(tmp)
    aic.se[m]=tmp$opt$aicc
    lnl.se[m]=tmp$opt$lnL
  }
  aic=numeric(length(models))
  lnl=numeric(length(models))
  
  for(m in 1:length(models)){
    cat("\n\n\n\n\t*** ", paste(toupper(summaries[m]),": fitting ", sep=""), models[m],
        " *** \n", sep="")
    tmp=fitContinuous(phy,dat[,trait],SE=0,model=models[m], ncores=2)
    print(tmp)
    aic[m]=tmp$opt$aicc
    lnl[m]=tmp$opt$lnL
  }
  names(aic.se)<-names(lnl.se)<-names(aic)<-names(lnl)<-models
  delta_aic<-function(x) x-x[which(x==min(x))]
  
  daic=delta_aic(aic)
  cat("\n\n\n\t\t\t\t*** MODEL COMPARISON: ",trait," *** \n",sep="")
  cat("\tdelta-AIC values for models assuming no measurement error
    \t\t\t\t zero indicates the best model\n\n")
  print(daic, digits=2)
  
  daic.se=delta_aic(aic.se)
  cat("\n\n\n\n\t\t\t\t*** MODEL COMPARISON: ",trait," ***\n",sep="")
  cat("\t\t   delta-AIC values for models estimating SE
    \t\t\t\t zero indicates the best model\n\n")
  print(daic.se, digits=2)
  cat("\n\n\n")
  
  res_aicc=rbind(aic, aic.se, daic, daic.se)
  rownames(res_aicc)=c("AICc","AICc_SE","dAICc", "dAICc_SE")
  
  return(res_aicc)
}
res=fitGeospiza("wingL")
print(res)

library(geiger)
library(picante)
library(phytools)
anolesvlData <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors = F, row.names = 1)
lambdaModel <- fitContinuous(phy, dat, model = "lambda")
brownianModel <- fitContinuous(phy, dat, model = "BM")
nosigModel <- fitContinuous(phy, dat, model = "white")
lambdaModel$opt$aicc
brownianModel$opt$aicc
lambdaModel$opt$aicc
nosigModel$opt$aicc
brownianModel
lambdaModel
nosigModel

# Question 9: The "White" model is the best-fit model.

?fastAnc
?fitContinuous

# Question 10: The "White" model is different from what fastAnc assumes, in that the White model assumes that data follow
#              a single normal distribution where there is no covariance between species, and is a non-phylogenetic model.
#              Variances are designated under the Brownian motion model. With fastAnc, variances are based on equation 6 of Rohlf,
#              while a 95% confidence interval can be computed on the estimates and is phylogenetic. The model is different
#              from what fastAnc assumes.