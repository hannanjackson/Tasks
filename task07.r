text.string <- "(((((((cow, pig), whale), (bat, (lemur, human))), (robin, iguana)), coelacanth), (gold_fish, trout)), shark);"
vert.tree <- read.tree (text=text.string)
plot (vert.tree, edge.width=2)
nodelabels(frame="circle", bg='white', cex=1)
# A shark is more closely related to the gold fish.
vert.tree
str (vert.tree)
# There are not branch lengths in this tree.
tree <- read.tree (text="(((A,B), (C,D)), E);")
plotTree (tree, offset=1)
tiplabels (frame="circle", bg='lightblue', cex=1)
nodelabels (frame="circle", bg='white', cex=1)
tree$tip.label
# The edge matrix gives indices for the starting and ending nodes, which corresponds with species shown.
tree$edge
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par (las=1)
hist (AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0, 50), xlim=c(0, 6))
tipEdges <- which(AnolisTree$edge [,2] <= Ntip (AnolisTree))
Lengths <- AnolisTree$edge.length
names (Lengths) <- AnolisTree$tip.label
names (Lengths) [which(Lengths==min(Lengths))]
plot (AnolisTree, cex=0.25)
Labs <- sapply (AnolisTree$edge.length, round, digits=2)
edgelabels (text=Labs, cex=0.25)
?plot.phylo
# Question 3: Code for a tree with no tip labels.
plot (AnolisTree, cex=0.25, show.tip.label = FALSE)
# Question 4: Code for a tree plotted as a circle.
plotTree (AnolisTree, type="fan", fsize=0.7, lwd=1, ftype="i")
# Question 5: Code for a tree with tips colored red instead of black.
plot (AnolisTree, cex=0.25, tip.color = c("red"))
# Question 6: Find which living, named species has the shortest edge length.
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
n <- length (AnolisTree$tip.label)
ee <- setNames (AnolisTree$edge.length[sapply(1:n,function(x,y)which(y==x), y=AnolisTree$edge[,2])], AnolisTree$tip.label)
AnolisTree <- pbtree (n=10)
AnolisTree$edge.length <- round (AnolisTree$edge.length, 0.5)
n <- length (AnolisTree$tip.label)
ee <- setNames (AnolisTree$edge.length[sapply(1:n,function(x,y)which(y==x), y=AnolisTree$edge[,2])], AnolisTree$tip.label)
ee
plot (AnolisTree, cex=1)
edgelabels(AnolisTree$edge.length)
# Anolis bahorucoensis and Anolis equestris have the shortest edge length.
# Question 7: Drop species with shortest edge length from the tree.
tip <- c("bahorucoensis", "equestris")
pr.tree <- drop.tip(AnolisTree, tip)
plotTree (pr.tree, ftype="i")
# Question 8: Plot the resulting tree of the dropped species.
anolis.pruned <- collapseTree(AnolisTree)
plotTree(anolis.pruned, type="fan", fsize=0.7, lwd=1, ftype="i")
# LTT Plot.
ltt (AnolisTree)
abline (0, 1, lwd=2, col='red', lty=2)
# Question 9: The line increases exponentially, it does not go down.
#             Species arise rapidly but are going towards fixation.
#             The slope is the same. The curve of the lizards shows
#             them heading for fixation.
# Question 10: Calculate the rate at which new species form and disappear in Anolis lizards.
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
fit.bd (AnolisTree, b, d, rho = 0.2)
# B is equal to 0.8031 and D is equal to 0. Species are heading towards fixation.