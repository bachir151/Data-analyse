#   Si le package "ade4" n'est pas installe, la commande "install.packages()" juste dessous permet de le telecharger sur le CRAN web de R et de l'intaller sur l'ordinateur. NB: necessite d'etre connecte a internet

# install.packages(c("ade4"),dependencies=TRUE, repos="http://cran.r-project.org")
library(ade4)
library(mclust)

Tribolium <- read.csv("/Users/macretina/Documents/These/R/Donnees_transformees/Ga_DT.CSV", header=TRUE, sep=";", row.names ="Individus")

# Ascendent Hierarchical Clustering
# la matrice de distance, calculee avec la fonction "dist()"
d <- dist(Tribolium[,-1], method = "euclidean")

# La fonction "hclust()" fait les calculs relative a la CAH utilisant ici la methode de ward
# les resulats de la CAH sont stockes dans l'objet nommee ci-dessous "res_classif"
res_classif <- hclust(d, method="ward.D2")

# Affichage du dendogram
# l'option "cex" permet de reduire les labels en bas du dendogramme, ici de moitie (de 50% = 0.5)
plot(res_classif, cex=0.5)

# Si on decide de couper l'arbre en 4 clusters par exemple, et afficher l'effectif dans chaque classe
groups <- factor(cutree(res_classif, k=4))
table(groups)

# draw dendogram with red borders around the 4 clusters
plot(res_classif, cex=0.5)
rect.hclust(res_classif, k=4, border="red", cluster = groups)


# Les inerties peuvent etre calculees en utilisant la fonction between-class analysis "bca()" du package "ade4"
# Il faut faire une ACP avec les memes donnees utilisees pour faire la classification hierarchique
res_acp <- dudi.pca(Tribolium[,-1], scale=FALSE, scannf=FALSE, nf=ncol(Tribolium[,-1]))

# Puis une analyse "between class" incluant les classes obtenues par la classification, ici sauvees sous l'objet nomme "groups"
betw <- bca(res_acp, groups, scannf=F, nf=2)

# L'inertie inter est obtenue avec "betw$ratio". 
iner.inter <- betw$ratio
iner.inter

# L'inertie intra est obtenue avec 1-iner.inter. 
iner.intra <- 1 - iner.inter
iner.intra

# Pour installer JLutils
library(devtools)
install_github("larmarange/JLutils")

# Compl??ment du net

library(ade4)
library(mclust)
library(fastcluster)
library(RColorBrewer)
library(ggplot2)
library(devtools)
library(JLutils)
library(scales)
library(stats)
Tribolium <- read.csv("/Users/macretina/Documents/These/R/Donnees_transformees/Se_DT.CSV", header=TRUE, sep=";", row.names ="Individus")
d <- dist(Tribolium[,-1], method = "euclidean")
res_classif <- hclust(d, method="ward.D2")
plot(res_classif, cex=0.5)
# Pour determiner le nombre de classes a retenir 2
inertie <- sort(res_classif$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", 
     ylab = "Inertie")

# Pour determiner le nombre de classes a retenir 1
par(mfrow = c(1, 2))
library(JLutils)
best.cutree(res_classif)
best.cutree(res_classif, min = 2)
best.cutree(res_classif, min = 2, graph = TRUE, xlab = "Nombre de classes", 
            ylab = "Inertie relative")

# Pour colorier les sauts d'inertie
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", 
     ylab = "Inertie")
points(c(2, 7), inertie[c(2, 7)], col = c("green3", 
                                                  "red3", "blue3", "yellow3"), cex = 2, lwd = 3)


-------------------------------------------------------------------------------------------------

# Pour colorier le dendo selon les classes
A2Rplot(res_classif, k = 3, boxes = TRUE, col.up = "gray20", 
        col.down = brewer.pal(5, "Dark2"), show.labels = TRUE)

# Pour representer deux graphiques dans une seule page
par(mfrow = c(1, 2))

library(stats)
typo <- cutree(res_classif, 3)
freq(typo)
typo <- cutree(res_classif, 3)
freq(typo)

par(mfrow = c(1, 2))
library(RColorBrewer)
s.class(Tribolium[,-1]$li, as.factor(typo), col = brewer.pal(3, "Set1"), sub = "Axes 1 et 2")
s.class(Tribolium[,-1], as.factor(typo), 2, 3, col = brewer.pal(3, "Set1"), sub = "Axes 1 et 3")



groups <- factor(cutree(res_classif, k=4))
table(groups)
plot(res_classif, cex=0.5)
rect.hclust(res_classif, k=4, border="red", cluster = groups)
res_acp <- dudi.pca(Tribolium[,-1], scale=FALSE, scannf=FALSE, nf=ncol(Tribolium[,-1]))
getwd()
betw
wca(class)
res_acp