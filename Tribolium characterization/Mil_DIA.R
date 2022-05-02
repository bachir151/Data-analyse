library(ade4)
library(rgl)
library(ggplot2)
library(grid)
library(FactoMineR)
library(devtools)
library(factoextra)
library(mclust)
Tribolium <- read.csv("/Users/Bachir/Documents/These/R/Mil_DT.CSV", header=TRUE, sep=";")
names(Tribolium)
pca=PCA(Tribolium[,-c(1,2)])
fviz_pca_ind(pca, label="ind",habillage=Tribolium[,2] , addEllipses=TRUE, ellipse.level=0.95) + scale_shape_manual(values=seq(0:12))
fviz_pca_ind(pca, label="none",habillage=Tribolium[,2] , addEllipses=TRUE, ellipse.level=0.95)

hist(Tribolium$LmA)
tapply(Tribolium$Lp, Tribolium$Race,mean)
tapply(Tribolium$Lp, Tribolium$Race,min)
tapply(Tribolium$Lp, Tribolium$Race,max)

#CContributions des individus
names(pca)
names(pca$ind)
pca$ind$contrib
sort(pca$ind$contrib[,2])
sort(pca$ind$contrib[,2])
sort(pca$ind$contrib[,3])

#Contribution des variables
names(pca)
names(pca$var)
pca$var$contrib
sort(pca$var$contrib[,1])
sort(pca$var$contrib[,2])
sort(pca$var$contrib[,3])

#Test de corr??lation lin??aire
pairs(Tribolium,main="var")
cor(Tribolium[, 4:19])
s.corcircle(Tribolium[, 4:19]$var)

#Biplot
library(stats)
library(ade4)
library(rgl)
library(ggplot2)
library(grid)
library(devtools)
library(FactoMineR)
library(factoextra)
Tribolium <- read.csv("/Users/Bachir/Documents/These/R/Mil_DT.CSV", header=TRUE, sep=";")
names(Tribolium)
res.pca <- prcomp(Tribolium[, -c(1,2)],  scale = TRUE)
pca=PCA(Tribolium[,-c(1,2)])
fviz_pca_biplot(res.pca, label="var", habillage=Tribolium[,2], addEllipses=TRUE, ellipse.level=0.95, labelsize = 5, pointsize = 2, col.var = "black") + scale_shape_manual(values=seq(0:12))
lifviz_pca_biplot(res.pca, label="var", habillage=Tribolium[,3], addEllipses=FALSE, ellipse.level=0.95, labelsize = 5, pointsize = 2, col.var = "yellow")

#30 individus et 5 variables les plus contributif
fviz_pca_biplot(res.pca, geom = c("arrow", "text"), label="all", select.ind = list(contrib = 30), select.var = list(contrib =5))

#PCA Pour plus de 6 populations
fviz_pca_biplot(res.pca, label="var", habillage=Tribolium[,2], addEllipses=TRUE, ellipse.level=0.95, labelsize = 5, pointsize = 2, col.var = "black") + scale_shape_manual(values=seq(0:12))

# Controler la couleur des variables selon la contribution
fviz_pca_var(res.pca, geom = c("arrow", "text"), col.var="contrib") + scale_color_gradient2(low="blue", mid="red", high="white", midpoint=96, space ="Lab") + theme_minimal()

# Colorer les individus en fonction de la contribution
fviz_pca_ind(res.pca, col.ind="contrib") + scale_color_gradient2(low="white", mid="blue", high="red", midpoint=4, space ="Lab")

# Selectionner le top 8 selon la contribution
fviz_pca_var(res.pca, geom = c("arrow", "text"), select.var = list(contrib = 5))

# Utiliser uniquement des fleches et/ou des textes
fviz_pca_var(res.pca, geom = c("text"))
fviz_pca_var(res.pca, geom = c("arrow", "text"))

# Changer la couleur et le theme du cercle des variables
fviz_pca_var(res.pca, col.var="steelblue") + theme_minimal()

# S??lectionner le top 3 des variables selon la contribution
fviz_pca_var(res.pca, select.var = list(contrib = 3))

# Selectionner les variables par noms
fviz_pca_var(res.pca, select.var= list(name = c("Lc3", "Lta3")))

# Annoter uniquement les variables
fviz_pca_biplot(res.pca, label ="var")

# Annoter uniquement les individus
fviz_pca_biplot(res.pca, geom = c("arrow", "text"), label ="ind")
                                                                                                                                                                        
# Cacher les individus
fviz_pca_biplot(res.pca, invisible ="ind")





