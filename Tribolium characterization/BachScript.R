#   Si le package "ade4" n'est pas installe, la commande "install.packages()" juste dessous permet de le telecharger sur le CRAN web de R et de l'intaller sur l'ordinateur. NB: necessite d'etre connecte a internet

# install.packages(c("ade4"),dependencies=TRUE, repos="http://cran.r-project.org")
#install.packages("data.table")
library(ade4)
library(mclust)
library(data.table)

Tribolium <- read.csv("/Users/Bachir/Se_DT.CSV", header=TRUE, sep=";",row.names = "Individus")

Tribolium1 <- read.csv("/Users/Bachir/Se_DT.CSV", header=TRUE, sep=";")

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
print(groups)


##########++++++++++Transformer le tableau groups en dataframe---###############
####donner des noms spécifiques aux colones########
#
donnees_df<-function(donnees,colname1){
  #donnees1<-setDT(donnees, keep.rownames = TRUE)[]
  #colnames(donnees1)[which(names(donnees1) == "rn")] <- colname1
  return(as.data.frame(donnees))
}

#Tribolium_df=donnees_df(Tribolium1,"Individus")



#+++++++++++Transforme le groupe obtenu par cutree en datframe avec des noms colones que tu choisiras
groupe_df<-function(groups,colname1,colname2){
groupe<-setDT(data.frame(groups), keep.rownames = TRUE)[]
colnames(groupe)<-c(colname1,colname2)
return(as.data.frame(groupe))
}
#groups_df=groupe_df(groups,"Individus","Groupe")

###++++++++Concatener les deux tableaux#####################
concat<-function(donnees,groups,colname,order=FALSE){ 
  donnees1<-donnees_df(donnees,colname)
  groups1<-groupe_df(groups,colname,"Groupe")
  Merge<-merge(donnees1,groups1,by=colname)
  if (order==TRUE){ 
    Merge<-Merge[order(Merge$Groupe),]
  }
  print("Population de chaque groupe=Freq:")
  print(as.data.frame(table(Merge["Groupe"])))
  return(Merge)
    
}
#merged_df=concat(Tribolium1,groups,"Individus",order=TRUE)



######################Suprrimer les colonnes inutiles##################
#Merge<-Merge[,-2:-3]



#######Créer un dataframe pour chaque groupe 
####contenant chacun un groupe donné#####
dissocier_gr<-function(merge, nbGroupe){
  rep <- readline("Avez-vous concatené le tableau des groupes avec les données?  O/N: ")
  rep <- as.character(rep)
  if (rep=="O") {
    AllGroups<-split(merge,merge$Groupe) #str(Allgroups)
    Y <- lapply(seq_along(AllGroups), function(x) as.data.frame(AllGroups[[x]])[, 1:length(merge)])
    
    #Créer un nom pour chaque groupe
    for(i in 1:nbGroupe){
      a<-paste('Groupe_', i, sep='')
      #Générer les groupes dans l'environnement de travail
      assign(a,Y[[i]],envir = .GlobalEnv)
      cat(paste('Groupe_', i, sep=''),"est crée", "\n", sep=" ")
    }
  }
  else {
    print ("Utilser la fonction concat(donnees,groups,colname1,order=FALSE) , et réesayer après")

  }
} 
#dissocier_gr(merged_df,4)

###+++++++Coordonnées barycentre Groupe 1+++++++++++++++

#//////Nettoyer les données avant d'utiliser cette fonction
##////////Supprimer les colonnes contenant des chaînes de caractères

Coor.Bary<-function(donnees){
  #Remove all non numerical columns
  #Be careful, it removes factor also
  donnees=donnees[,sapply(donnees, is.numeric)] 
  Coordonnee=c()
  for(i in names(donnees)){
    Coordonnee[i]<-mean(donnees[[i]])
  }
  return(Coordonnee)
}
print(Coor.Bary(Groupe_4))
#/////Pour tester si les listes sont les mêmes (sont égaux)
#identical(1,2)



#//////Distance Euclidienne
distance <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

#///////Distance intra-classe
DistanceIntraClasse<-function(groupe){
  EcartsPointsBarycentre=c()
  B=Coor.Bary(groupe)
  #Remove all non numerical columns
  #Be careful, it removes factor also
  groupe=groupe[,sapply(groupe, is.numeric)] 
  for(i in 1:nrow(groupe)) {
    #On parcours chaque ligne
    point<- groupe[i,]
    #On met toutes les distances dans un vecteur
    EcartsPointsBarycentre[i]<-distance(point,B)
    
  }
  return(EcartsPointsBarycentre)
}
#print(DistanceIntraClasse(Groupe_1))
##+++++++++++++++++++++++++++++++++++++++++++++++
###+++++++++++++INERTIE INTRAAAAA++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++
Inertie_Intra<-function(groupe){
  #Remove all non numerical columns
  #Be careful, it removes factor also
  groupe=groupe[,sapply(groupe, is.numeric)] 
  return(mean(DistanceIntraClasse(groupe)))
}
#Inertie_Intra(Groupe_1)

    
##+++++++++++++++++++++++++++++++++++++++++++++++
###+++++++++++++INERTIE INTERRRRR++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++
Inertie_Inter<-function(donnees,groupe){
  #nbGroupe = nombre de groupe= nombre de cluster
  g=Coor.Bary(groupe)
  G=Coor.Bary(donnees)
  #Remove all non numerical columns
  #Be careful, it removes factor also
  groupe=groupe[,sapply(groupe, is.numeric)] 
  donnees=donnees[,sapply(donnees, is.numeric)] 
  Inertie.inter<-distance(g,G)
  return(Inertie.inter)
}

#Inertie_Inter(Tribolium1,Groupe_1)






