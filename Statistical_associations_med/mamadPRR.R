library(gdata)
#Import des données
mes_données <- read.xls('fichierAECUT.xlsx',sheet=1)
# Nos données sont stockées sous forme de dataframe
class(mes_données)
# Visualisation du tableau de données sur R 
str(mes_données)
mes_données
# Accés à la colonne HLGT_DESC : Colonne des plaintes 
mes_données$HLGT_DESC
# Accès éléments dataframe
mes_données[3,"HLGT_DESC"]
# Algorithme PRR 
# Afficher la ligne 22
mes_données[22,c("PROD_NM","BATCH_NBR","HLGT_DESC")]
#Extraire un tableau du médicament "Tadalafil"
mes_données[mes_données$PROD_NM=="FORTEO (COLTER) PEN, 250 MCG/ML" & mes_données$HLGT_DESC=="Administration site reactions",c("PROD_NM","BATCH_NBR","HLGT_DESC")]
nrow(mes_données)

##---------------------Calcul des éléments du PRR-----------------------------

#Calcul de A
A<-function(donnees,batch_nbr,plainte){
  return((donnees[donnees$BATCH_NBR==batch_nbr & donnees$HLGT_DESC==plainte,c("PROD_NM","BATCH_NBR","HLGT_DESC")]))
}

#Calcul de B
B<-function(donnees,batch_nbr,plainte){
  return (donnees[donnees$BATCH_NBR==batch_nbr & donnees$HLGT_DESC !=plainte,c("PROD_NM","BATCH_NBR","HLGT_DESC")])
}

#Calcul de C
C<-function(donnees,batch_nbr,plainte){
  return (donnees[donnees$BATCH_NBR !=batch_nbr & donnees$HLGT_DESC==plainte,c("PROD_NM","BATCH_NBR","HLGT_DESC")])
}

#Calcul de D
D<-function(donnees,batch_nbr,plainte){
  return (donnees[donnees$HLGT_DESC !=plainte & donnees$BATCH_NBR != batch_nbr,c("PROD_NM","BATCH_NBR","HLGT_DESC")])
}

# calcul  de Nj
Nj<-function(donnees,batch_nbr,plainte){
  Nj<-nrow(donnees[donnees$BATCH_NBR==batch_nbr,])
  return (Nj)
}

#Calcul de Ni
Ni<-function(donnees,plainte){
  return((donnees[donnees$HLGT_DESC==plainte,c("PROD_NM","BATCH_NBR","HLGT_DESC")]))
}

PRR<-function(donnees,batch_nbr,plainte){
  a=nrow(A(donnees,batch_nbr,plainte))
  b=nrow(B(donnees,batch_nbr,plainte))
  c=nrow(C(donnees,batch_nbr,plainte))
  d=nrow(D(donnees,batch_nbr,plainte))
  return ((a*(a+b)) / (c*(c+d)) )
}


print(paste("A=",nrow(A(mes_données,"UNKNOWN","Medication errors and other product use errors and issues"))))
print(paste("B=",nrow(B(mes_données,"UNKNOWN","Medication errors and other product use errors and issues"))))
print(paste("C=",nrow(C(mes_données,"UNKNOWN","Medication errors and other product use errors and issues"))))
print(paste("D=",nrow(D1(mes_données,"UNKNOWN","Medication errors and other product use errors and issues"))))
print(paste("Nj=",Nj(mes_données,"UNKNOWN","Medication errors and other product use errors and issues")))
print(paste("Ni=",nrow(Ni(mes_données,"Medication errors and other product use errors and issues"))))
print(paste("PRR=",PRR(mes_données,"UNKNOWN","Medication errors and other product use errors and issues")))


#C1<-function(donnees,batch_nbr,plainte){
# return (nrow(Ni(donnees,plainte))-nrow(A(donnees,batch_nbr,plainte)))
#}
#print(paste("C1=",C1(mes_données,"UNKNOWN","Medication errors and other product use errors and issues")))

