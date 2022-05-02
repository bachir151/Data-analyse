#setwd("~/Users/Bachir/Documents")
TAB=read.table("/Users/Bachir/Documents/PHYLOCOST.csv", h=T, sep=";", dec=",", na.strings = "NA")
library(ggplot2)
#------------------DATASET MAMMALS-----------------------------------------------
TAB1=subset(TAB, subset = className == "MAMMALIA")
MAM_DAM_QUA=quantile(TAB1$Average.annual.cost_damage, probs = seq(0, 1, 0.25), na.rm = T)
MAM_MAN_QUA=quantile(TAB1$Average.annual.cost_damage, probs = seq(0, 1, 0.25), na.rm = T)
#50% des esp??ces de mammif??res les plus couteuses en termes de dommage
MAM_DAM_QUA_pp50=subset(TAB1, subset = Average.annual.cost_damage >= MAM_DAM_QUA[3])
#50% des esp??ces de mammif??res les plus couteuses en termes de MANAGEMENT
MAM_MAN_QUA_pp50=subset(TAB1, subset = Average.annual.cost_management >= MAM_MAN_QUA[3])
#------------------DATASET BIRDS-----------------------------------------------
TAB2=subset(TAB, subset = className == "AVES")
AVES_DAM_QUA=quantile(TAB2$Average.annual.cost_damage, probs = seq(0, 1, 0.25), na.rm = T)
AVES_MAN_QUA=quantile(TAB2$Average.annual.cost_management, probs = seq(0, 1, 0.25), na.rm = T)
#50% des esp??ces d'oiseaux les plus couteuses en termes de dommage
AVES_DAM_QUA_pp50=subset(TAB2, subset = Average.annual.cost_damage >= AVES_DAM_QUA[3])
#50% des esp??ces d'oiseaux les plus couteuses en termes de MANAGEMENT
AVES_MAN_QUA_pp50=subset(TAB2, subset = Average.annual.cost_management >= AVES_MAN_QUA[3])

#------------BARPLOTS------------------------
#----------Management-----------
##Mammals / Phylogenetic
p1=ggplot(data=MAM_MAN_QUA_pp50, aes(x= reorder(Species, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size=30),
        axis.text.y= element_text(face="bold.italic", size=15),
        axis.text.x = element_text(face = "bold", color = "black",size = 15),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 19, face = "bold"),
        legend.position="bottom")+
  labs(title="A MAMMALS", x="Most costly Species (Management)", y = "Phylogenetic originality scores")+
  geom_hline(aes(yintercept=mean(TAB$oriPtree),linetype="Average originality score"), color="red",size=2)+
  coord_flip()
p1

##AVES / Phylogenetic
p1=ggplot(data=AVES_MAN_QUA_pp50, aes(x= reorder(Species, oriPtree), y=oriPtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size=30),
        axis.text.y= element_text(face="bold.italic", size=15),
        axis.text.x = element_text(face = "bold", color = "black",size = 15),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 19, face = "bold"),
        legend.position="bottom")+
  labs(title="B  BIRDS", x="Most costly Species (Management)", y = "Phylogenetic originality scores")+
  geom_hline(aes(yintercept=mean(TAB$oriPtree),linetype="Average originality score"), color="red",size=2)+
  coord_flip()
p1

##Mammals / Functional

p1=ggplot(data=MAM_MAN_QUA_pp50, aes(x= reorder(Species, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size=30),
        axis.text.y= element_text(face="bold.italic", size=15),
        axis.text.x = element_text(face = "bold", color = "black",size = 13),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 19, face = "bold"),
        legend.position="bottom")+
  labs(title="C MAMMALS", x="Most costly Species (Management)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=mean(TAB$meanoriFtree),linetype="Average originality score"), color="red",size=2)+
  coord_flip()
p1

##AVES / Functional
p1=ggplot(data=AVES_MAN_QUA_pp50, aes(x= reorder(Species, meanoriFtree), y=meanoriFtree))+
  geom_bar(stat="identity", fill="gray30") +
  theme(plot.title = element_text(color="black", size=30),
        axis.text.y= element_text(face="bold.italic", size=15),
        axis.text.x = element_text(face = "bold", color = "black",size = 13),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 19, face = "bold"),
        legend.position="bottom")+
  labs(title="D BIRDS", x="Most costly Species (Management)", y = "Functional originality scores")+
  geom_hline(aes(yintercept=mean(TAB$meanoriFtree),linetype="Average originality score"), color="red",size=2)+
  coord_flip()
p1


#-----Statut de menace ------
##-----Mammals------
#-----Statut de menace------
##-----Mammals------
p1=ggplot(data=MAM_DAM_QUA_pp50, aes(x=Species, y=Average.annual.cost_damage, fill=redlistCategory_version_2020.2)) + ggtitle("E  MAMMALS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("orange2", "green4")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 25),   legend.text = element_text(color = "black", size = 20)) + 
  theme(plot.title = element_text(color="black", size=30))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size=20))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Damage") + theme(axis.title.x = element_text(size = 30, face = "bold"),
                                                                                      axis.title.y = element_text(size = 30, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 20))
p1_4 + scale_x_discrete(limits=c("Paguma larvata","Rattus norvegicus","Mus musculus","Ondatra zibethicus","Vulpes vulpes","Canis lupus","Sus scrofa","Oryctolagus cuniculus","Rattus rattus"))

##-----AVES------
p1=ggplot(data=AVES_DAM_QUA_pp50, aes(x=Species, y=Average.annual.cost_damage, fill=redlistCategory_version_2020.2)) + ggtitle("F  BIRDS") +
  geom_bar(stat="identity") + theme_minimal() + scale_fill_manual(values=c("green4")) + labs(fill = "Red List Category") + theme(legend.title = element_text(color = "black", size = 25),   legend.text = element_text(color = "black", size = 20)) + 
  theme(plot.title = element_text(color="black", size=30))
p1
p1_2=p1 + coord_flip() + theme(axis.text.y= element_text(face="bold.italic", size=20))
p1_3=p1_2 + labs(x = "Most costly Species", y = "Average annual cost Damage") + theme(axis.title.x = element_text(size = 30, face = "bold"),
                                                                                      axis.title.y = element_text(size = 30, face = "bold"))
p1_4=p1_3 + theme(axis.text.x = element_text(face = "bold", color = "black", size = 20))
p1_4
p1_4 + scale_x_discrete(limits=c("Phasianus colchicus","Branta canadensis","Psittacula krameri","Ploceus melanocephalus","Columba livia","Sturnus vulgaris"))

#Damage / Functional-----------
##Mammals
library(dplyr)
MENACE_DOM=filter(TAB1, redlistCategory_version_2020.2 != "Least Concern")




