#vider la m�moire
rm(list=ls())

######################################
# CHARGEMENT DES DONNEES - DESCRIPTION
######################################

#modifier le r�pertoire par d�faut
setwd("...")

#charger les donn�es - attention aux options
fromage <- read.table(file="fromage.txt",header=T,row.names=1,sep="\t",dec=".")

#afficher les premi�res lignes
print(head(fromage))

#stat. descriptives
print(summary(fromage))

#graphique - croisement deux � deux
pairs(fromage)

######################################
# CAH
######################################

#centrage r�duction des donn�es
fromage.cr <- scale(fromage,center=T,scale=T)

#distance entre individus
d.fromage <- dist(fromage.cr)

#CAH - crit�re de Ward
cah.ward <- hclust(d.fromage,method="ward.D2")

#affichage dendrogramme
plot(cah.ward)

#dendrogramme avec mat�rialisation des groupes
rect.hclust(cah.ward,k=4)

#d�coupage en 4 groupes
groupes.cah <- cutree(cah.ward,k=4)

#liste des groupes
print(sort(groupes.cah))

######################################
# K-MEANS
######################################

#k-means avec les donn�es centr�es et r�duites
#center = 4 - nombre de groupes demand�s
#nstart = 5 - nombre d'essais avec diff�rents individus de d�part
groupes.kmeans <- kmeans(fromage.cr,centers=4,nstart=5)

#affichage des r�sultats
print(groupes.kmeans)

#correspondance avec les groupes de la CAH
print(table(groupes.cah,groupes.kmeans$cluster))

######################################
# K-MEANS - DETECTION NB. DE GROUPES
######################################

#(1)�valuer la proportion d'inertie expliqu�e
inertie.expl <- rep(0,times=10)
for (k in 2:10){
  clus <- kmeans(fromage.cr,centers=k,nstart=5)  
  inertie.expl[k] <- clus$betweenss/clus$totss
}

#graphique
plot(1:10,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliqu�e")

#(2) indice de Calinski Harabasz
#utilisation du package fpc 
library(fpc)

#�valuation des solutions
sol.kmeans <- kmeansruns(fromage.cr,krange=2:10,criterion="ch")

#graphique
plot(1:10,sol.kmeans$crit,type="b",xlab="Nb. de groupes",ylab="Calinski Harabasz")

######################################
# INTERPRETATION - STAT. UNIVARIEES COMPARATIVES
######################################

#fonction de calcul des stats
stat.comp <- function(x,y){
  #nombre de groupes
  K <- length(unique(y))
  #nb. d'observations
  n <- length(x)
  #moyenne globale
  m <- mean(x)
  #variabilit� totale
  TSS <- sum((x-m)^2)
  #effectifs conditionnels
  nk <- table(y)
  #moyennes conditionnelles
  mk <- tapply(x,y,mean)
  #variabilit� expliqu�e
  BSS <- sum(nk * (mk - m)^2)
  #moyennes + prop. variance expliqu�e
  result <- c(mk,100.0*BSS/TSS)
  #nommer les �lements du vecteur
  names(result) <- c(paste("G",1:K),"% epl.")
  #renvoyer le r�sultat
  return(result)
}

#appliquer stat.comp aux variables de
#la base originelle fromage
#et non pas aux variables centr�es et r�duites
print(sapply(fromage,stat.comp,y=groupes.cah))

######################################
# CAH et ACP
######################################


#ACP
acp <- princomp(fromage,cor=T,scores=T)

#screeplot - 2 axes retenus
plot(1:9,acp$sdev^2,type="b",xlab="Nb. de facteurs",ylab="Val. Propres")

#biplot
biplot(acp,cex=0.65)

#positionnement des groupes dans le plan factoriel avec etiquettes des points
plot(acp$scores[,1],acp$scores[,2],type="n",xlim=c(-5,5),ylim=c(-5,5))
text(acp$scores[,1],acp$scores[,2],col=c("red","green","blue","black")[groupes.cah],cex=0.65,labels=rownames(fromage),xlim=c(-5,5),ylim=c(-5,5))

######################################
# RETRAIT DES FROMAGES FRAIS
######################################

#retirer les 4 obs. du groupe 4
fromage.subset <- fromage[groupes.cah!=4,]

#centrage r�duction
fromage.subset.cr <- scale(fromage.subset,center=T,scale=T)

#matrice de distance
d.subset <- dist(fromage.subset.cr)

#cah 2
cah.subset <- hclust(d.subset,method="ward.D2")

#affichage
plot(cah.subset)

#groupes
groupes.subset <- cutree(cah.subset,k=3)

#affichage des groupes
print(sort(groupes.subset))

#acp
acp.subset <- princomp(fromage.subset,cor=T,scores=T)

#screeplot - 2 axes retenus
plot(1:9,acp.subset$sdev^2,type="b")

#biplot
biplot(acp.subset,cex=0.65)

#positionnement des groupes dans le plan factoriel
plot(acp.subset$scores[,1],acp.subset$scores[,2],type="n",xlim=c(-6,6),ylim=c(-6,6))

#etiquettes des points
text(acp.subset$scores[,1],acp.subset$scores[,2],col=c("red","green","blue")[groupes.subset],cex=0.65,labels=rownames(fromage.subset),xlim=c(-6,6),ylim=c(-6,6))

#stat. comparatives
print(sapply(fromage.subset,stat.comp,y=groupes.subset))
