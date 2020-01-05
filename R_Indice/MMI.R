#Importation des données

library(openxlsx)
ech<-read.xlsx("Exemple Indice.xlsx", sheet= 3)
nech<-subset(ech, !(ech$Unit.Value<=2.61 | ech$Unit.Value>=7.37))


# Library
library(classInt)

nclass=classIntervals(nech$Unit.Value, n=10, style='equal', intervalClosure='right')
Etendue<-max(nech$Unit.Value)-min(nech$Unit.Value)

library(modes)

mode(x = nclass$brks)
nech$nclass=cut(nech$Unit.Value, nclass$brks, labels=FALSE, include.lowest=TRUE)

# Comptuation du MMI
##Creation de zz zee1 vee
zee<-table(nech$nclass)
zee1<-as.matrix(zee)
vee<-vector(mode="integer", length=10)

#version  zee avec nivo suivant
for(i in 1:10) {
  
  if (i >= 1 & i<=9) {vee[i]<-zee1[i+1]-zee1[i] }   
  
  else  {vee[i]<- 0-zee1[i]}
  
}

qee<-as.vector(sign(vee))
qee3<-vector(mode="integer", length=10)

for (i in 1:10) {
  if (i==1 & qee[i]==-1) {qee3[i]<-1}
  else if (i==1 & qee[i]==1) {qee3[i]<-0}
  else if (i >1 & i<=9 & qee[i]==1 & qee[i+1]==-1) {qee3[i+1]<-1}
  #else qee3[i]<-0
}

MMIDATA<-data.frame(classe=zee, Multimode=qee3)

MMI<-sum(MMIDATA[which(MMIDATA[,3]==1),2])^2/sum(MMIDATA[which(MMIDATA[,3]==1),2]^2)
RIQ<-IQR(nech$Unit.Value)/median(nech$Unit.Value)
RIQ
MMI