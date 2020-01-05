
MMIRIQ <- function(dt, var) {
  
  Var_name <- eval(substitute(var), dt)
  
  
  #Importation des données
  
  library(openxlsx)
  rech<-read.xlsx("Exemple Indice.xlsx", sheet= 4)
  #nech<-subset(ech, !(ech$Var_name<=2.61 | ech$Var_name>=7.37))
  
  
  # Library
  library(classInt)
  
  nclass=classIntervals(nech$var_name, n=10, style='equal', intervalClosure='right')
  Etendue<-max(nech$Var_name)-min(nech$Var_name)
  
  library(modes)
  
  mode(x = nclass$brks)
  nech$nclass=cut(nech$Var_name, nclass$brks, labels=FALSE, include.lowest=TRUE)
  
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
  RIQ<-IQR(nech$Var_name)/median(nech$Var_name)
  
  Result<- c(RIQ,MMI)
  
}



MMIRIQ(nech, Unit.Value)
